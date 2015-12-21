{-| Need to capture
words:
    - primitives,
    - parameters   (assumed to be in function def because must compile)
    - functions
        - applied: including Xyz.fnc and Constructors
        - defined: word word = ...
"word": String
-123: digit

at [word, "word"]
object2 (word word) word (word ("word" or digit))
tuple2 (,) string string

-}
module Common (..) where

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num as Num
import Combine.Infix exposing (..)
import String
import Json.Decode as Json exposing (Decoder, Value, (:=))

import AST exposing (..)

-- list delimiters
delimiters = [',', ']', ')', '"']

-- SPACING
spaceChars = [' ', '\t']

spacingChars = '\n' :: spaceChars

wordDelimiters =
    spacingChars ++ delimiters

endWord =
    end `or`
    (skip <| oneOf <| spacingChars ++ delimiters)

spaces =
    skipMany1 (oneOf spaceChars)

possSpaces =
    skipMany <|
        oneOf spaceChars

spacing : Parser ()
spacing =
    skipMany1 <|
        oneOf spacingChars

possibleSpacing : Parser ()
possibleSpacing =
    skipMany <|
        oneOf spacingChars

-- WORDS
-- word : Parser String
-- word =
--     regex "[a-zA-Z_\\d\\.\\$]+"

-- 1 or more letter, numbers, . $
-- must exclude comma so that "float," is properly handled
-- must include comma to capture e.g. (,,,) in objext3 (,,,)
word : Parser String
word =
    regex "[a-zA-Z\\d\\_\\.\\$]+"

number : Parser String
number =
    regex "[-]?[0-9]+"

-- stringLiteral : Parser String
-- stringLiteral =
--     between (char '"') (char '"')
--         word

quotedWord : Parser String
quotedWord =
    between (char '"') (char '"') word

sentence : Parser String
sentence =
    map String.fromList <| between (char '"') (char '"') (many <| noneOf ['"'])

-- stringLiteral : Parser Command
-- stringLiteral =
--     quotedWord
--     -- between (char '"') (char '"') word
--     `andThen` (succeed << Str)

-- wordOrBrackets : Parser String
-- wordOrBrackets =
--     word `or`
--         map String.fromList (between openBrackets closeBrackets (many <| noneOf [')']))

-- BRACKETS
-- parse brackets first, accept some space just inside brackets
bracketed : Parser a -> Parser a
bracketed parser =
    choice
        [ delimiter parser
        , between openBrackets closeBrackets parser
        , parser
        ]

delimiter : Parser a -> Parser a
delimiter parser =
    string "<|" *> spacing *> parser

openBrackets : Parser ()
openBrackets =
    (char '(') *> possibleSpacing

closeBrackets : Parser ()
closeBrackets =
    possibleSpacing <* (char ')')

-- used to transform output of decoder
-- irrelevant for this project
transformFunc : Parser String
transformFunc =
    -- between openBrackets closeBrackets <|
    bracketed <|
        word <* many (spaces <* (word `or` quotedWord))

-- NEEDS more work
anonFunc : Parser String
anonFunc =
    let
        pre = string "(" <* possSpaces <* string "\\"
    -- regex "\\(\\[^\\)]*\\)"
    in
    map String.fromList <| between pre (char ')') (many1 <| noneOf [')'])
    -- between openBrackets closeBrackets (char '\' )

somethingInBrackets : Parser String
somethingInBrackets =
    map String.fromList <| between (char '(') (char ')') (many <| noneOf [')'])

-- LISTOF

listOf : Parser a -> Parser (List a)
listOf dec =
    (char '[') *> (many <| possibleSpacing *> dec <* possibleSpacing <* (oneOf [',', ']']))
