module Common2 (..) where
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

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num as Num
import Combine.Infix exposing (..)
import String
import Json.Decode as Json exposing (Decoder, Value, (:=))

import AST exposing (..)

-- SPACING
spaceChars = [' ', '\t', '\n']

spacing : Parser ()
spacing =
    skipMany1 <| oneOf spaceChars

possibleSpacing : Parser ()
possibleSpacing =
    skipMany <| oneOf spaceChars

-- WORDS
-- other word delimiters
wordDelimiters =
    [',', ']', ')', '"'] ++ spaceChars

-- 1 or more letter, numbers, . $
-- must exclude comma so that "float," is properly handled
-- must include comma to capture e.g. (,,,) in objext3 (,,,)
word : Parser String
word =
    regex "[a-zA-Z\\d\\_\\.\\$]+"
    -- map String.fromList <| many (noneOf wordDelimiters)

string_ : Parser String
string_ =
    -- map String.fromList <| between (char '"') (char '"') (many <| noneOf ['"'])
    char '"' *> while ((/=) '"') <* char '"'

-- NUMBERS
int_ : Parser String
int_ =
    regex "[-]?[0-9]+"

-- C O D E    D E L I M I T E R S
funcApp : Parser a -> Parser a
funcApp parser =
    string "<|" *> spacing *> parser

openBrackets : Parser ()
openBrackets =
    (char '(') *> possibleSpacing

closeBrackets : Parser ()
closeBrackets =
    possibleSpacing <* (char ')')

-- SUB - COMPONENTS
-- parse brackets first, accept some space just inside brackets
bracketed : Parser a -> Parser a
bracketed parser =
    choice
        [ funcApp parser
        , between openBrackets closeBrackets parser
        , parser
        ]

-- should count balanced parens
somethingInBrackets : Parser String
somethingInBrackets =
    map String.fromList <| between (char '(') (char ')') (many <| noneOf [')'])

-- LISTOF

listOf : Parser a -> Parser (List a)
listOf dec =
    (char '[') *> (many <| possibleSpacing *> dec <* possibleSpacing <* (oneOf [',', ']']))

--
--
-- endWord =
--     end `or`
--     (skip <| oneOf <| spacingChars ++ delimiters)
--
-- -- WORDS
-- -- word : Parser String
-- -- word =
-- --     regex "[a-zA-Z_\\d\\.\\$]+"
--
-- -- stringLiteral : Parser String
-- -- stringLiteral =
-- --     between (char '"') (char '"')
-- --         word
--
-- quotedWord : Parser String
-- quotedWord =
--     between (char '"') (char '"') word
--
-- sentence : Parser String
-- sentence =
--     map String.fromList <| between (char '"') (char '"') (many <| noneOf ['"'])
--
-- -- stringLiteral : Parser Command
-- -- stringLiteral =
-- --     quotedWord
-- --     -- between (char '"') (char '"') word
-- --     `andThen` (succeed << Str)
--
-- -- wordOrBrackets : Parser String
-- -- wordOrBrackets =
-- --     word `or`
-- --         map String.fromList (between openBrackets closeBrackets (many <| noneOf [')']))
--
-- -- used to transform output of decoder
-- -- irrelevant for this project
-- transformFunc : Parser String
-- transformFunc =
--     -- between openBrackets closeBrackets <|
--     bracketed <|
--         word <* many (spaces <* (word `or` quotedWord))
--
-- -- NEEDS more work
-- anonFunc : Parser String
-- anonFunc =
--     let
--         pre = string "(" <* possSpaces <* string "\\"
--     -- regex "\\(\\[^\\)]*\\)"
--     in
--     map String.fromList <| between pre (char ')') (many1 <| noneOf [')'])
--     -- between openBrackets closeBrackets (char '\' )
--
-- -- spaces =
-- --     skipMany1 (oneOf spaceChars)
-- --
-- -- possSpaces =
-- --     skipMany <|
-- --         oneOf spaceChars
-- --
