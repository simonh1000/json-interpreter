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
word : Parser String
word =
    regex "[a-zA-Z_\\d\\.\\$]+"

-- variable/  function names
-- used by pSucceed
var : Parser Command
var =
    word
    `andThen` \w -> succeed <| Call w []

-- stringLiteral : Parser String
-- stringLiteral =
--     between (char '"') (char '"')
--         word

quotedWord : Parser String
quotedWord =
    between (char '"') (char '"') word

stringLiteral : Parser Command
stringLiteral =
    between (char '"') (char '"') word
    `andThen` (succeed << Str)

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

listOf : Parser a -> Parser (List a)
listOf dec =
    (char '[') *> (many <| possibleSpacing *> dec <* possibleSpacing <* (oneOf [',', ']']))
