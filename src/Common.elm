module Common (..) where

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num as Num
import Combine.Infix exposing (..)
import String
import Json.Decode as Json exposing (Decoder, Value, (:=))

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
word : Parser String
word =
    regex "[a-zA-Z_\\d\\.\\$]+"
    -- map String.fromList <| many1 (noneOf <| spaceChars ++ delimiters)

stringLiteral : Parser String
stringLiteral =
    between (char '"') (char '"')
        word
        -- <| Combine.map String.fromList <| many (noneOf ['"'])

wordOrBrackets : Parser String
wordOrBrackets =
    word `or`
        map String.fromList (between openBrackets closeBrackets (many <| noneOf [')']))

-- BRACKETS
-- parse brackets first, accept some space just inside brackets
bracketed : Parser a -> Parser a
bracketed parser =
    between openBrackets closeBrackets parser `or` parser

openBrackets : Parser ()
openBrackets =
    (char '(') *> possibleSpacing

closeBrackets : Parser ()
closeBrackets =
    possibleSpacing <* (char ')')

--
anonFunc : Parser String
anonFunc =
    -- regex "\\(\\[^\\)]*\\)"
    map String.fromList <| between (char '(') (char ')') (many1 <| noneOf [')'])
    -- between openBrackets closeBrackets (char '\' )

listOf : Parser a -> Parser (List a)
listOf dec =
    (char '[') *> (many <| possibleSpacing *> dec <* possibleSpacing <* (oneOf [',', ']']))
