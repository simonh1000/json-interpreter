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
    between (char '(') (char ')') <|
        -- map String.fromList (many <| noneOf [')'])
        ( (many (noneOf ['(', ')']) *> (rec <| \() -> somethingInBrackets) )
          -- second part fails when matching a ')', causing second branch
          -- on a '(', we get the bracketed content
          `or`
          map String.fromList (many <| noneOf [')'])
        )
        -- take until ( or ) andThen
        --   - case of
        --       ( -> somethingInBrackets and keep taking again
        --       ) -> end

transformFunc = word `or` somethingInBrackets
-- LISTOF

listOf : Parser a -> Parser (List a)
listOf dec =
    (char '[') *> (many <| possibleSpacing *> dec <* possibleSpacing <* (oneOf [',', ']']))
