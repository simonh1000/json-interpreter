module Parser (..) where

import Combine as C exposing (..)
import Combine.Char exposing (..)
import Combine.Num as Num
import Combine.Infix exposing (..)
import String
import Result exposing (Result)

import Common exposing (..)
import AST exposing (Command(..))

{- Success is
    - if one result is a Func, then all must be; or
    - if one result is not Func, then must be single response
-}

parseString : String -> Result.Result String (List Command)
parseString s =
    case C.parse (many eol *> (oneOrMoreProcs `or` singleItem) <* possibleSpacing <* end) s of
        (Done lst, cntx) ->
            Result.Ok lst
        (Fail errs, cntx) ->
            Result.Err (toString errs ++ "Parsing error beginning at: " ++ toString cntx)  -- cntx.input

oneOrMoreProcs : Parser (List Command)
oneOrMoreProcs =
    pProc `andThen`
        \p1 -> many ((many1 eol) *> pProc)
        `andThen` \ps -> succeed (p1 :: ps)
    -- many1 <|
    --     (pProc <* (many1 spacing))

singleItem : Parser (List Command)
singleItem =
    pCommand <* possibleSpacing <* end
    `andThen` \item -> succeed [ Proc "Default" [] item ]

-- PROCS / FUNCTIONS

-- includes spaces at end of definition
pProc : Parser Command
pProc =
    parseFunctionSignature *> many1 (word <* spaces)
        `andThen` \ps -> (char '=') *> spacing *> pCommand <* possSpaces
        `andThen` \decoder ->
            case ps of
                [] -> fail ["Failure parsing Proc"]
                -- [x] -> succeed <| Proc x [] decoder
                -- (x :: xs) -> fail ["Can't handle functions with parameters yet"]
                (x :: xs) -> succeed <| Proc x xs decoder

-- if there is signature, ignore it
parseFunctionSignature : Parser ()
parseFunctionSignature =
    optional
        () <|
        skip <| var <* between possibleSpacing spacing (char ':') <* many (noneOf ['\n']) *> spacing

-- C O M M A N D S

pCommand : Parser Command
pCommand =
    bracketed <|
        rec <| \() ->
            choice
                [ pProblem
                -- , pAndThen      -- needs to precede primitives
                , pString
                , pInt
                , pFlt
                , pBln
                , pKV
                , pAt
                , pObject
                , pList
                , pArr
                , pTuple
                , pSucceed
                , pMap
                , pOneOf
                , pMaybe
                , pCustom
                , pKeyValuePairs
                , pStr
                , pCall
                ]
-- 0 NO <|
pProblem : Parser Command
pProblem =
    choice [ string "<|", string "|>" ]
    `andThen` \_ -> fail ["I can't yet handle <| or |>"]

pStr : Parser Command
pStr =
    stringLiteral `andThen` succeed

-- 1 PRMITIVES
pString : Parser Command
pString =
    string "string" `andThen` \_ -> C.succeed Strng

pInt : Parser Command
pInt =
    string "int" `andThen` \_ -> C.succeed Itg

pFlt : Parser Command
pFlt =
    string "float" `andThen` \_ -> C.succeed Flt

pBln : Parser Command
pBln =
    string "boolean" `andThen` \_ -> C.succeed Bln

--null

-- 5 KEY : VALUE
-- does not capture when k is a function applied to something
pKV : Parser Command
pKV =
    C.map KV ( (stringLiteral `or` var) <* between spacing spacing (string ":="))
    `C.andMap` pCommand

-- -- 6 AT
pAt : Parser Command
pAt =
    string "at" *> spacing *> listOf (stringLiteral `or` var)
    `andThen` \lst -> spacing *> pCommand
    `andThen` \dec -> succeed (At lst dec)
--
-- -- 7 OBJECT
pObject : Parser Command
pObject =
    string "object" *> Num.digit
    `andThen` \n ->
        spacing *> (anonFunc `or` transformFunc) *> count n (spacing *> bracketed (possibleSpacing *> pCommand))
        `andThen` (succeed << Object)
-- -- 8 LIST / ARRAY
pList : Parser Command
pList =
    string "list" *> spacing *> bracketed pCommand
    `andThen` \dec -> succeed <| List dec

pArr : Parser Command
pArr =
    string "array" *> spacing *> bracketed pCommand
    `andThen` (succeed << Arr)

-- 10 TUPLE
pTuple : Parser Command
pTuple =
    string "tuple" *> Num.digit
    `andThen` \n -> spacing *> (word `or` anonFunc) *> count n (spacing *> pCommand)
    `andThen` (succeed << Tuple)

-- 11 ANDTHEN
-- pAndThen : Parser Command
-- pAndThen =
--     -- (....) or var
--     -- bracketed (many1 <| noneOf [')'])
--     varOrBrackets
--         <* spacing <* string "`andThen`" <* spacing <* (var `or` anonFunc)
--     `andThen`
--         \decStr ->
--             case parse pCommand decStr of
--                 (Done com, _) -> (succeed << AndThen) com
--                 (Fail m, _) -> (succeed << AndThen) (Error <| toString m)

-- 12 SUCCEED
-- succeed : a -> Decoder a
-- for some reason this interferes with Call
pSucceed : Parser Command
pSucceed =
    string "succeed" *> spacing *> (stringLiteral `or` var)
    `andThen` (succeed << Succeed)

-- 13 MAP
pMap : Parser Command
pMap =
    -- string "map" *> spacing *> (var `or` anonFunc) *> spacing *> pCommand
    string "map" *> spacing *> (transformFunc `or` anonFunc) *> spacing *> pCommand
    `andThen` (succeed << Map)

-- 14 ONEOF
pOneOf : Parser Command
pOneOf =
    string "oneOf" *> spacing *> listOf pCommand
    `andThen` (succeed << OneOf)

-- 15 MAYBE
pMaybe : Parser Command
pMaybe =
    string "maybe" <* spacing <* pCommand
    `andThen` \_ -> succeed MaybeCommand

-- 16 CUSTOM
-- customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
pCustom : Parser Command
pCustom =
    -- string "customDecoder" *> spacing *> pCommand <* spacing <* var
    string "customDecoder" *> spacing *>
        pCommand <* spacing <* bracketed (skip (many1 var) `or` skip anonFunc)
    `andThen` \dec -> succeed (Custom dec)

-- 17 KEY VALUE PAIRS
pKeyValuePairs : Parser Command
pKeyValuePairs =
    string "keyValuePairs" <* spacing <* pCommand
    `andThen` \_ -> succeed KeyValuePairs

-- CALL
pCall : Parser Command
pCall =
    word
    `andThen` \proc -> many (spaces *> (stringLiteral `or` pCommand))
    `andThen` \args -> succeed <| Call proc args
