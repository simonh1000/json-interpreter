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
    case C.parse ((oneOrMoreProcs `or` singleItem) <* possibleSpacing <* end) s of
        (Done lst, cntx) ->
            Result.Ok lst
        (Fail errs, cntx) ->
            Result.Err (toString errs ++ "Parsing error beginning at: " ++ cntx.input)

oneOrMoreProcs : Parser (List Command)
oneOrMoreProcs =
    pProc `andThen`
        \p1 -> many ((many1 eol) *> pProc)
        `andThen` \ps -> succeed (p1 :: ps)
    -- many1 <|
    --     (pProc <* (many1 spacing))

singleItem : Parser (List Command)
singleItem =
    optional Succeed pCommand <* possibleSpacing <* end `andThen` \item -> succeed [ item ]

-- PROCS / FUNCTIONS

-- includes spaces at end of definition
pProc : Parser Command
pProc =
    parseFunctionSignature *> many1 (word <* spaces)
        `andThen` \ps -> (char '=') *> spacing *> pCommand <* possSpaces
        `andThen` \decoder ->
            case ps of
                [] -> fail ["Failure parsing Proc"]
                [x] -> succeed <| Proc x [] decoder
                (x :: xs) -> fail ["Can't handle functions with parameters yet"]

-- if there is signature, ignore it
parseFunctionSignature : Parser ()
parseFunctionSignature =
    optional
        ()
        <| skip <| word <* between possibleSpacing spacing (char ':') <* many (noneOf ['\n']) *> spacing

-- C O M M A N D S

pCommand : Parser Command
pCommand =
    bracketed <|
        rec <| \() ->
            choice
                [ pAndThen      -- needs to precede primitives
                , pProblem
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
                -- , pProc
                , pCall
                ]
-- 0 NO <|
pProblem : Parser Command
pProblem =
    choice [ string "<|", string "|>" ]
    `andThen` \_ -> fail ["I can't yet handle <| or |>"]

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
pKV : Parser Command
pKV =
    C.map KV (stringLiteral <* between spacing spacing (string ":="))
    `C.andMap` pCommand

-- 6 AT
pAt : Parser Command
pAt =
    string "at" *> spacing *> listOf (word `or` stringLiteral)
    `andThen` \lst -> spacing *> pCommand
    `andThen` \dec -> succeed (At lst dec)

-- 7 OBJECT
pObject : Parser Command
pObject =
    string "object" *> Num.digit
    `andThen` \n ->
        spacing *> (anonFunc `or` word) *> count n (spacing *> bracketed (possibleSpacing *> pCommand))
        `andThen` (succeed << Object)
-- 8 LIST / ARRAY
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
    `andThen` \n -> spacing *> (anonFunc `or` word) *> count n (spacing *> pCommand)
    `andThen` (succeed << Tuple)

-- 11 ANDTHEN
pAndThen : Parser Command
pAndThen =
    -- (....) or word
    -- bracketed (many1 <| noneOf [')'])
    wordOrBrackets
        <* spacing <* string "`andThen`" <* spacing <* (word `or` anonFunc)
    `andThen`
        \decStr ->
            case parse pCommand decStr of
                (Done com, _) -> (succeed << AndThen) com
                (Fail m, _) -> (succeed << AndThen) (Error <| toString m)

-- 12 SUCCEED
pSucceed : Parser Command
pSucceed =
    string "succeed" *> spacing *> (many1 word)
    `andThen` \_ -> succeed Succeed

-- 13 MAP
pMap : Parser Command
pMap =
    -- string "map" *> spacing *> (word `or` anonFunc) *> spacing *> pCommand
    string "map" *> spacing *> bracketed pCall *> spacing *> pCommand
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
    -- string "customDecoder" *> spacing *> pCommand <* spacing <* word
    string "customDecoder" *> spacing *>
        pCommand <* spacing <* bracketed (skip (many1 word) `or` skip anonFunc)
    `andThen` \dec -> succeed (Custom dec)

-- 17 KEY VALUE PAIRS
pKeyValuePairs : Parser Command
pKeyValuePairs =
    string "keyValuePairs" <* spacing <* pCommand
    `andThen` \_ -> succeed KeyValuePairs

-- CALL
pCall : Parser Command
pCall =
    -- possibleSpacing *> word
    -- `andThen` \w -> succeed (Call w)
    word `andThen` \p ->
        many (spaces *> (word `or` stringLiteral))
        `andThen` \args -> succeed <| Call p args
