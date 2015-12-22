module Parser2 (..) where

import Combine as C exposing (..)
import Combine.Char exposing (..)
import Combine.Num as Num
import Combine.Infix exposing (..)
import String
import Result exposing (Result)

import Common2 exposing (..)
import AST exposing (Command(..))

parseString : String -> Result.Result String (List Command)
parseString s =
    case parse
            (many spacing
            *> (choice [oneOrMoreProcs, singleItem, empty])
            -- *> (choice [oneOrMoreProcs, singleItem])
            <* possibleSpacing <* end) s of
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
    `andThen` \item -> succeed [ Proc "default" [] item ]

empty : Parser (List Command)
empty = succeed [ Proc "default" [] (Str "empty") ]

-- PROCS / FUNCTIONS

-- ***** Should not fail when a parameter is used in the body of Proc **********
pProc : Parser Command
pProc =
    parseFunctionSignature *> many1 (word <* spacing)
        `andThen` \ps -> (char '=') *> spacing *> pCommand
        `andThen` \decoder ->
            case ps of
                [] -> fail ["Failure parsing Proc"]
                (x :: xs) -> succeed <| Proc x xs decoder

-- if there is signature, ignore it
parseFunctionSignature : Parser ()
parseFunctionSignature =
    optional
        () <|
        skip <| word <* between possibleSpacing spacing (char ':') <* many (noneOf ['\n']) *> spacing

-- C O M M A N D S
pCommand : Parser Command
pCommand =
    bracketed <|
        rec <| \() ->
            choice
                [ pProblem
                , pPrimitive
                -- , pNull
                , pList
                , pArr
                , pTuple
                , pKV
                , pAt
                , pObject
                , pKeyValuePairs
                , pDict
                , pMaybe
                , pOneOf
                , pMap
                , pFail
                , pSucceed
                , pCustom
                -- , pStr
                , pCall
                ]

-- 0 NO <|
pProblem : Parser Command
pProblem =
    string "|>"
    `andThen` \_ -> fail ["I can't yet handle |>"]

pStr : Parser Command
pStr =
    string_ `andThen` (succeed << Str)

pNumber : Parser Command
pNumber =
    int_ `andThen` (succeed << Str)

pBln : Parser Command
pBln =
    (string "True" `or` string "False") -- Elm values
    `andThen` (succeed << Str)

-- -- V A R I A B L E
pVar: Parser Command
pVar =
    word `andThen` \w -> succeed <| Call w []

-- 1 PRMITIVES
-- **** fails on e.g. stringy ******
-- USE WHILE INSTEAD?????
pPrimitive : Parser Command
pPrimitive =
    word
    `andThen` \p ->
        case p of
            "string" -> succeed Strng
            "int" -> succeed Itg
            "float" -> succeed Flt
            "bool" -> succeed Bln
            otherwise -> fail ["x", p, "y"]

pNull : Parser Command
pNull =
    (string "null" *> spacing *> (string_ `or` word)
    `andThen` (succeed << Null))
--
-- -- -- 8 LIST / ARRAY
pList : Parser Command
pList =
    -- pStructure "list" List
    string "list" *> spacing *> pCommand
    `andThen` (succeed << List)

pArr : Parser Command
pArr =
    -- pStructure "array" Arr
    string "array" *> spacing *> pCommand
    `andThen` (succeed << Arr)

-- pStructure : String -> (Command -> Command) -> Parser Command
-- pStructure typ cnstrctr =
--     string typ *> spacing *> pCommand
--     `andThen` (succeed << cnstrctr)
--
-- -- 10 TUPLE
pTuple : Parser Command
pTuple =
    string "tuple" *> Num.digit
    `andThen` \n -> spacing *> transformFunc *> count n (spacing *> pCommand)
    `andThen` (succeed << Tuple)
--
-- -- 5 KEY : VALUE
-- -- does not capture when k is a function applied to something
pKV : Parser Command
pKV =
    map KV ( (pStr `or` pVar) <* between spacing spacing (string ":="))
    `andMap` pCommand

-- AT
pAt : Parser Command
pAt =
    string "at" *> spacing *> listOf (pStr `or` pVar)
    `andThen` \lst -> spacing *> pCommand
    `andThen` \dec -> succeed (At lst dec)

-- OBJECT
pObject : Parser Command
pObject =
    string "object" *> Num.digit
    `andThen` \n ->
        spacing *> transformFunc *> count n (spacing *> bracketed (possibleSpacing *> pCommand))
        `andThen` (succeed << Object)

-- KEY VALUE PAIRS
pKeyValuePairs : Parser Command
pKeyValuePairs =
    string "keyValuePairs" <* spacing <* pCommand
    `andThen` \_ -> succeed KeyValuePairs

pDict : Parser Command
pDict =
    -- pStructure "dict" Dict
    string "dict" *> spacing *> pCommand
    `andThen` (succeed << Dict)

-- 15 MAYBE
pMaybe : Parser Command
pMaybe =
    string "maybe" *> spacing *> pCommand
    `andThen` (succeed << JMaybe)

-- 14 ONEOF
pOneOf : Parser Command
pOneOf =
    string "oneOf" *> spacing *> listOf pCommand
    `andThen` (succeed << OneOf)

-- MAP
pMap : Parser Command
pMap =
    string "map" *> spacing *> transformFunc *> spacing *> pCommand
    -- string "map" *> spacing *> (transformFunc `or` anonFunc) *> spacing *> pCommand
    `andThen` (succeed << Map)
--
-- -- F A I L   /   S U C C E E D
-- -- fail : String -> Decoder a
-- -- ***** TOO NARROW ***** string could also be a passed parameter of a function
pFail : Parser Command
pFail =
    string "fail" *> spacing *> pStr
    `andThen` (succeed << DFail)
--
-- -- succeed : a -> Decoder a
-- -- ************* TOO NARROW *************************
pSucceed : Parser Command
pSucceed =
    -- string "succeed" *> spacing *> (pNumber `or` pStr)
    string "succeed" *> spacing *> (choice [pBln, pNumber, pStr])
    `andThen` (succeed << Succeed)
--
-- -- 16 CUSTOM
-- -- customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
pCustom : Parser Command
pCustom =
    -- string "customDecoder" *> spacing *> pCommand <* spacing <* pVar
    string "customDecoder" *> spacing *>
        -- pCommand <* spacing <* bracketed (skip (many1 pVar) `or` skip anonFunc)
        pCommand <* spacing <* transformFunc
    `andThen` \dec -> succeed (Custom dec)

-- CALL
{-
tuple1 id f1
tuple1 id (f2 "string")
tuple1 id (f2 [something in scope])
tuple1 id (f2 primitive)

but .... := f2 [something] does not need brackets

*** use of char ' ' below may create edge cases
-}

pCall : Parser Command
pCall =
    -- (word `andThen` \proc -> succeed <| Call proc [])
    -- `or` (rec <| \() -> pCallWithParams)
    word
    -- `andThen` \proc -> many (spacing *> (pStr `or` pCommand))
    `andThen` \proc -> many (many1 (oneOf [' ', '\t']) *> (pStr `or` pCommand))
    `andThen` \args -> succeed <| Call proc args

pCallWithParams : Parser Command
pCallWithParams =
    between openBrackets closeBrackets <|
        (word
        `andThen` \proc -> many1 (spacing *> (pStr `or` pCommand))
        `andThen` \args -> succeed <| Call proc args)
