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
        skip <| pVar<* between possibleSpacing spacing (char ':') <* many (noneOf ['\n']) *> spacing

-- C O M M A N D S
commands =
    bracketed <|
        rec <| \() ->
            choice
                [ pProblem
                , pString
                , pInt
                , pFlt
                , pBln
                , pNull
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
                , pStr
                , pCall
                ]

pCommand : Parser Command
pCommand =
    commands
    -- (bracketed <| rec <| \() -> pAndThen)
    -- `or` commands

-- pCommand' : Parser Command
-- pCommand' =
--     commands

-- 0 NO <|
pProblem : Parser Command
pProblem =
    string "|>"
    `andThen` \_ -> fail ["I can't yet handle |>"]

pStr : Parser Command
pStr =
    quotedWord `andThen` (succeed << Str)
    -- pStr `andThen` succeed

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

pNull : Parser Command
pNull =
    (string "null" *> spacing *> (quotedWord `or` word) `andThen` (succeed << Null))
    -- `or`
    -- (string "null" *> word `andThen` (succeed << Null ))

-- 5 KEY : VALUE
-- does not capture when k is a function applied to something
pKV : Parser Command
pKV =
    C.map KV ( (pStr `or` pVar) <* between spacing spacing (string ":="))
    `C.andMap` pCommand

-- -- 6 AT
pAt : Parser Command
pAt =
    string "at" *> spacing *> listOf (pStr `or` pVar)
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
    -- pStructure "list" List
    string "list" *> spacing *> pCommand
    `andThen` (succeed << List)

pArr : Parser Command
pArr =
    -- pStructure "array" Arr
    string "array" *> spacing *> pCommand
    `andThen` (succeed << Arr)

pStructure : String -> (Command -> Command) -> Parser Command
pStructure typ cnstrctr =
    string typ *> spacing *> pCommand
    `andThen` (succeed << cnstrctr)

-- 10 TUPLE
pTuple : Parser Command
pTuple =
    string "tuple" *> Num.digit
    `andThen` \n -> spacing *> (word `or` anonFunc) *> count n (spacing *> pCommand)
    `andThen` (succeed << Tuple)

-- 15 MAYBE
pMaybe : Parser Command
pMaybe =
    string "maybe" <* spacing <* pCommand
    `andThen` \_ -> succeed MaybeCommand

-- 17 KEY VALUE PAIRS
pKeyValuePairs : Parser Command
pKeyValuePairs =
    string "keyValuePairs" <* spacing <* pCommand
    `andThen` \_ -> succeed KeyValuePairs

pDict : Parser Command
pDict =
    -- pStructure "dict" Dict
    string "dict" *> spacing *> pCommand
    `andThen` (succeed << Dict)

-- MAP
pMap : Parser Command
pMap =
    -- string "map" *> spacing *> (pVar`or` anonFunc) *> spacing *> pCommand
    string "map" *> spacing *> (transformFunc `or` anonFunc) *> spacing *> pCommand
    `andThen` (succeed << Map)

-- 14 ONEOF
pOneOf : Parser Command
pOneOf =
    string "oneOf" *> spacing *> listOf pCommand
    `andThen` (succeed << OneOf)

-- SUCCEED
-- succeed : a -> Decoder a
-- ************* TOO NARROW *************************
pSucceed : Parser Command
pSucceed =
    -- string "succeed" *> spacing *> (pStr `or` pVar)
    string "succeed" *> spacing *> pStr
    `andThen` (succeed << Succeed)

-- fail : String -> Decoder a
-- ***** TOO NARROW ***** string could also be a passed parameter of a function
pFail : Parser Command
pFail =
    string "fail" *> spacing *> pStr
    `andThen` (succeed << DFail)

-- 16 CUSTOM
-- customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
pCustom : Parser Command
pCustom =
    -- string "customDecoder" *> spacing *> pCommand <* spacing <* pVar
    string "customDecoder" *> spacing *>
        pCommand <* spacing <* bracketed (skip (many1 pVar) `or` skip anonFunc)
    `andThen` \dec -> succeed (Custom dec)

-- CALL
pCall : Parser Command
pCall =
    word
    `andThen` \proc -> many (spaces *> (pStr `or` pCommand))
    `andThen` \args -> succeed <| Call proc args

-- V A R I A B L E
pVar: Parser Command
pVar =
    word `andThen` \w -> succeed <| Call w []



-- 11 ANDTHEN
-- string `andThen` test
-- pAndThen : Parser Command
-- pAndThen =
--     let ans =
--         -- pCommand' <* spacing <* string "`andThen`" <* spacing
--         before <* string "`andThen`" <* spacing
--         -- `andThen` \dec1 -> pCommand
--         `andThen` \str -> pCommand
--         `andThen` \dec2 ->
--             case str of
--                 Str str ->
--                     case parse pCommand' str of
--                         (Done dec1, cntx) ->
--                             succeed <| AndThen dec1 (Proc "andthen" [] dec2)
--                         (Fail e, cntx) ->
--                             fail <| "andThen" :: e
--                 _ -> fail ["andThen", toString str]
--
--     in ans

-- before : Parser Command
-- before =
--     C.map (Str << String.fromList) <| many (noneOf ['`', '\n'])
    -- in C.map (\y -> Call "def " [y]) x
--     -- (....) or pVar
--     -- bracketed (many1 <| noneOf [')'])
--     varOrBrackets
--         <* spacing <* string "`andThen`" <* spacing <* (pVar`or` anonFunc)
--     `andThen`
--         \decStr ->
--             case parse pCommand decStr of
--                 (Done com, _) -> (succeed << AndThen) com
--                 (Fail m, _) -> (succeed << AndThen) (Error <| toString m)
