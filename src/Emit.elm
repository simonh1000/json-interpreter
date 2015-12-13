module Emit (..) where

import Json.Decode as Json exposing (..)
import Array
import List exposing (length)

import AST exposing (Command(..))

emit : List Command -> Command -> Json.Decoder String
emit procs com =
    let
        emit' = emit procs
        filt n f =
            case f of
                Proc name _ _ -> n == name
                otherwise -> False
        lookup calledFunc =
            case calledFunc of
                "Default" ->
                    Maybe.withDefault (Error "nothing parsed") (List.head procs)
                otherwise ->
                    let proc = List.filter (filt calledFunc) procs
                    in case proc of
                        [(Proc _ _ d)] -> d
                        otherwise -> Error <| "Could not find function: " ++ calledFunc
    in
    case com of
        Strng -> string
        Itg -> Json.map toString int
        Flt -> Json.map toString float
        KV k v -> (k := emit' v)
        Object coms ->
            let
                go : Command -> Json.Decoder String -> Json.Decoder String
                go com acc =
                    object2 (\a b -> b ++ ", " ++ a) (emit' com) acc
            in  List.foldr go (succeed "obj: ") coms
        At lst com -> at lst (emit' com)
        List com ->
            list (emit' com)
                |> Json.map (List.foldr (++) "")
        Arr com ->
            array (emit' com)
                |> Json.map (Array.foldr (++) "")
        -- tuple2:  (String -> String -> value) -> Decoder String -> Decoder String -> Decoder String
        Tuple coms ->
            -- let f a b = a
            -- in
            -- case coms of
            --     [c1] -> tuple1 identity (emit' c1)
            --     [c1, c2] ->
            --         tuple2 f (emit' c1) (emit' c2)     -- Decoder String
            --     [c1, c2, c3] ->
            --         tuple3 (\a _ _->a) (emit' c1) (emit' c2) (emit' c3)
            --     [c1, c2, c3, c4] ->
            --         tuple4 (\a _ _ _->a) (emit' c1) (emit' c2) (emit' c3) (emit' c4)
            --     [c1, c2, c3, c4, c5] ->
            --         tuple5 (\a _ _ _ _->a) (emit' c1) (emit' c2) (emit' c3) (emit' c4) (emit' c5)

            maybe (emit' <| Object <| List.indexedMap (\i com -> KV (toString i) com) coms)
            `andThen` \res ->
                case res of
                    Nothing ->
                        fail <| "An error occured decoding with tuple" ++ (toString <| length coms)
                    Just ans -> Json.succeed ans
        Map com -> emit' com
        Succeed -> Json.succeed "succeed"
        MaybeCommand -> Json.succeed "maybe"
        KeyValuePairs -> Json.succeed "keyValuePairs"
        -- Proc
        Call name args -> emit' (lookup name)
        Error e -> Json.fail e
        otherwise -> Json.succeed "Whoops - missing something in emit"

andMap : Decoder (a -> b) -> Decoder a -> Decoder b
andMap decFun decA =
    object2 (\f a -> f a) decFun decA


                    -- oneOf
                    --     [ (emit' <| Object <| List.indexedMap (\i com -> KV (toString i) com) coms)
                    --     , fail "Tuple error"
                    --     ]
                    -- Need a Decoder String
                    -- that checks 3 values
                    -- and that each is of right type
                    -- tuple2 f (emit' c1) (tuple2 f (emit' c2) (tuple2 f (emit' c3) (succeed "tup:")))
                    -- tuple2 f (emit' c1)
                    -- `andMap` (tuple2 f (emit' c2)
                    -- `andMap` tuple2 f (emit' c3) (succeed "tup: "))
                -- otherwise -> fail "tuple"


-- decodeString (Json.Decode.Decoder a) String

                    -- \lst ->
                    --     case lst of
                    --         (a::b::c::[]) ->
                    --             tuple2 f (emit' c3) <|
                    --             tuple2 f (emit' c1) (emit' c2)
                    --         otherwise -> fail "tuple3 error"
                    -- tuple3 (\a b c->a++b++c) (emit' c1) (emit' c2) (emit' c3)
            -- let
            --     go : Command -> Json.Decoder String -> Json.Decoder String
            --     go com acc =
            --         tuple2 (\a b -> b ++ ", " ++ a) (emit' com) acc
            -- in  List.foldr go (succeed "tup: ") coms
                --     -- Json.decodeString (tuple2 f (emit' c1) (emit' c2)) (take 2 lst)
                --     -- `andThen`
                --     \lst ->
