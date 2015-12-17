module Emit (..) where

import Json.Decode as Json exposing (..)
import Array
import List exposing (length)

import AST exposing (..)

emit : List Command -> Command -> Json.Decoder String
emit env com =
    let
        filt : Param -> Command -> Bool
        filt n f =
            case f of
                Proc name _ _ -> n == name
                Var name _ -> n == name
                otherwise -> False
        -- env contains Var (aliases) and Procs
        lookup : String -> Command
        lookup n =
            Maybe.withDefault
                (Error "loopup error")
                (List.head <| List.filter (filt n) env)
    in
    case com of
        Strng -> string
        Itg -> Json.map toString int
        Flt -> Json.map toString float
        KV key value ->
            case key of
                Str k' ->
                    k' := (emit env value)
                Call p _ ->
                    case lookup p of
                        Var _ p' ->
                            emit env <| KV p' value
                        otherwise -> fail <| "KV: can't call " ++ p
                otherwise ->
                    fail <| "KV: odd key " ++ toString key
                -- P p ->
                --     case lookup p of
                --         Var _ p' ->
                --             emit env <| KV p' value
                --         otherwise -> fail <| "KV: can't lookup " ++ toString key
                    -- emit env <| KV (emit env key) value
        Object coms ->
            let
                go : Command -> Json.Decoder String -> Json.Decoder String
                go com acc =
                    object2 (\a b -> b ++ ", " ++ a) (emit env com) acc
            in  List.foldr go (succeed "obj: ") coms
        -- At lst com ->
        --     let
        --         go l =
        --             case l of
        --                 Str l -> l
        --                 otherwise ->
        --                     case lookup l of
        --                         Var _ l' -> l'
        --                         otherwise -> "At: lookup failure"
        --         lst' = map go lst
        --     in
        --         at lst' (emit env com)

        List com ->
            list (emit env com)
                |> Json.map (List.foldr (++) "")
        Arr com ->
            array (emit env com)
                |> Json.map (Array.foldr (++) "")
        -- tuple2:  (String -> String -> value) -> Decoder String -> Decoder String -> Decoder String
        Tuple coms ->
            list value
            `andThen` \vs ->
                if length vs == length coms
                    then
                        (emit env <| Object <| List.indexedMap (\i com -> KV (Str (toString i)) com) coms)
                        `andThen` Json.succeed
                        -- emit env (Object <| map toString coms)
                        -- maybe (emit env <| Object <| List.indexedMap (\i com -> KV (Str (toString i)) com) coms)
                        -- `andThen` \res ->
                        --     case res of
                        --         Nothing ->
                        --             fail <| "An error occured decoding with tuple" ++ (toString <| length coms)
                        --         Just ans -> Json.succeed ans
                            -- case res of
                            --     Nothing ->
                            --         fail <| "An error occured decoding with tuple" ++ (toString <| length coms)
                            --     Just ans -> Json.succeed ans
                    else fail <|
                        "Array has wrong length: had " ++
                        toString (length vs) ++ " elements vs expected " ++
                        toString (length coms)
        --     -- let f a b = a
        --     -- in
        --     -- case coms of
        --     --     [c1] -> tuple1 identity (emit env c1)
        --     --     [c1, c2] ->
        --     --         tuple2 f (emit env c1) (emit env c2)     -- Decoder String
        --     --     [c1, c2, c3] ->
        --     --         tuple3 (\a _ _->a) (emit env c1) (emit env c2) (emit env c3)
        --     --     [c1, c2, c3, c4] ->
        --     --         tuple4 (\a _ _ _->a) (emit env c1) (emit env c2) (emit env c3) (emit env c4)
        --     --     [c1, c2, c3, c4, c5] ->
        --     --         tuple5 (\a _ _ _ _->a) (emit env c1) (emit env c2) (emit env c3) (emit env c4) (emit env c5)
        --
        Map com -> emit env com
        Succeed _ -> Json.succeed "succeed"
        MaybeCommand -> Json.succeed "maybe"
        KeyValuePairs -> Json.succeed "keyValuePairs"
        -- Proc
        Call name params ->
            case lookup name of
                Proc pname pargs pcom ->
                    let
                        env' =
                            List.foldl
                                (\(p, a) acc -> Var p a :: acc)
                                env
                                (zip pargs params)
                    in emit env' pcom
                Var vname vcom ->
                    emit env vcom
                otherwise ->
                    Json.fail ("Call loop failed for " ++ name)

        Error e -> Json.fail e
        otherwise -> Json.succeed "Whoops - missing something in emit"

andMap : Decoder (a -> b) -> Decoder a -> Decoder b
andMap decFun decA =
    object2 (\f a -> f a) decFun decA

zip l1 l2 =
    case (l1,l2) of
        (l::ls, k::ks) -> (l,k) :: zip ls ks
        otherwise -> []
{-}
tuple5
    :  (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder value

object5
    :  (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder value
-}
