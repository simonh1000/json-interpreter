module Emit (..) where

import Json.Decode as Json exposing (..)
import Array
import List exposing (length)

import AST exposing (..)

filt : Param -> Command -> Bool
filt n f =
    case f of
        Proc name _ _ -> n == name
        Var name _ -> n == name
        otherwise -> False

emit : List Command -> Command -> Json.Decoder String
emit env com =
    let
        lookup : String -> Command
        lookup n =
            Maybe.withDefault
                (Error "loopup error")
                -- env contains Var (aliases) and Procs
                (List.head <| List.filter (filt n) env)

        eval : Command -> Maybe String
        eval ast =
            case ast of
                Str s -> Just s
                Call p _ ->
                    case lookup p of
                        Var _ p' -> eval p'
                        otherwise -> Nothing
                otherwise -> Nothing
    in
    case com of
        Strng -> string
        Itg -> Json.map toString int
        Flt -> Json.map toString float
        Bln -> Json.map toString bool
        Null s -> null s
        KV key value ->
            case eval key of
                Just k -> k := (emit env value)
                Nothing ->fail <| "Key lookup failure for " ++ (toString key)
        At lst dec ->
            let
                fields = List.map eval lst
            in case List.any ( (==) Nothing) fields of
                True -> fail "something wrong with fields"
                False -> at (List.map (Maybe.withDefault "") fields) (emit env dec)
        Object coms ->
            let
                go : Command -> Json.Decoder String -> Json.Decoder String
                go com acc =
                    object2 (\a b -> b ++ "{k0: " ++ a ++ "}") (emit env com) acc
            in  List.foldr go (succeed "Con: ") coms

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
                        `andThen` succeed
                    else fail <|
                        "Array has wrong length: had " ++
                        toString (length vs) ++ " elements vs expected " ++
                        toString (length coms)
        Map com -> emit env com
        Succeed s ->
            case eval s of
                Just s' -> succeed s'
                Nothing -> fail <| "could not eval " ++ toString s
        MaybeCommand -> succeed "maybe"
        KeyValuePairs -> succeed "keyValuePairs"
        OneOf lst -> oneOf <| List.map (emit env) lst

        -- Only test com1
        -- AndThen com1 com2 ->
        --     emit env com1
        --     `andThen` \v -> succeed (toString v)
                -- let env' = (Proc ______) :: env
                -- in emit env' Call "andThen" [v]
        -- Proc
        Call name params ->
            case lookup name of
                Proc pname pargs pcom ->
                    let
                        -- map proc's args to the passed params
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
        otherwise -> Json.fail <| "Whoops - emit can't yet handle " ++ toString com

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
    -- stringify s =
    -- case s of
    --     Str s' -> s'
    --     otherwise -> "fail"
        -- Call name _ ->
        --     case lookup name of
        --         Var vname vcom ->
        --             vcom
        --         otherwise ->
        --             Json.fail ("Call loop failed for " ++ name)
    -- fields = List.map (\l -> Maybe.withDefault "" <| eval l) lst
            -- case key of
            --     Str k' ->
            --         k' := (emit env value)
            --     Call p _ ->
            --         case lookup p of
            --             Var _ p' ->
            --                 emit env <| KV p' value
            --             otherwise -> fail <| "KV: can't call " ++ p
            --     otherwise ->
            --         fail <| "KV: odd key " ++ toString key
