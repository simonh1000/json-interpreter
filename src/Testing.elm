module Testing (..) where

import Html exposing (Html, h1, h2, h3, text, div, a, header, footer, p, button, textarea, span)
import Html.Attributes as Attr exposing (type', id, style, multiple, href)
import Html.Events exposing (onClick, on, onSubmit, onWithOptions, targetValue)

import Effects exposing (Effects)
import Time exposing (Time)

import AST exposing (..)
import Parser2 exposing (parseString)
import Emit exposing (emit)
import Json.Decode as Json

type alias TestSuite =
    { decodeStr : String
    , jsonStr : String
    , answer : String
    , description : String
    }

type alias Model = List TestSuite

init =
    [ TestSuite "" "" "Unexpected end of input" "Empty string"
    , TestSuite "int" "6" "6" "Integer 6"
    , TestSuite
        "default = list int"
        "[4,5,6,7,8]"
        "List 4 5 6 7 8"
        "List of integers"
    , TestSuite
        "default = array float"
        "[4,5,6,7,8]"
        "Array.fromList 4 5 6 7 8"
        "Array of floats"
    , TestSuite
        "default = tuple5\n\t\t\tModel\n\t\t\tint int int int int"
        "[4,5,6,7,8]"
        "Tup 4 5 6 7 8"
        "Tuple5 of ints"
    , TestSuite
        "default = \"data\" :=\n\t\ttuple5\n\t\t\tModel\n\t\t\tint int int int int"
        "{\"data\": [4,5,6,7,8]}"
        "Tup 4 5 6 7 8"
        "KV with tuple5"
    , TestSuite
        "default = \"data\" := (list int)"
        "{\"data\": [4,5,6,7,8]}"
        "List 4 5 6 7 8"
        "list int"
    , TestSuite
        "default = at \n\t[\"outer\", \"inner\"] <|\n\t\tstring"
        "{\"outer\": {\"inner\": \"Success\"}}"
        "Success"
        "at ..."
    , TestSuite
        "default = object1 blah <| \"outer\" := (object1 blahblah <| \"inner\" := string)"
        "{\"outer\": {\"inner\": \"Success\"}}"
        "Obj Obj Success"
        "Nested objects"
    , TestSuite
        "default = (dict int)"
        "{ \"x\":3, \"y\":4, \"z\":5 }"
        "Dict.fromList (x,3) (y,4) (z,5)"
        "dict ..."
    , TestSuite
        "default = maybe (\"profession\" := string)"
        "{ \"profession\":\"juggler\", \"z\":5 }"
        "Just \"juggler\""
        "maybe something"
    , TestSuite
        "default : Decoder (Maybe String)\ndefault = maybe (\"name\" := string)"
        "{ \"profession\":\"juggler\", \"z\":5 }"
        "Nothing"
        "maybe Nothing"
    , TestSuite
        "default = at \n\t[\"outer\", \"inner\"] <|\n\t\toneOf [string, int]"
        "{\"outer\": {\"inner\": \"Success\"}}"
        "Success"
        "at + oneOf"
    , TestSuite
        "default = map Model (\"outer\" := bool)"
        "{\"outer\": true}"
        "True"
        "map boolean"
    , TestSuite
        "default = oneOf [ tuple2 (,) float float\n, object2 (,) (\"x\" := float) (\"y\" := float)\n, fail \"expecting point\"\n]"
        "[4,5,6]"
        "\"expecting point\""
        "fail, as part of oneOf"
    , TestSuite
        "default : Decoder (Float,Float,Float)\ndefault =\n\tobject3 (,,)\n\t\t(\"x\" := float)\n\t\t(\"y\" := float)\n\t\t(oneOf [ \"z\" := float, succeed 0 ]) "
        "{\"x\": 4, \"y\": 5, \"z\":6}"
        "Obj 4 5 6"
        "object3"
    , TestSuite
        "default : Decoder (Float,Float,Float)\ndefault =\n\tobject3 (,,)\n\t\t(\"x\" := float)\n\t\t(\"y\" := float)\n\t\t(oneOf [ \"z\" := float, succeed 0 ]) "
        "{\"x\": 4, \"y\": 5, \"z\":\"six\"}"
        "Obj 4 5 0"
        "succeed, with object3"
    , TestSuite
        "f1 s = at [\"outer\", s] string\n\ndefault = f1 \"inner\""
        "{\"outer\": {\"inner\": \"Success\"}}"
        "Success"
        "function applied to string"
    , TestSuite
        "f1 o d = at [o] (\"inner\" := d)\ndefault = f1 \"outer\" string"
        "{\"outer\": {\"inner\": \"Success\"}}"
        "Success"
        "function application"
    , TestSuite
        "f s = s := float\ndefault =\n\tobject3 (,,)\n\t\t(f \"x\") (f \"y\") (f \"z\")"
        "{\"x\": 4, \"y\": 5, \"z\":5.5}"
        "Obj 4 5 5.5"
        "object3 ... (f \"x\")..."
    , TestSuite
        "default = object1 bbbb <| \"inner\" := string"
        "{\"inner\": \"Success\"}"
        "Obj Success"
        "<|"
    , TestSuite
        "default = object1 (bbbb (aaaa)) <| \"inner\" := string"
        "{\"inner\": \"Success\"}"
        "Obj Success"
        "object1 (...(...)...)"
    ]

type Action
    = Tick Time

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Tick _ ->
            (model, Effects.none)
        -- otherwise -> (model, Effects.none)


view : Signal.Address Action -> Model -> Html
view address model =
    div [] <|
        List.map testOne model


testOne m =
    let
        ast =
            case parseString m.decodeStr of
                Result.Ok s -> s
                Result.Err err -> [ Error err ]
        result =
            case Json.decodeString (emit ast (Call "default" [])) m.jsonStr of
                Result.Ok s -> s
                Result.Err err -> err
        success = result == m.answer
    in
        div
            [ answerStyles success ]
            [ p
                [ style
                    [ ("margin", "0") ]
                ]
                [ text <| if success then "" else toString ast ]
            , p
                [ style
                    [ ("margin", "0 0 10px 0") ]
                ]
                [ text <| m.description ++
                    if success
                    then ""
                    else result ++ " " ++ m.answer
                ]
            ]

answerStyles b =
    style
        [ ("color", if b then "green" else "red")
        , ("padding", "0 15px")
        ]
