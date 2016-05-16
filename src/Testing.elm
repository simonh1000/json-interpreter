module Testing exposing (..)

import Html exposing (Html, h1, h2, h3, text, div, a, header, footer, p, button, textarea, span)
import Html.App as Html
import Html.Attributes as Attr exposing (type', id, style, multiple, href)
-- import Html.Events exposing (onClick, on, onSubmit, onWithOptions, targetValue)

import Platform.Cmd exposing (Cmd)
import Time exposing (Time)

import AST exposing (..)
import Parser2 exposing (parseString)
import Emit exposing (emit)
import Json.Decode as Json

import ElmTest exposing (..)

type alias TestSuite =
    { decodeStr : String
    , jsonStr : String
    , answer : String
    , description : String
    }

type alias Model = List TestSuite

init =
    [ TestSuite "" "" "Given an invalid JSON: Unexpected end of JSON input" "Empty string"
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
        "default = succeed True"
        "{\"x\": 4, \"y\": 5, \"z\":\"six\"}"
        "True"
        "succeed Boolean"
    , TestSuite
        "f1 s = at [\"outer\", s] string\n\ndefault = f1 \"inner\""
        "{\"outer\": {\"inner\": \"Success\"}}"
        "Success"
        "function applied to string"
    , TestSuite
        "default = f1  \t\"outer\" string\nf1 o d = at [o] (\"inner\" := d)\n"
        "{\"outer\": {\"inner\": \"Success\"}}"
        "Success"
        "function application with double space"
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
        "default = object1 (\\b -> (b ++ \"bbbb\")) <| \"inner\" := string"
        "{\"inner\": \"Success\"}"
        "Obj Success"
        "Complex transformFunc - object1 (...(...)...) ..."
    , TestSuite
        "f1 = \"Activity\" := tuple1 id aaaa\n\ndefault = int"
        "5"
        "5"
        "\n Call function at end of proc"
    , TestSuite
        "f1 = \"Activity\" := tuple1 id (f1 aaa)\n\ndefault = int"
        "5"
        "5"
        "\n Tuple with embedded function call "
    ]

type Msg
    = NoOp
    -- | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)
    -- case msg of
    --     Tick _ ->
    --         (model, Cmd.none)
        -- otherwise -> (model, Cmd.none)


view : Model -> Html Msg
view model =
    div [] <|
        -- List.map testOne model
        List.map viewAssertion model

tester : TestSuite -> Test
tester m =
    let
        ast =
            case parseString m.decodeStr of
                Result.Ok s -> s
                Result.Err err -> [ Error err ]
        result =
            case Json.decodeString (emit ast (Call "default" [])) m.jsonStr of
                Result.Ok s -> s
                Result.Err err -> err
    in
    test
      m.description
      (assertEqual result m.answer)

viewAssertion : TestSuite -> Html Msg
viewAssertion t =
    div []
        [ text t.description
        , text ": "
        , t
            |> tester
            |> stringRunner
            |> text
        ]

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
                [ text <|
                    if success
                        -- then ""
                        then toString ast
                        else toString ast ]
            , p
                [ style
                    [ ("margin", "0 0 10px 0") ]
                ]
                [ text <| m.description ++
                    if success
                    then ""
                    else ". " ++ result
                ]
            ]

answerStyles b =
    style
        [ ("color", if b then "green" else "red")
        , ("padding", "0 15px")
        ]

-- MAIN
main =
    Html.program
        { init = (init, Cmd.none)
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
