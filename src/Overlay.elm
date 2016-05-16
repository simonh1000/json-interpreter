module Overlay exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Markdown

elmBlue = "#60B5CC"

type alias Model = Bool

init = True

update : Msg -> Model -> Model
update msg model =
    case msg of
        Close -> False

type Msg
    = Close

content : Html Msg
content =
    Markdown.toHtml [] """
# Elm Json.Decode interpreter

This online tool aims to support the development of Json decoders. It implements a simple interpreter of the Json.Decode library, and calls `decodeString` with the result and the json value provided. An example is shown below - can you correct the deliberate mistake?

The Decoders must already compile - this application is only about testing compatability with json data.

A few things to note:
 - `succeed` is not fully polymorphic;
 - `andThen` is not supported, which is primarily because;
 - there is no interpretation of Elm control flow syntax, e.g. `case`, `if ... then ... else`, ...;
 - `<|` **is interpreted**, but `|>` is not
 - some decoders - e.g. `keyValuePairs`, `value` - make no specific requirements on the Json, and are thus considered always compatible

"""

view : Model -> Html Msg
view m =
    div
        [ style <| if m then overlayOpen else overlayClosed ]
        [ content
        , div
            [ style
                [ ("margin-bottom", "10px")
                ]
            ]
            [ button
                [ onClick Close ]
                [ text "Close" ]
            ]
        ]

overlayOpen =
    [ ( "background-color", elmBlue )
    , ( "border-top", "3px solid white" )
    , ( "padding", "0 15px")
    , ( "transition", "max-height 2s" )
    ]

overlayClosed =
    [ ( "max-height", "0" )
    , ( "transition", "max-height 2s" )
    , ( "border-top-width", "0" )
    ] ++ overlayOpen
