module Overlay (Action(..), view, elmBlue) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Markdown

elmBlue = "#60B5CC"

type Action
    = Close

content : Html
content =
    Markdown.toHtml """
# Elm Json.Decode interpreter

This online tool aims to support the development of Json decoders. It implements a simple interpreter of the Json.Decode library, and calls `decodeString` with the result and the json value provided. An example is shown below - can you correct the deliberate mistake?

Note that some decoders - e.g. `keyValuePairs`, `value` - make no specific requirements on the Json, and are thus not interpreted.

A few things to note:
 - `succeed` is not polymorphic; it only supports strings and integers
 - `andThen` is not supported, which is primarily because;
 - there is no interpretation of Elm control flow syntax, e.g. `case`, `if ... then ... else`, ...; and
 - while `<|` **is supported**, `|>` is not

"""

view address m =
    div
        [ style <| if m then overlayOpen else overlayClosed ]
        [ content
        , div
            [ style
                [ ("margin-bottom", "10px")
                ]
            ]
            [ button
                [ onClick address Close
                ]
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
