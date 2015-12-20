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

This online tool aims to support the development of Json decoders. It implements a simple interpreter of the Json.Decode library and uses the applies the interpreted decoder to sample json content. An example is shown below - can you correct the deliberate mistake?

Note that some decoders - keyValuePairs`, `value` etc. - make few requirements on the Json, and are thus not interpreted.

Limitations - no support for:
 - `andThen`, which is primarily because of;
 - Elm control flow syntax, e.g. `case`, `|>`, but note that **`<|` is supported**

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
