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
## Test your Json decode statements

I cannot yet parse `|>`, `null` nor `andThen`.

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
