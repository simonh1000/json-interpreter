module Footer (view) where

import Html exposing (Html, h1, h2, h3, text, div, a, header, footer, p, button, textarea, span)
import Html.Attributes as Attr exposing (type', id, style, multiple, href)
import Html.Events exposing (onClick, on, onSubmit, onWithOptions, targetValue)

type alias Model =
    { presult : Result String (List Command)
    , dresult : Maybe (Result String String)
    }

view address model =
    footer
        [ style
            [ ( "display", "flex")
            ]
        ]
        [ parseView address model.presult
        , parseView address model.dresult
        ]

footerSection : Color ->  List Html -> String -> Html
footerSection col firstLine secondLine =
    div
        [ footerStyles col ] <|
        firstLine ++
        [ p [ smallStyle ] [ text secondLine ]
        ]
-- success: 1st line as h3, second as 10px
-- fail: 1 line as 14px
-- unknown

parseResult address model =
    let
        (col, line1, line2) =
            case model of
                Result.Ok s ->
                    ( elmGreen
                    , [ h3
                            [ style [("margin", "0")] ]
                            [ text "Choose entry point"
                            ]
                        , div []
                            (List.map (decoderButton address) model.ast)
                      ]
                    , toString s
                    )
                Result.Err err ->
                    ( elmRed
                    , [ p [] [ text err ] ]
                    , ""
                    )
    in footerSection col line1 line2

decodeResult address model =
    let col =
        (col, line1, line2) =
            case model of
                Nothing ->
                    (elmOrange, "", "")
                Just (Result.Ok res) ->
                    ( elmGreen
                    , [ h3
                            [ style [("margin", "0")] ]
                            [ text "Success - match"
                            ]
                      ]
                    , res
                    )
                Just (Result.Err err) ->
                    ( elmRed
                    , [ p [] [ text err ] ]
                    , ""
                    )

decoderButton address func =
    let n =
        case func of
            Proc name _ _ -> name
            otherwise -> "!!!!!"
    in
        span
            [ onClick address (ChooseDecoder n)
            , fakeButton
            ]
            [ text n ]

--
-- escapeText str =
--     String.fromList <|
--         List.foldr
--         (\c acc -> if c == '"' then '\\' :: '"' :: acc  else c :: acc )
--         []
--         (String.toList str)

footerStyles c =
    style
        [ ("background-color", c)
        , ("flex-grow", "1")
        , ("flex-basis", "0")
        , ("padding", "15px")
        -- , ("display", "flex")
        -- , ("flex-wrap", "wrap")
        -- , ("align-items", "center")
        ]
