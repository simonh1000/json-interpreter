module App exposing (Model, Msg(Tick), init, update, view)

import Html exposing (Html, h1, h2, h3, text, div, a, header, footer, p, button, textarea, span)
import Html.App exposing (map)
import Html.Attributes as Attr exposing (type', id, style, multiple, href)
import Html.Events exposing (onClick, onInput)

import Platform.Cmd exposing (Cmd)
import Time exposing (Time)
import String
import Json.Decode as Json

import AST exposing (Command(..))
import Parser2 exposing (parseString)
import Emit exposing (..)
import Overlay

-- MODEL

type alias Model =
    { json : String
    , decoderStr : String
    , ast : List Command
    , parseSuccess : Bool
    , result : String
    , errorMessage: String
    , decodeSuccess : Bool
    , overlay : Overlay.Model
    }

init =
    -- { json = "{\"outer\": {\"inner\": \"Success\"}}"
    -- { json = "{ \"x\":3, \"y\":4, \"z\":5 }"
    { json = "{\"data\": [4,5,6,7,8]}"
    -- , decoderStr = "f1 = \"data\" := (dict int)"
    -- , decoderStr = "\"outer\" := <| \"inner\" := string"
    -- , decoderStr = "f1 = object2 \n\tinit2\n\t(\"outer\" := bool)\n\t(\"inner\" := string)"
    -- , decoderStr = "f1 = at \n\t[\"outer\", \"inner\"] <|\n\t\toneOf [string, int]"
    -- , decoderStr = "f1 = object1 blah <| \"outer\" := (object1 blahblah <| \"inner\" := string)"
    -- , decoderStr = "f1 s = at [\"outer\", s] string\nf2 = f1 \"inner\""
    -- , decoderStr = "f1 o d = at [o] (\"inner\" := d)\nf2 = f1 \"outer\" string"
    -- , decoderStr = "f1 s = s := string\nf2 = \"outer\" := f1 \"inner\""
    -- , decoderStr = "f1 s = \"inner\" := s\nf2 = \"outer\" := f1 boolean"
    -- , decoderStr = "f1 = \"data\" := (list int)"
    , decoderStr = "func = \"data\" :=\n\t\ttuple5\n\t\t\tModel\n\t\t\tint int int int string"
    , result = ""
    , ast = []
    , parseSuccess = False
    , errorMessage = ""
    , decodeSuccess = False
    , overlay = Overlay.init
    }

-- UPDATE

type Msg
    = JsonChange String
    | Decoder String
    | ChooseDecoder String
    | Tick Time
    | OverlayMsg Overlay.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    let
        parse str =
            let newModel =
                { model
                | decoderStr = str
                , decodeSuccess = False
                , result = ""
                }
            in
            -- newModel
            case parseString str of
                Result.Ok ast ->
                    { newModel
                    | ast = ast
                    , parseSuccess = True
                    }
                Result.Err e ->
                    { newModel
                    | ast = []
                    , parseSuccess = False
                    , errorMessage = e
                    }
    in
    case action of
        Tick _ ->
            ( parse model.decoderStr
            , Cmd.none
            )
        JsonChange j ->
            ( { model | json = j }
            , Cmd.none
            )
        Decoder d ->
            ( parse d
            , Cmd.none
            )
        ChooseDecoder name ->
            let newModel =
                case Json.decodeString (emit model.ast (Call name [])) model.json of
                    Result.Ok r ->
                        { model
                        | decodeSuccess = True
                        , result =
                            "Success: decoder(s) & json match."
                            -- r
                        }
                    Result.Err e ->
                        { model
                        | decodeSuccess = False
                        , result = e
                        }
            in ( newModel , Cmd.none )
        OverlayMsg msg ->
            ( { model | overlay = Overlay.update msg model.overlay }
            , Cmd.none
            )

-- VIEW

view : Model -> Html Msg
view model =
    div
        [ style
            [ ("display", "flex")
            , ("flex-direction", "column")
            -- , ("height", "100%")
            , ("width", "100vw")
            , ("minHeight", "100vh")
            ]
        ]
        [ navbar model.overlay
        , Overlay.view model.overlay
            |> map OverlayMsg
        , mainSection model
        , footer
            [ style
                [ ( "display", "flex")
                ]
            ]
            [ parseResult model
            , decodeResult model
            ]
        ]

navbar m =
    header
        [ navbarStyles ]
        [ h1
            [ style
                [ ("margin", "0")
                , ("font-weight", "400" )
                , ("font-size", "22px")
                ]
            ]
            [ text <| if not m then "Elm Json.Decode interpreter" else "" ]
        , p
            [ style
                [ ("margin", "0") ]
            ]
            [ text "Simon Hampton ("
            , a
                [ href "https://github.com/simonh1000/json-interpreter"
                , style [ ("color", "inherit") ]
                ]
                [ text "Source: github" ]
            , text ")"
            ]
        ]

-- M A I N   S E C T I O N

mainSection : Model -> Html Msg
mainSection model =
    div
        [ style
            [ ( "display", "flex")
            , ( "flex-grow", "2" )
            ]
        ]
        [ div
            [ containerStyle elmGrey elmPale ]
            [ h2
                [ h2Styles elmPale ]
                [ text "Json.Decode functions" ]
            , textarea
                [ textareaStyles "#f7f7f7"
                -- , onchange Decoder
                , onInput Decoder
                ]
                [ text model.decoderStr ]
            ]
        , div
            [ containerStyle elmPale elmGrey ]
            [ h2
                [ h2Styles elmGrey ]
                [ text "Json" ]
            , textarea
                [ textareaStyles "#3a3432"
                -- , onchange address JsonChange
                , onInput JsonChange
                ]
                [ text model.json ]
                ]
        ]

parseResult model =
    if model.parseSuccess
    then
        div
            [ footerStyles elmGreen
            ]
            [ div
                [ style
                    [ ("display" , "flex")
                    , ("align-items", "baseline")
                    ]
                ]
                [ h3
                    [ style [("margin", "0")] ]
                    [ text "Choose entry point"
                    ]
                , div []
                    (List.map decoderButton model.ast)
                ]
            , div
                [ style [("font-size", "10px")]
                ]
                [ text <| toString model.ast ]
            ]
    else
        div
            [ if model.errorMessage /= ""
                then footerStyles elmRed
                else footerStyles "#f0ad00"
            ]
            [ p [] [ text model.errorMessage ]
            ]

decoderButton func =
    let n =
        case func of
            Proc name _ _ -> name
            otherwise -> "!!!!!"
    in
        span
            [ onClick (ChooseDecoder n)
            , fakeButton
            ]
            [ text n ]

decodeResult model =
    let col =
        if model.decodeSuccess
        then elmGreen
        else if model.result == ""
            then elmOrange
            else elmRed
    in
        div
            [ footerStyles col
            ]
            [ p
                [ style
                    [ ("margin", "0")
                    ]
                ]
                [ text model.result ]
            ]

escapeText str =
    String.fromList <|
        List.foldr
        (\c acc -> if c == '"' then '\\' :: '"' :: acc  else c :: acc )
        []
        (String.toList str)

-- STYLES
elmBlue = "#60B5CC"
elmGreen = "#7fd13b"
elmGrey = "#293C4B"   -- "rgb(41, 60, 75)"
elmPale = "#f7f7f7"
elmOrange = "#f0ad00"
elmRed = "#FF1965"

navbarStyles =
    style
        [ ("background-color", elmBlue)
        , ("color", elmGrey)
        , ("padding", "5px 15px")
        , ("display", "flex")
        , ("align-items", "baseline")
        , ("justify-content", "space-between")
        ]

containerStyle bkgCol col =
    style
        [ ("flex-grow", "1")
        , ("flex-basis", "0")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("background-color", bkgCol)
        , ("color", col)
        , ("padding", "0 15px 10px 15px")
        ]

h2Styles col =
    style
        [ ("background-color", "rgba(0,0,0,0.3)")
        , ("margin", "0 -15px")
        , ("color", col)
        , ("font-weight", "400")
        , ("padding", "5px 15px")
        , ("font-size", "18px")
        ]

textareaStyles col =
    style
        [ ("box-sizing", "border-box")
        , ("flex-grow", "100")
        , ("padding", "15px 0")
        , ("width", "100%")
        , ("resize", "none")
        , ("border", "0")
        , ("background-color", "inherit")
        , ("color", col)
        , ("font-family", "monospace")
        , ("font-size", "14px")
        , ("outline-width", "0")
        ]

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

fakeButton =
    style
        [ ("background-color", elmBlue )
        , ("padding", "5px 12px")
        , ("cursor", "pointer")
        , ("display", "inline-block")
        , ("margin", "0 10px 5px 10px")
        , ("border-radius", "5px")
        ]

-- on : String -> Decoder msg -> Attribute msg
-- onchange address action =
--     on "input" (Json.map action targetValue)
