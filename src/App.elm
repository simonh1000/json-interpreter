module App (Model, Action(Tick), init, update, view) where

import Html exposing (Html, h1, h2, h3, text, div, a, header, footer, p, button, textarea, span)
import Html.Attributes as Attr exposing (type', id, style, multiple, href)
import Html.Events exposing (onClick, on, onSubmit, onWithOptions, targetValue)

import Effects exposing (Effects)
import Time exposing (Time)
import String
import Json.Decode as Json

import Parser exposing (parseString)
import AST exposing (Command(..))
import Emit exposing (..)

-- MODEL

type alias Model =
    { json : String
    , decoderStr : String
    , ast : List Command
    , parseSuccess : Bool
    , result : String
    , errorMessage: String
    , decodeSuccess : Bool
    }

init =
    -- { json = "{\"simon\": \"Success!!!\"}"
    -- { json = "{\"simon\": \"Success!!\", \"test\": \"Unbelievable\"}"
    { json = "{\"outer\": {\"inner\": \"Success\"}}"
    -- { json = "{\"array\": [4,5]}"
    -- { json = "[1,2,3,4,5]"
    -- , decoderStr = "f1 = (\"simon\" := string)"
    -- , decoderStr = "\"outer\" := (\"inner\" := string)"
    -- , decoderStr = "f1 = object2 \n\tinit2\n\t(\"simon\" := string)\n\t(\"test\" := string)"
    -- , decoderStr = "f1 = at \n\t[\"outer\"] \n\t(object1 dummy (\"inner\" := string))"
    -- , decoderStr = "f1 s = s := string\nf2 = \"outer\" := f1 \"inner\""
    , decoderStr = "f1 s = \"inner\" := s\nf2 = \"outer\" := f1 string"
    -- , decoderStr = "f1 = \"array\" := (list int)"
    -- , decoderStr = "func = \"array\" := (tuple2 (\\a b -> [a,b]) int int)"
    -- , decoderStr = "func = tuple5 comb int int int int int"
    , result = ""
    , ast = []
    , parseSuccess = False
    , errorMessage = ""
    , decodeSuccess = False
    }

-- UPDATE

type Action
    = JsonChange String
    | Decoder String
    | ChooseDecoder String
    | Tick Time

update : Action -> Model -> (Model, Effects Action)
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
            , Effects.none
            )
        JsonChange j ->
            ( { model | json = j }
            , Effects.none
            )
        Decoder d ->
            ( parse d
            , Effects.none
            )
        ChooseDecoder name ->
            let
                newModel =
                    case Json.decodeString (emit model.ast (Call name [])) model.json of
                        Result.Ok r ->
                            { model
                            | decodeSuccess = True
                            , result =
                                "Decode success: i.e. your json seems to be compatible with your decode functions"
                            }
                        Result.Err e ->
                            { model
                            | decodeSuccess = False
                            , result = "Decode error: " ++ e
                            }
            in ( newModel , Effects.none )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ style
            [ ("display", "flex")
            , ("flex-direction", "column")
            , ("height", "100%")
            ]
        ]
        [ navbar
        , mainSection address model
        , footer
            [ style
                [ ( "display", "flex")
                ]
            ]
            [ parseResult address model
            , decodeResult model
            ]
        ]

navbar =
    header
        [ navbarStyles ]
        [ h1
            [ style
                [ ("margin", "0")
                , ("font-weight", "400" )
                , ("font-size", "22px")
                ]
            ]
            [ text "Elm Json decoding tester" ]
        , p []
            [ text "Simon Hampton ("
            , a
                [ href "https://github.com/simonh1000/json-interpreter"
                , style [ ("color", "inherit")]]
                [ text "Source: github" ]
            , text ")"
            ]
        ]

mainSection address model =
    div
        [ style
            [ ( "display", "flex")
            , ("flex-grow", "2")
            ]
        ]
        [ div
            [ containerStyle elmGrey elmPale ]
            [ h2
                [ h2Styles elmPale ]
                [ text "Json.Decode functions" ]
            , textarea
                [ textareaStyles "#f7f7f7"
                , onchange address Decoder
                ]
                [ text model.decoderStr ]
            , p [] [ text "Sorry: I cannot yet parse <| or |>, and nor does tuple check array length"]
            ]
        , div
            [ containerStyle elmPale elmGrey ]
            [ h2
                [ h2Styles elmGrey ]
                [ text "Json" ]
            , textarea
                [ textareaStyles "#3a3432"
                , onchange address JsonChange
                ]
                [ text model.json ]
                ]
        ]

parseResult address model =
    if model.parseSuccess
    then
        div
            [ footerStyles elmGreen
            ] <|
            [ h3
                [ style [("margin", "0")] ]
                [ --text ""
                text "Choose entry point"
                ]
            , div []
                (List.map (decoderButton address) model.ast)
            , div
                [ style [("font-size", "10px")]
                ]
                [ text <| toString model.ast ]
            ]
    else
        div
            [ if model.errorMessage /= ""
                then footerStyles "red"
                else footerStyles "#f0ad00"
            ]
            [ p [] [ text model.errorMessage ]
            ]

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

decodeResult model =
    let col =
        if model.decodeSuccess
        then elmGreen
        else if model.result == ""
            then elmOrange
            else "red"
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

elmGreen = "#7fd13b"
elmBlue = "#60B5CC"
elmGrey = "#293C4B"   -- "rgb(41, 60, 75)"
elmPale = "#f7f7f7"
elmOrange = "#f0ad00"

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
        , ("display", "flex")
        , ("flex-wrap", "wrap")
        , ("align-items", "center")
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

-- on (String -> Json.Decode.Decoder a -> a -> Signal.Message) -> Attribute
onchange address action =
    on "input" targetValue (\v -> Signal.message address (action v))
