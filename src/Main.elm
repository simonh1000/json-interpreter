module Main exposing (main)

import Html.App as Html
import Platform.Cmd exposing (Cmd)
import Task
import Time

import App exposing (Msg(..), init, update, view)
-- import Testing exposing (Msg(..), init, update, view)

main =
    Html.program
        { init = (init, Task.perform Tick Tick Time.now)
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
