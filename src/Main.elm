module Main (main) where

import StartApp
import Effects exposing (Effects)
import Task

import App exposing (Action(..), init, update, view)

app =
    StartApp.start
        { init = (init, Effects.tick Tick)
        , update = update
        , view = view
        , inputs = []
        }

main =
    app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
