module Main exposing (..)

import Array exposing (Array)
import Html
import Html.Styled
import Keyboard
import Model exposing (..)
import Task exposing (perform)
import Window exposing (Size, resizes)
import AnimationFrame
import Update
import View
import PageVisibility

main : Program Never Model Msg
main = Html.program
    { init = init
    , update = Update.update
    , subscriptions = subscriptions
    , view = View.view >> Html.Styled.toUnstyled
    }


init : (Model, Cmd Msg)
init =
    let windowSize = Size 0 0
        menu = Just MainMenu
        characterSettings =
            Array.fromList
                [ { leftKeyCode = 37
                  , rightKeyCode = 39
                  }
                , { leftKeyCode = 16
                  , rightKeyCode = 17
                  }
                , { leftKeyCode = 18
                  , rightKeyCode = 91
                  }
                , { leftKeyCode = 49
                  , rightKeyCode = 50
                  }
                , { leftKeyCode = 78
                  , rightKeyCode = 77
                  }
                , { leftKeyCode = 79
                  , rightKeyCode = 80
                  }
                ]
        settings =
            { characterSettings = characterSettings
            , footballGenerationTime = 10.0
            , username = "Kattux"
            , numberOfPlayers = 2
            }
        model =
            { windowSize = windowSize
            , game = Nothing
            , frameRate = 0
            , menu = menu
            , settings = settings
            }
        cmd = Cmd.batch
            [ perform Resize Window.size
            ]
    in
        ( model
        , cmd
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animationSubscriptions : List (Sub Msg)
        animationSubscriptions =
            if model.game
                |> Maybe.map .gameState
                |> Maybe.map ((==) Running)
                |> Maybe.withDefault False
            then
                [ AnimationFrame.diffs Tick ]
            else
                []

        subs =
            List.concat
                [ [ resizes Resize
                  , Keyboard.downs KeyDown
                  , PageVisibility.visibilityChanges VisibilityChanged
                  ]
                , animationSubscriptions
                ]

    in
        Sub.batch subs
