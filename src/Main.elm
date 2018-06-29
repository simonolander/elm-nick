module Main exposing (..)

import Array exposing (Array)
import Html
import Html.Styled
import Keyboard
import Model exposing (..)
import Task exposing (perform)
import Window exposing (Size, resizes)
import AnimationFrame
import Constants exposing (..)
import Game exposing (..)
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
        footballs =
            []
        character =
            { lane = Left
            , leftKeyCode = 37
            , rightKeyCode = 39
            , spriteAnimation = characterIdle
            , boardIndex = 0
            }
        characters =
            [ character
            , character
            ] |> List.indexedMap (\ index character -> { character | boardIndex = index })
        game =
            { score = 0
            , footballs = footballs
            , characters = characters
            , gameState = Running
            }
        menu = Just MainMenu
        model =
            { windowSize = windowSize
            , game = Nothing
            , frameRate = 0
            , menu = menu
            }
        cmd = Cmd.batch
            [ perform Resize Window.size
            , Update.generateFootball 2.0 (GameCoordinate 0 0) game
            , Update.generateFootball 2.2 (GameCoordinate 0 0) game
            , Update.generateFootball 2.4 (GameCoordinate 0 0) game
            , Update.generateFootball 2.6 (GameCoordinate 0 0) game
            , Update.generateFootball 2.8 (GameCoordinate 0 0) game
            , Update.generateFootball 3.0 (GameCoordinate 0 0) game
            , Update.generateFootball 3.2 (GameCoordinate 0 0) game
            , Update.generateFootball 3.4 (GameCoordinate 0 0) game
            , Update.generateFootball 3.6 (GameCoordinate 0 0) game
            ]
    in
        ( model
        , cmd
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        consIf : Bool -> a -> List a -> List a
        consIf cond value =
            if cond then (::) value else identity

        subs =
            [ resizes Resize
            , Keyboard.downs KeyDown
            , PageVisibility.visibilityChanges VisibilityChanged
            ]
            |> consIf (model.game |> Maybe.map (((==) Running) << (.gameState)) |> Maybe.withDefault False) (AnimationFrame.diffs Tick)
    in
    Sub.batch subs
