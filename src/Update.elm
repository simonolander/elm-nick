module Update exposing (..)

import Array
import Keyboard exposing (KeyCode)
import Model exposing (..)
import PageVisibility exposing (Visibility(Hidden))
import Time exposing (Time)
import Constants exposing (..)
import Game exposing (..)
import Game.Update exposing (..)
import Random


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Resize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

        Tick diff ->
            updateOnTick (diff / 1000) model

        KeyDown keyCode ->
            updateOnKeyDown keyCode model

        FootballGenerated football ->
            updateOnFootballGenerated football model

        VisibilityChanged visibility ->
            updateOnVisibilityChanged visibility model

        ResumeClicked ->
            updateOnResumeClicked model

        MainMenuClicked ->
            updateOnMainMenuClicked model

        SettingsClicked ->
            ( { model | menu = Just SettingsMenu }, Cmd.none)

        SinglePlayerMenuClicked ->
            ( { model | menu = Just SinglePlayerMenu }, Cmd.none)

        SinglePlayerFreeModeClicked ->
            updateOnSinglePlayerFreeModeClicked model

        SinglePlayerSurvivalModeClicked ->
            updateOnSinglePlayerSurvivalModeClicked model


updateOnTick : Time -> Model -> (Model, Cmd Msg)
updateOnTick diff model =
    let
        setFrameRate frameRate model =
            { model | frameRate = frameRate }

        setGame game model =
            { model | game = game }

        chain : (model -> field) -> (field -> model -> model) -> (field -> (field, Cmd msg)) -> (model, Cmd msg) -> (model, Cmd msg)
        chain getter setter transform (model, cmd) =
            let
                (newField, newCmd) =
                    transform (getter model)
            in
                (setter newField model, Cmd.batch [cmd, newCmd])

        updateGame : Time -> Maybe Game -> (Maybe Game, Cmd Msg)
        updateGame diff maybeGame =
            case maybeGame of
                Just game ->
                    let
                        (newGame, cmd) = updateGameOnTick diff game
                    in
                        (Just newGame, cmd)
                Nothing ->
                    (Nothing, Cmd.none)
    in
        (model, Cmd.none)
        |> chain .frameRate setFrameRate (always (diff, Cmd.none))
        |> chain .game setGame (updateGame diff)





updateOnKeyDown : KeyCode -> Model -> (Model, Cmd Msg)
updateOnKeyDown keyCode model =
    let
        newModel =
            { model
            | game = Maybe.map (updateGameOnKeyDown keyCode) model.game
            }
        cmd = Cmd.none
    in
        (newModel, cmd)


updateOnFootballGenerated : Football -> Model -> (Model, Cmd Msg)
updateOnFootballGenerated football model =
    ( { model
      | game = Maybe.map (updateGameOnFootballGenerated football) model.game
      }
    , Cmd.none
    )


updateOnVisibilityChanged : Visibility -> Model -> (Model, Cmd Msg)
updateOnVisibilityChanged visibility model =
    let
        game =
            Maybe.map (updateGameOnVisibilityChanged visibility) model.game
    in
        ( { model
          | game = game
          }
        , Cmd.none
        )


updateOnResumeClicked : Model -> (Model, Cmd msg)
updateOnResumeClicked model =
    let
        game =
            Maybe.map updateGameOnResumeClicked model.game
    in
        ( { model
          | game = game
          }
        , Cmd.none
        )


updateOnMainMenuClicked : Model -> (Model, Cmd msg)
updateOnMainMenuClicked model =
    ( { model
      | menu = Just MainMenu
      , game = Nothing
      }
    , Cmd.none
    )

updateOnSinglePlayerFreeModeClicked : Model -> (Model, Cmd Msg)
updateOnSinglePlayerFreeModeClicked model =
    let
        settings = model.settings
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
            , gameTime = 0
            , footballGenerationTime = settings.footballGenerationTime
            , remainingFootballGenerationTime = 2
            , numberOfDroppedFootballs = 0
            , gameMode = SinglePlayerFree
            }
        cmd = Cmd.batch
            [
            ]
    in
        ( { model
          | game = Just game
          , menu = Nothing
          }
        , cmd
        )

updateOnSinglePlayerSurvivalModeClicked : Model -> (Model, Cmd Msg)
updateOnSinglePlayerSurvivalModeClicked model =
    let
        settings = model.settings
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
            , gameTime = 0
            , footballGenerationTime = settings.footballGenerationTime
            , remainingFootballGenerationTime = 2
            , numberOfDroppedFootballs = 0
            , gameMode = SinglePlayerSurvival
            }
        cmd = Cmd.batch
            [
            ]
    in
        ( { model
          | game = Just game
          , menu = Nothing
          }
        , cmd
        )

