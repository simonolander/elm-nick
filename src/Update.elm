module Update exposing (..)

import Array
import Keyboard exposing (KeyCode)
import Menu.Update exposing (updateMenu)
import Model exposing (..)
import PageVisibility exposing (Visibility(Hidden))
import Settings.Update exposing (..)
import Time exposing (Time)
import Constants exposing (..)
import Game.Util exposing (..)
import Game.Update exposing (..)
import Random
import RemoteData exposing (RemoteData(Loading, NotAsked))
import Rest exposing (getScores, postScore)
import Util exposing (batch, chain)


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

        MenuNavigation menu ->
            updateMenu menu model

        InitializeGame gameMode ->
            initializeGame gameMode model

        ReceiveScores webData ->
            let
                setScoresGame webData game =
                    { game
                    | scoreboard = webData
                    }

                setScoresMenu webData menu =
                    case menu of
                        HighscoreMenu gameMode _ ->
                            HighscoreMenu gameMode webData

                        default ->
                            default
            in
                ( { model
                  | game = Maybe.map (setScoresGame webData) model.game
                  , menu = Maybe.map (setScoresMenu webData) model.menu
                  }
                , Cmd.none
                )

        UpdateUsername username ->
            updateUsername username model

        UpdateNumberOfPlayers numberOfPlayers ->
            updateNumberOfPlayers numberOfPlayers model

        UpdatePlayerControl index lane keyCode ->
            updatePlayerControl index lane keyCode model

        PostScore gameMode score ->
            let
                setScores webData game =
                    { game
                    | scoreboard = webData
                    }
            in
                ( { model
                  | game = Maybe.map (setScores Loading) model.game
                  }
                , postScore gameMode score
                )


updateOnTick : Time -> Model -> (Model, Cmd Msg)
updateOnTick diff model =
    let
        updateGame : Time -> Model -> (Model, List (Cmd Msg))
        updateGame diff model =
            case model.game of
                Just game ->
                    let
                        (newGame, cmd) = updateGameOnTick diff game
                    in
                        ( { model
                          | game = Just newGame
                          }
                        , [cmd]
                        )
                Nothing ->
                    ( model, [] )

        newModel =
            { model
            | frameRate = diff
            }
    in
        (newModel, [])
        |> chain (updateGame diff)
        |> batch


updateOnKeyDown : KeyCode -> Model -> (Model, Cmd Msg)
updateOnKeyDown keyCode model =
    ( { model
      | game = Maybe.map (updateGameOnKeyDown keyCode) model.game
      }
    , Cmd.none
    )


updateOnFootballGenerated : Football -> Model -> (Model, Cmd Msg)
updateOnFootballGenerated football model =
    ( { model
      | game = Maybe.map (updateGameOnFootballGenerated football) model.game
      }
    , Cmd.none
    )


updateOnVisibilityChanged : Visibility -> Model -> (Model, Cmd Msg)
updateOnVisibilityChanged visibility model =
    ( { model
      | game = Maybe.map (updateGameOnVisibilityChanged visibility) model.game
      }
    , Cmd.none
    )


updateOnResumeClicked : Model -> (Model, Cmd msg)
updateOnResumeClicked model =
    ( { model
      | game = Maybe.map updateGameOnResumeClicked model.game
      }
    , Cmd.none
    )
