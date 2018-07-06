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
import RemoteData exposing (RemoteData(Loading, NotAsked))
import Rest exposing (postScore)
import Util exposing (settingsToCharacters)


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

        MultiplayerMenuClicked ->
            ( { model | menu = Just MultiPlayerMenu }, Cmd.none)

        InitializeGame gameMode ->
            initializeGame gameMode model

        ReceiveScores webData ->
            let
                setScores webData game =
                    { game
                    | scoreboard = webData
                    }
            in
                ( { model
                  | game = Maybe.map (setScores webData) model.game
                  }
                , Cmd.none
                )

        UpdateUsername username ->
            let
                settings =
                    model.settings

                newSettings =
                    { settings
                    | username = username
                    }
            in
                ( { model
                  | settings = newSettings
                  }
                , Cmd.none
                )

        UpdateNumberOfPlayers numberOfPlayers ->
            let
                settings =
                    model.settings

                newSettings =
                    { settings
                    | numberOfPlayers = numberOfPlayers
                    }
            in
                ( { model
                  | settings = newSettings
                  }
                , Cmd.none
                )

        UpdatePlayerControl index lane keyCode ->
            let
                settings =
                    model.settings

                characterSettings = settings.characterSettings

                newCharacterSettings =
                    case Array.get index characterSettings of
                        Just setting ->
                            let
                                newSetting =
                                    case lane of
                                        Left ->
                                            { setting
                                            | leftKeyCode = keyCode
                                            }
                                        Right ->
                                            { setting
                                            | rightKeyCode = keyCode
                                            }
                            in
                                Array.set index newSetting characterSettings
                        Nothing ->
                            characterSettings

                newSettings =
                    { settings
                    | characterSettings = newCharacterSettings
                    }
            in
                ( { model
                  | settings = newSettings
                  }
                , Cmd.none
                )

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


initializeGame : GameMode -> Model -> (Model, Cmd Msg)
initializeGame gameMode model =
    let
        settings = model.settings
        footballs =
            []
        defaultLives =
            { max = 3
            , current = 0
            }
        lives =
            case gameMode of
                SinglePlayerFree ->
                    Nothing
                SinglePlayerSurvival ->
                    Just defaultLives
                MultiplayerCooperation ->
                    Just defaultLives
                MultiplayerFree ->
                    Nothing
                LastManStanding ->
                    Nothing

        firstCharacterSetting =
            Array.get 0 model.settings.characterSettings
            |> Maybe.withDefault
                { leftKeyCode = 37
                , rightKeyCode = 39
                }

        characters =
            case gameMode of
                SinglePlayerFree ->
                    settingsToCharacters 2 Nothing [firstCharacterSetting, firstCharacterSetting]
                SinglePlayerSurvival ->
                    settingsToCharacters 2 Nothing [firstCharacterSetting, firstCharacterSetting]
                MultiplayerCooperation ->
                    settingsToCharacters model.settings.numberOfPlayers Nothing (Array.toList model.settings.characterSettings)
                MultiplayerFree ->
                    settingsToCharacters model.settings.numberOfPlayers Nothing (Array.toList model.settings.characterSettings)
                LastManStanding ->
                    settingsToCharacters model.settings.numberOfPlayers (Just defaultLives) (Array.toList model.settings.characterSettings)

        game =
            { score = 0
            , footballs = footballs
            , characters = characters
            , gameState = Running
            , gameTime = 0
            , footballGenerationTime = settings.footballGenerationTime
            , remainingFootballGenerationTime = 3
            , numberOfDroppedFootballs = 0
            , gameMode = gameMode
            , lives = lives
            , scoreboard = NotAsked
            }
    in
        ( { model
          | menu = Nothing
          , game = Just game
          }
        , Cmd.none
        )
