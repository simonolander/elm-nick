module Settings.Update exposing
    ( updateUsername
    , updateNumberOfPlayers
    , updatePlayerControl
    )

import Array
import Keyboard exposing (KeyCode)
import Model exposing (..)


updateUsername : String -> Model -> (Model, Cmd msg)
updateUsername username model =
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


updateNumberOfPlayers : Int -> Model -> (Model, Cmd msg)
updateNumberOfPlayers numberOfPlayers model =
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


updatePlayerControl : Int -> Lane -> KeyCode -> Model -> (Model, Cmd Msg)
updatePlayerControl index lane keyCode model =
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
