module Menu.Update exposing (updateMenu)

import Model exposing (..)
import RemoteData exposing (RemoteData(Loading))
import Rest exposing (getScores)


updateMenu : Menu -> Model -> (Model, Cmd Msg)
updateMenu menu model =
    case menu of
        HighscoreMenu gameMode _ ->
            ( { model
              | menu = Just (HighscoreMenu gameMode Loading)
              , game = Nothing
              }
            , getScores gameMode
            )

        menu ->
            ( { model
              | menu = Just menu
              , game = Nothing
              }
            , Cmd.none
            )
