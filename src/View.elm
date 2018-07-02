module View exposing (..)

import Css exposing (..)
import Game exposing (getGameCharacterTop, getGameSize)
import Game.View exposing (renderGame)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (style)
import Html.Styled.Events exposing (onClick)
import Model exposing (..)
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes
import Constants exposing (..)
import UI
import Util exposing (filterMaybe)
import Window exposing (Size)

view : Model -> Html Msg
view model =
    let
        frameRateDiv =
            styled div
                [ position absolute
                , left (px 0)
                , top (px 0)
                , backgroundColor (rgb 255 255 255)
                ]
                []
                [ text (toString model.frameRate)
                ]

        gameDiv =
            model.game
            |> Maybe.map (renderGame model.windowSize)
            |> Maybe.withDefault (div [] [])


        menuDiv =
            model.menu
            |> Maybe.map renderMenu
            |> Maybe.withDefault (div [] [])
    in
        styled div
            [ width (pct 100)
            , height (pct 100)
            , overflow hidden
            , backgroundImage (url "/assets/field.png")
            , backgroundRepeat noRepeat
            , backgroundSize cover
            ]
            []
            [ gameDiv
            , menuDiv
            , frameRateDiv
            ]


renderMenu : Menu -> Html Msg
renderMenu menu =
    case menu of
        MainMenu ->
            renderMainMenu
        SettingsMenu ->
            UI.menu
                [ UI.menuTitle [] "Settings"
                , UI.btn [ onClick MainMenuClicked] "Ok"
                , UI.btn [ onClick MainMenuClicked] "Main Menu"
                ]
        SinglePlayerMenu ->
            UI.menu
                [ UI.menuTitle [] "Single Player"
                , UI.btn [ onClick SinglePlayerSurvivalModeClicked] "Survival"
                , UI.btn [ onClick SinglePlayerFreeModeClicked] "Free Mode"
                , UI.btn [ onClick MainMenuClicked] "Main Menu"
                ]
        other -> UI.menu [ UI.menuTitle [] (toString other)]


renderMainMenu : Html Msg
renderMainMenu =
    UI.menu
        [ UI.menuTitle [] "Main Menu"
        , UI.btn [ onClick SinglePlayerMenuClicked] "Single Player"
        , UI.btn [ onClick SettingsClicked] "Settings"
        ]
