module View exposing (..)

import Array
import Css exposing (..)
import Game exposing (getGameCharacterTop, getGameSize)
import Game.View exposing (renderGame)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (type_, value, selected)
import Html.Styled.Events exposing (onClick, onInput)
import Model exposing (..)
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes
import Constants exposing (..)
import UI
import Util exposing (filterMaybe, goodKeys)
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
            |> Maybe.map (renderGame model.windowSize model.settings)
            |> Maybe.withDefault (div [] [])


        menuDiv =
            model.menu
            |> Maybe.map (renderMenu model.settings)
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


renderMenu : Settings -> Menu -> Html Msg
renderMenu settings menu =
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
                , UI.btn [ onClick (InitializeGame SinglePlayerSurvival)] "Survival"
                , UI.btn [ onClick (InitializeGame SinglePlayerFree)] "Free Mode"
                , UI.btn [ onClick MainMenuClicked] "Main Menu"
                ]

        MultiPlayerMenu ->
            UI.menu
                [ UI.menuTitle [] "Multiplayer"
                , styled div
                    [ displayFlex
                    , flexDirection row
                    , justifyContent spaceBetween
                    ]
                    []
                    [ select
                        [ onInput (\ intString -> UpdateNumberOfPlayers (Result.withDefault 2 (String.toInt intString))) ]
                        ( [ (2, "Two Players")
                          , (3, "Three Players")
                          , (4, "Four Players")
                          , (5, "Five Players")
                          , (6, "Six Players")
                          ]
                          |> List.map
                              (\(num, label) ->
                                  option
                                      [ value (toString num)
                                      , selected (num == settings.numberOfPlayers)
                                      ]
                                      [ text label ]
                              )

                        )
                    ]
                , styled div
                    [ displayFlex
                    , flexDirection column
                    , width (pct 100)
                    ]
                    []
                    (
                    settings.characterSettings
                    |> Array.toList
                    |> List.take settings.numberOfPlayers
                    |> List.indexedMap
                        (\index setting ->
                            styled div
                                [ displayFlex
                                , flexDirection row
                                , justifyContent spaceBetween
                                ]
                                []
                                [ select
                                    [ onInput (\ intString -> UpdatePlayerControl index Left (Result.withDefault 37 (String.toInt intString))) ]
                                    ( goodKeys
                                        |> List.map
                                            (\(num, label) ->
                                                option
                                                    [ value (toString num)
                                                    , selected (num == setting.leftKeyCode)
                                                    ]
                                                    [ text label ]
                                            )
                                    )
                                , select
                                    [ onInput (\ intString -> UpdatePlayerControl index Right (Result.withDefault 37 (String.toInt intString))) ]
                                    ( goodKeys
                                        |> List.map
                                            (\(num, label) ->
                                                option
                                                    [ value (toString num)
                                                    , selected (num == setting.rightKeyCode)
                                                    ]
                                                    [ text label ]
                                            )
                                    )
                                ]
                        )
                    )
                , UI.btn [ onClick (InitializeGame MultiplayerCooperation)] "Cooperation"
                , UI.btn [ onClick MainMenuClicked] "Main Menu"
                ]


renderMainMenu : Html Msg
renderMainMenu =
    UI.menu
        [ UI.menuTitle [] "Main Menu"
        , UI.btn [ onClick SinglePlayerMenuClicked] "Single Player"
        , UI.btn [ onClick MultiplayerMenuClicked] "Multiplayer"
        , UI.btn [ onClick SettingsClicked] "Settings"
        ]
