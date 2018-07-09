module Menu.View exposing (renderMenu)

import Array
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (type_, value, selected, title)
import Html.Styled.Events exposing (onClick, onInput)
import Model exposing (..)
import RemoteData exposing (..)
import UI
import Util exposing (filterMaybe, gameModeToString, goodKeys)


renderMenu : Settings -> Menu -> Html Msg
renderMenu settings menu =
    case menu of
        MainMenu ->
            renderMainMenu
        SettingsMenu ->
            UI.menu
                [ UI.menuTitle [] "Settings"
                , UI.btn [ onClick (MenuNavigation MainMenu)] "Ok"
                , UI.btn [ onClick (MenuNavigation MainMenu)] "Main Menu"
                ]
        SinglePlayerMenu ->
            UI.menu
                [ UI.menuTitle [] "Single Player"
                , styled div
                    [ width (pct 100)
                    , color (rgb 255 255 255)
                    , textShadow3 (px 1) (px 2) (rgb 0 0 0)
                    , fontWeight bold
                    , fontFamily sansSerif
                    ]
                    []
                    [ text "Left and right controls" ]
                , styled div
                    [ displayFlex
                    , flexDirection row
                    , justifyContent spaceBetween
                    , width (pct 100)
                    ]
                    []
                    [ styled select
                        [ fontSize (px 18)
                        , color (rgb 51 51 51)
                        , border3 (px 1) solid (rgb 191 191 191)
                        , borderRadius (px 8)
                        , padding2 (px 5) (px 10)
                        , height (px 48)
                        , backgroundColor (rgb 255 255 255)
                        , boxShadow6 inset (px 0) (px 3) (px 0) (px 0) (rgb 235 235 235)
                        , flex (int 1)
                        , marginRight (px 8)
                        ]
                        [ onInput (\ intString -> UpdatePlayerControl 0 Left (Result.withDefault 37 (String.toInt intString)))
                        , title ("Left key for Player 1")
                        ]
                        ( goodKeys
                            |> List.map
                                (\(num, label) ->
                                    option
                                        [ value (toString num)
                                        , selected (Array.get 0 settings.characterSettings |> Maybe.map (.leftKeyCode) |> Maybe.map ((==) num) |> Maybe.withDefault False)
                                        ]
                                        [ text label ]
                                )
                        )
                    , styled select
                        [ fontSize (px 18)
                        , color (rgb 51 51 51)
                        , border3 (px 1) solid (rgb 191 191 191)
                        , borderRadius (px 8)
                        , padding2 (px 5) (px 10)
                        , height (px 48)
                        , backgroundColor (rgb 255 255 255)
                        , boxShadow6 inset (px 0) (px 3) (px 0) (px 0) (rgb 235 235 235)
                        , flex (int 1)
                        ]
                        [ onInput (\ intString -> UpdatePlayerControl 0 Right (Result.withDefault 39 (String.toInt intString)))
                        , title ("Right key for Player 1")
                        ]
                        ( goodKeys
                            |> List.map
                                (\(num, label) ->
                                    option
                                        [ value (toString num)
                                        , selected (Array.get 0 settings.characterSettings |> Maybe.map (.rightKeyCode) |> Maybe.map ((==) num) |> Maybe.withDefault False)
                                        ]
                                        [ text label ]
                                )
                        )
                    ]
                , styled div [ height (px 20) ] [] []
                , UI.btn [ onClick (InitializeGame SinglePlayerSurvival)] "Survival"
                , UI.btn [ onClick (InitializeGame SinglePlayerFree)] "Free Mode"
                , styled div [ height (px 20) ] [] []
                , UI.btn [ onClick (MenuNavigation MainMenu)] "Main Menu"
                ]

        MultiplayerMenu ->
            UI.menu
                [ UI.menuTitle [] "Multiplayer"
                , styled div
                    [ width (pct 100)
                    , color (rgb 255 255 255)
                    , textShadow3 (px 1) (px 2) (rgb 0 0 0)
                    , fontWeight bold
                    , fontFamily sansSerif
                    ]
                    []
                    [ text "Number of players" ]
                , styled div
                    [ displayFlex
                    , flexDirection row
                    , justifyContent spaceBetween
                    , width (pct 100)
                    ]
                    []
                    [ styled select
                        [ fontSize (px 18)
                        , color (rgb 51 51 51)
                        , border3 (px 1) solid (rgb 191 191 191)
                        , borderRadius (px 8)
                        , padding2 (px 5) (px 10)
                        , height (px 48)
                        , backgroundColor (rgb 255 255 255)
                        , boxShadow6 inset (px 0) (px 3) (px 0) (px 0) (rgb 235 235 235)
                        , marginBottom (px 4)
                        , flex (int 1)
                        ]
                        [ onInput (\ intString -> UpdateNumberOfPlayers (Result.withDefault 2 (String.toInt intString)))
                        , title "Number of players"
                        ]
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
                    [ width (pct 100)
                    , color (rgb 255 255 255)
                    , textShadow3 (px 1) (px 2) (rgb 0 0 0)
                    , fontWeight bold
                    , fontFamily sansSerif
                    , marginTop (px 8)
                    ]
                    []
                    [ text "Left and right controls" ]
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
                                , marginBottom (px 4)
                                ]
                                []
                                [ styled select
                                    [ fontSize (px 18)
                                    , color (rgb 51 51 51)
                                    , border3 (px 1) solid (rgb 191 191 191)
                                    , borderRadius (px 8)
                                    , padding2 (px 5) (px 10)
                                    , height (px 48)
                                    , backgroundColor (rgb 255 255 255)
                                    , boxShadow6 inset (px 0) (px 3) (px 0) (px 0) (rgb 235 235 235)
                                    , flex (int 1)
                                    , marginRight (px 8)
                                    ]
                                    [ onInput (\ intString -> UpdatePlayerControl index Left (Result.withDefault 37 (String.toInt intString)))
                                    , title ("Left key for Player " ++ toString (index + 1))
                                    ]
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
                                , styled select
                                    [ fontSize (px 18)
                                    , color (rgb 51 51 51)
                                    , border3 (px 1) solid (rgb 191 191 191)
                                    , borderRadius (px 8)
                                    , padding2 (px 5) (px 10)
                                    , height (px 48)
                                    , backgroundColor (rgb 255 255 255)
                                    , boxShadow6 inset (px 0) (px 3) (px 0) (px 0) (rgb 235 235 235)
                                    , flex (int 1)
                                    ]
                                    [ onInput (\ intString -> UpdatePlayerControl index Right (Result.withDefault 39 (String.toInt intString)))
                                    , title ("Right key for Player " ++ toString (index + 1))
                                    ]
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
                , styled div [ height (px 20) ] [] []
                , UI.btn [ onClick (InitializeGame MultiplayerCooperation)] "Cooperation"
                , UI.btn [ onClick (InitializeGame LastManStanding)] "Last Man Standing"
                , UI.btn [ onClick (InitializeGame MultiplayerFree)] "Free Mode"
                , styled div [ height (px 20) ] [] []
                , UI.btn [ onClick (MenuNavigation MainMenu)] "Back to Main Menu"
                ]

        SelectHighscoreMenu ->
            UI.menu
                [ UI.menuTitle [] "Highscores"
                , UI.btn [ onClick (MenuNavigation (HighscoreMenu SinglePlayerSurvival NotAsked)) ] "Single Player Survival"
                , UI.btn [ onClick (MenuNavigation (HighscoreMenu MultiplayerCooperation NotAsked)) ] "Multiplayer Cooperation"
                , styled div [ height (px 20) ] [] []
                , UI.btn [ onClick (MenuNavigation MainMenu)] "Main Menu"
                ]

        HighscoreMenu gameMode webData ->
            renderHighscoresMenu gameMode webData


renderHighscoresMenu : GameMode -> WebData (List Score) -> Html Msg
renderHighscoresMenu gameMode webData =
    UI.menu
        [ UI.menuTitle [] (gameModeToString gameMode)
        , UI.scoreboard webData
        , styled div [ height (px 20) ] [] []
        , UI.btn [ onClick (MenuNavigation SelectHighscoreMenu)] "Back to Highscores"
        ]


renderMainMenu : Html Msg
renderMainMenu =
    UI.menu
        [ UI.menuTitle [] "Main Menu"
        , UI.btn [ onClick (MenuNavigation SinglePlayerMenu)] "Single Player"
        , UI.btn [ onClick (MenuNavigation MultiplayerMenu)] "Multiplayer"
        , styled div [ height (px 20) ] [] []
        , UI.btn [ onClick (MenuNavigation SelectHighscoreMenu)] "Highscores"
        , styled div [ height (px 20) ] [] []
        , styled div
            [ width (pct 100)
            , textAlign right
            , fontFamily monospace
            , color (rgb 220 220 220)
            ]
            []
            [ span
                []
                [ text "@ Simon Olander Sahlén" ]
            , br [] []
            , a
                [ Html.Styled.Attributes.href "https://github.com/simonolander/elm-nick"
                , Html.Styled.Attributes.target "_blank"
                ]
                [ text "Source code" ]
            ]
--        , UI.btn [ onClick (MenuNavigation SettingsMenu)] "Settings"
        ]

