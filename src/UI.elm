module UI exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onInput)
import Html.Styled.Attributes exposing (title, value, selected)
import Model exposing (..)
import RemoteData exposing (WebData, RemoteData(..))
import Constants exposing (goodKeys)

primaryTheme : Color
primaryTheme =
    hex "146aff"

secondaryTheme : Color
secondaryTheme =
    hex "004cc9"

btn : List (Attribute msg) -> String -> Html msg
btn attributes buttonText =
    styled
        button
            [ hover
                [ backgroundColor secondaryTheme
                ]
            , width (pct 100)
            , fontSize large
            , color (rgb 255 255 255)
            , borderRadius (px 8)
            , cursor pointer
            , height (px 60)
            , lineHeight (px 57)
            , fontWeight bold
            , backgroundColor primaryTheme
            , padding2 (px 0) (px 16)
            , border3 (px 1) solid primaryTheme
            , borderBottom3 (px 3) solid secondaryTheme
            , display inlineBlock
            , flexShrink (int 0)
            , textAlign center
            , textDecoration none
            , backgroundImage none
            , textShadow none
            , marginTop (px 8)
            ]
        attributes
        [ text buttonText
        ]


menuTitle : String -> Html msg
menuTitle menuTitleText =
    styled
        h1
            [ fontSize xxLarge
            , color (rgb 255 255 255)
            , textShadow3 (px 1) (px 2) (rgb 0 0 0)
            , fontWeight bold
            , fontFamily sansSerif
            ]
        []
        [ text menuTitleText
        ]


menu : List (Html msg) -> Html msg
menu content =
    styled div
        [ position fixed
        , width (pct 100)
        , height (pct 100)
        , top (px 0)
        , left (px 0)
        , right (px 0)
        , bottom (px 0)
        , backgroundColor (rgba 0 0 0 0.5)
        , displayFlex
        , alignItems center
        , justifyContent center
        ]
        []
        [ styled div
            [ minWidth (pct 30)
            , maxHeight (pct 80)
            , backgroundColor (rgba 255 255 255 0.5)
            , borderRadius (px 10)
            , padding (px 10)
            , displayFlex
            , overflow auto
            , alignItems center
            , flexDirection column
            ]
            []
            content
        ]


scoreboard : WebData (List Score) -> Html Msg
scoreboard webData =
    styled div
        [ width (pct 100)
        , maxHeight (pct 50)
        , minHeight (px 55)
        , borderRadius (px 10)
        , padding (px 10)
        , boxSizing borderBox
        , backgroundColor (rgba 255 255 255 1.0)
        , border3 (px 1) solid (rgb 191 191 191)
        , boxShadow6 inset (px 0) (px 3) (px 0) (px 0) (rgb 235 235 235)
        , overflow auto
        , fontSize xLarge
        , color (rgb 80 80 80)
        , fontWeight bold
        , fontFamily sansSerif
        ]
        []
        ( case webData of
            NotAsked ->
                [ styled div
                    [ textAlign center
                    , width (pct 100)
                    ]
                    []
                    [text "Not even asked"]
                ]
            Loading ->
                [ styled div
                    [ textAlign center
                    , width (pct 100)
                    ]
                    []
                    [text "Loading scores..."]
                ]
            Success scores ->
                scores
                |> List.indexedMap
                    ( \index score ->
                        styled div
                            [ minWidth (pct 100)
                            , display Css.table
                            , tableLayout fixed
                            , borderBottom3 (px 1) solid (rgb 200 200 200)
                            , paddingBottom (px 8)
                            , paddingTop (px 8)
                            , lastChild
                                [ borderBottom (px 0)
                                ]
                            , hover
                                [ backgroundColor (rgb 235 235 235)
                                ]
                            ]
                            []
                            [ styled div
                                [ display tableCell
                                , verticalAlign middle
                                , width (pct 15)
                                ]
                                []
                                [text (toString (index + 1) ++ ".")]
                            , styled div
                                [ display tableCell
                                , verticalAlign middle
                                , width (pct 60)
                                ]
                                []
                                [text score.username]
                            , styled div
                                [ display tableCell
                                , verticalAlign middle
                                , width (pct 25)
                                , textAlign right
                                ]
                                []
                                [text (toString score.score)]
                            ]
                    )
            Failure error ->
                [ styled span
                    [textAlign center]
                    []
                    [text ("Could not load scores: " ++ toString error)]
                ]
        )


leftRightControls : List CharacterSetting -> Html Msg
leftRightControls characterSettings =
    styled div
        [ displayFlex
        , flexDirection column
        , flexShrink (int 0)
        , width (pct 100)
        ]
        []
        (
        characterSettings
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


label : String -> Html msg
label labelText =
    styled div
        [ width (pct 100)
        , color (rgb 255 255 255)
        , textShadow3 (px 1) (px 2) (rgb 0 0 0)
        , fontWeight bold
        , flexShrink (int 0)
        , fontFamily sansSerif
        ]
        []
        [ text labelText ]
