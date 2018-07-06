module UI exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onInput)
import Model exposing (Msg(UpdateUsername), Score)
import RemoteData exposing (WebData, RemoteData(..))

primaryTheme : Color
primaryTheme =
    hex "146aff"

secondaryTheme : Color
secondaryTheme =
    hex "004cc9"

btn : List (Attribute msg) -> String -> Html msg
btn attributes content =
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
        [ text content
        ]


menuTitle : List (Attribute msg) -> String -> Html msg
menuTitle attributes content =
    styled
        h1
            [ fontSize xxLarge
            , color (rgb 255 255 255)
            , textShadow3 (px 1) (px 2) (rgb 0 0 0)
            , fontWeight bold
            , fontFamily sansSerif
            ]
        attributes
        [ text content
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
--            , justifyContent center
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
        , backgroundColor (rgba 200 200 255 0.5)
        , border3 (px 1) solid (rgb 100 100 255)
        , boxShadow6 inset (px 0) (px 3) (px 0) (px 0) (rgb 100 100 255)
        , displayFlex
        , overflow auto
        , flexDirection column
        , justifyContent flexStart
        , fontSize xLarge
        , color (rgb 255 255 255)
        , textShadow3 (px 1) (px 2) (rgb 0 0 0)
        , fontWeight bold
        , fontFamily sansSerif
        ]
        []
        ( case webData of
            NotAsked ->
                [ styled div
                    [ displayFlex
                    , overflow auto
                    , justifyContent spaceBetween
                    , flexDirection row
                    , flexShrink (int 0)
                    ]
                    []
                    [ input [ onInput UpdateUsername ] []
                    , btn [] "Send"
                    ]
                ]
            Loading ->
                [ styled span
                    [textAlign center]
                    []
                    [text "Loading scores..."]
                ]
            Success scores ->
                scores
                |> List.map
                    ( \score ->
                        styled div
                            [ displayFlex
                            , overflow auto
                            , justifyContent spaceBetween
                            , flexDirection row
                            , flexShrink (int 0)
                            ]
                            []
                            [ span [] [text score.username]
                            , span [] [text (toString score.score)]
                            ]
                    )
            Failure error ->
                [ styled span
                    [textAlign center]
                    []
                    [text ("Could not load scores: " ++ toString error)]
                ]
        )
