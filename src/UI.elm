module UI exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)

theme : { secondary : Color, primary : Color }
theme =
    { primary = hex "146aff"
    , secondary = hex "004cc9"
    }

btn : List (Attribute msg) -> String -> Html msg
btn attributes content =
    styled
        button
            [ hover
                [ backgroundColor theme.secondary
                ]
            , focus
                [ backgroundColor theme.secondary
                ]
            , width (pct 100)
            , fontSize large
            , color (rgb 255 255 255)
            , borderRadius (px 8)
            , cursor pointer
            , height (px 60)
            , lineHeight (px 57)
            , fontWeight bold
            , backgroundColor theme.primary
            , padding2 (px 0) (px 16)
            , border3 (px 1) solid theme.primary
            , borderBottom3 (px 3) solid theme.secondary
            , display inlineBlock
            , textAlign center
            , textDecoration none
            , backgroundImage none
            , textShadow none
            , marginTop (px 8)
            ]
        attributes
        [ text content
        ]


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
