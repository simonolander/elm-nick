module View exposing (view)

import Css exposing (..)
import Game.View exposing (renderGame)
import Html.Styled exposing (..)
import Menu.View exposing (renderMenu)
import Model exposing (..)


view : Model -> Html Msg
view model =
    let
--        frameRateDiv =
--            styled div
--                [ position absolute
--                , left (px 0)
--                , top (px 0)
--                , backgroundColor (rgb 255 255 255)
--                ]
--                []
--                [ text (toString model.frameRate)
--                ]

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
            [ (width << px << toFloat) model.windowSize.width
            , (height << px << toFloat) model.windowSize.height
            , overflow hidden
            , backgroundImage (url "/assets/field.png")
            , backgroundRepeat noRepeat
            , backgroundSize cover
            ]
            []
            [ gameDiv
            , menuDiv
--            , frameRateDiv
            ]
