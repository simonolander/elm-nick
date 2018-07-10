module View exposing (view)

import Css exposing (..)
import Game.View exposing (renderGame)
import Html.Styled exposing (..)
import Menu.View exposing (renderMenu)
import Model exposing (..)


view : Model -> Html Msg
view model =
    let
        gameDiv =
            model.game
            |> Maybe.map (renderGame model.windowSize model.settings)
            |> Maybe.withDefault (text "")


        menuDiv =
            model.menu
            |> Maybe.map (renderMenu model.settings)
            |> Maybe.withDefault (text "")
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
            ]
