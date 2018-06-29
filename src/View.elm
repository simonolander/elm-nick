module View exposing (..)

import Css exposing (..)
import Game exposing (getGameCharacterTop, getGameSize)
import Html.Styled exposing (Html, div, styled, text)
import Html.Styled.Attributes exposing (style)
import Html.Styled.Events exposing (onClick)
import Model exposing (..)
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes
import Constants exposing (..)
import UI
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


renderGame : Size -> Game -> Html Msg
renderGame windowSize game =
    let
        w = toFloat windowSize.width
        h = toFloat windowSize.height
        (GameSize gw gh) = getGameSize game

        gameToWindowCoordinate : GameCoordinate -> WindowCoordinate
        gameToWindowCoordinate gameCoordinate =
            let
                (GameCoordinate gx gy) = gameCoordinate
                wx = gx / gw * w
                wy = (gh - gy) / gh * h
            in
                WindowCoordinate wx wy

        footballs =
            game.footballs
            |> List.map (renderFootball gameToWindowCoordinate)

        characters =
            game.characters
            |> List.map (renderCharacter gameToWindowCoordinate)

        elements =
            List.concat
                [ characters
                , footballs
                ]

        gameScene =
            Svg.Styled.svg
                [ Svg.Styled.Attributes.width (toString w)
                , Svg.Styled.Attributes.height (toString h)
                , Svg.Styled.Attributes.viewBox ("0 0 " ++ (toString w) ++ " " ++ (toString h))
                ]
                elements

        result =
            if game.gameState == Paused
            then
                styled div
                    []
                    []
                    [ gameScene
                    , UI.menu
                        [ UI.menuTitle [] "Paused"
                        , UI.btn [ onClick OnResumeClicked] "Resume"
                        , UI.btn [ onClick OnMainMenuClicked] "Main Menu"
                        ]
                    ]
            else
                gameScene
    in
        result


renderFootball : (GameCoordinate -> WindowCoordinate) -> Football -> Svg.Styled.Svg msg
renderFootball g2w football =
    let
        (WindowCoordinate wcx wcy) = g2w (GameCoordinate football.x football.y)
        (WindowCoordinate wtlx wtly) = g2w (GameCoordinate (football.x - footballRadius) (football.y + footballRadius))
        (WindowCoordinate wbrx wbry) = g2w (GameCoordinate (football.x + footballRadius) (football.y - footballRadius))
        width = wbrx - wtlx
        height = wbry - wtly
        angle = football.r
    in
        Svg.Styled.image
            [ Svg.Styled.Attributes.x (toString wtlx)
            , Svg.Styled.Attributes.y (toString wtly)
            , Svg.Styled.Attributes.width (toString width)
            , Svg.Styled.Attributes.height (toString height)
            , Svg.Styled.Attributes.transform ("rotate(" ++ (toString football.r) ++ ", " ++ (toString wcx) ++ ", " ++ (toString wcy) ++ ")")
            , Svg.Styled.Attributes.xlinkHref "/assets/football.png"
            , Svg.Styled.Attributes.imageRendering "pixelated"
            ] []


renderCharacter : (GameCoordinate -> WindowCoordinate) -> Character -> Svg.Styled.Svg msg
renderCharacter g2w character =
    let
        (GameCoordinate gcx gty) = getGameCharacterTop character.boardIndex character.lane
        (WindowCoordinate wtlx wtly) = g2w (GameCoordinate (gcx - characterHeight / 2) (gty))
        (WindowCoordinate wbrx wbry) = g2w (GameCoordinate (gcx + characterHeight / 2) (gty - characterHeight))
        width = wbrx - wtlx
        height = wbry - wtly
        currentFrame = character.spriteAnimation.currentFrame
    in
        Svg.Styled.image
            [ Svg.Styled.Attributes.x (toString wtlx)
            , Svg.Styled.Attributes.y (toString wtly)
            , Svg.Styled.Attributes.width (toString width)
            , Svg.Styled.Attributes.height (toString height)
            , Svg.Styled.Attributes.xlinkHref currentFrame
            , Svg.Styled.Attributes.imageRendering "pixelated"
            ] []


renderMenu : Menu -> Html Msg
renderMenu menu =
    case menu of
        MainMenu ->
            renderMainMenu
        SettingsMenu ->
            UI.menu
                [ UI.menuTitle [] "Settings"
                , UI.btn [ onClick OnSettingsClicked] "Settings"
                , UI.btn [ onClick OnMainMenuClicked] "Ok"
                ]


renderMainMenu : Html Msg
renderMainMenu =
    UI.menu
        [ UI.menuTitle [] "Main Menu"
        , UI.btn [ onClick OnSettingsClicked] "Settings"
        , UI.btn [ onClick OnMainMenuClicked] "Main Menu"
        ]
