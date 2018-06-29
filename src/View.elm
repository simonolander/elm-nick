module View exposing (..)

import Game exposing (getGameCharacterTop, getGameSize)
import Html exposing (Html, div)
import Model exposing (..)
import Svg
import Svg.Attributes
import Constants exposing (..)
import Html.Attributes exposing (style)
import Window exposing (Size)

view : Model -> Html Msg
view model =
    let
        windowWidth = model.windowSize.width
        windowHeight = model.windowSize.height
        windowAspect = toFloat windowWidth / toFloat windowHeight
        (width, height) =
            if windowAspect > aspect
            then (round (toFloat windowHeight * aspect), windowHeight)
            else (windowWidth, round (toFloat windowWidth / aspect))
        left =
            if windowAspect > aspect
            then toString ((windowWidth - width) // 2) ++ "px"
            else "0px"
        top =
            if windowAspect > aspect
            then "0px"
            else toString ((windowHeight - height) // 2) ++ "px"
    in
        div
            [ style
                [ ("width", "100%")
                , ("height", "100%")
                , ("overflow", "hidden")
                , ("background", "rgb(18, 43, 23)")
                , ("background", "url(/assets/field.png)")
                , ("background-repeat", "no-repeat")
                , ("background-size", "cover")
                ]
            ]
            [ renderGame model.windowSize model.game
            , div
                [ style
                    [ ("position", "absolute")
                    , ("left", "0")
                    , ("top", "0")
                    , ("background", "white")
                    ]
                ]
                [ Html.text (toString model.frameRate)
                ]
            ]


renderGame : Size -> Game -> Html msg
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
        footballs = game.footballs
            |> List.map (renderFootball gameToWindowCoordinate)
        characters = game.characters
            |> List.map (renderCharacter gameToWindowCoordinate)
        elements = List.concat
            [ characters
            , footballs
            ]
    in
        Svg.svg
            [ Svg.Attributes.width (toString w)
            , Svg.Attributes.height (toString h)
            , Svg.Attributes.viewBox ("0 0 " ++ (toString w) ++ " " ++ (toString h))
            ]
            elements


renderFootball : (GameCoordinate -> WindowCoordinate) -> Football -> Svg.Svg msg
renderFootball g2w football =
    let
        (WindowCoordinate wcx wcy) = g2w (GameCoordinate football.x football.y)
        (WindowCoordinate wtlx wtly) = g2w (GameCoordinate (football.x - footballRadius) (football.y + footballRadius))
        (WindowCoordinate wbrx wbry) = g2w (GameCoordinate (football.x + footballRadius) (football.y - footballRadius))
        width = wbrx - wtlx
        height = wbry - wtly
        angle = football.r
    in
        Svg.image
            [ Svg.Attributes.x (toString wtlx)
            , Svg.Attributes.y (toString wtly)
            , Svg.Attributes.width (toString width)
            , Svg.Attributes.height (toString height)
            , Svg.Attributes.transform ("rotate(" ++ (toString football.r) ++ ", " ++ (toString wcx) ++ ", " ++ (toString wcy) ++ ")")
            , Svg.Attributes.xlinkHref "/assets/football.png"
            , Svg.Attributes.imageRendering "pixelated"
            ] []


renderCharacter : (GameCoordinate -> WindowCoordinate) -> Character -> Svg.Svg msg
renderCharacter g2w character =
    let
        (GameCoordinate gcx gty) = getGameCharacterTop character.boardIndex character.lane
        (WindowCoordinate wtlx wtly) = g2w (GameCoordinate (gcx - characterHeight / 2) (gty))
        (WindowCoordinate wbrx wbry) = g2w (GameCoordinate (gcx + characterHeight / 2) (gty - characterHeight))
        width = wbrx - wtlx
        height = wbry - wtly
        currentFrame = character.spriteAnimation.currentFrame
    in
        Svg.image
            [ Svg.Attributes.x (toString wtlx)
            , Svg.Attributes.y (toString wtly)
            , Svg.Attributes.width (toString width)
            , Svg.Attributes.height (toString height)
            , Svg.Attributes.xlinkHref currentFrame
            , Svg.Attributes.imageRendering "pixelated"
            ] []
