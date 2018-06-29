module View exposing (..)

import Game exposing (getGameCharacterTop, getGameSize)
import Html.Styled exposing (Html, div, text)
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
                [ text (toString model.frameRate)
                ]
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
                div
                    []
                    [ gameScene
                    , div
                        [ style
                            [ ("position", "fixed")
                            , ("width", "100%")
                            , ("height", "100%")
                            , ("top", "0")
                            , ("left", "0")
                            , ("right", "0")
                            , ("bottom", "0")
                            , ("background-color", "rgba(0,0,0,0.5)")
                            , ("display", "flex")
                            , ("align-items", "center")
                            , ("justify-content", "center")
--                            , ("flex-direction", "column")
                            ]
                        ]
                        [ div
                            [ style
                                [ ("width", "50%")
                                , ("background-color", "rgba(255,255,255,0.5)")
                                , ("border-radius", "10px")
                                , ("padding", "10px")
                                , ("display", "flex")
                                , ("align-items", "center")
                                , ("justify-content", "center")
                                , ("flex-direction", "column")
                                ]
                            ]
                            [ UI.menuTitle [] "Paused"
                            , UI.btn [ onClick OnResumePressed] "Resume"
                            , UI.btn [] "Main Menu"
                            ]
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
