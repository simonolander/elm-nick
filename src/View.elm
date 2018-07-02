module View exposing (..)

import Css exposing (..)
import Game exposing (getGameCharacterTop, getGameSize)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (style)
import Html.Styled.Events exposing (onClick)
import Model exposing (..)
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes
import Constants exposing (..)
import UI
import Util exposing (filterMaybe)
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

        vpw = gw
        vph = h / w * vpw

        gameToWindowCoordinate : GameCoordinate -> WindowCoordinate
        gameToWindowCoordinate gameCoordinate =
            let
                (GameCoordinate gx gy) = gameCoordinate
                wx = gx / gw * vpw
--                wy = (gh - gy) / gh * vph
                wy = vph - gy
            in
                WindowCoordinate wx wy

        footballs =
            game.footballs
            |> List.map (renderFootball gameToWindowCoordinate)

        dividers =
            List.range 0 (List.length game.characters)
            |> List.map (toFloat >> ((*) (boardWidth + 2 * boardMargin)) >> (renderDivider gameToWindowCoordinate))

        characters =
            game.characters
            |> List.map (renderCharacter gameToWindowCoordinate)

        elements =
            List.concat
                [ dividers
                , characters
                , footballs
                ]

        gameScene =
            Svg.Styled.svg
                [ Svg.Styled.Attributes.width (toString w)
                , Svg.Styled.Attributes.height (toString h)
                , Svg.Styled.Attributes.viewBox ("0 0 " ++ (toString vpw) ++ " " ++ (toString vph))
                ]
                elements

        pausedDiv =
            if game.gameState == Paused
            then
                UI.menu
                    [ UI.menuTitle [] "Paused"
                    , UI.btn [ onClick ResumeClicked] "Resume"
                    , UI.btn [ onClick MainMenuClicked] "Main Menu"
                    ]
                |> Just
            else Nothing

        scoreAndDroppedBalls =
            styled div
                [ position absolute
                , margin auto
                , textAlign center
                , left (px 0)
                , top (px 0)
                , right (px 0)
                , bottom (px 0)
                , fontSize xxLarge
                , textShadow3 (px 1) (px 2) (rgb 0 0 0)
                , fontWeight bold
                , fontFamily sansSerif
                , color (rgb 255 255 255)
                ]
                []
                (filterMaybe
                    [ Just <| styled h1
                        [ marginBottom (px 0) ] [] [ text (toString game.score) ]
                    , if game.numberOfDroppedFootballs > 0 then
                        Just <| styled h2
                            [ marginTop (px 8)
                            , color (rgb 255 150 150)
                            ]
                            []
                            [ text (toString game.numberOfDroppedFootballs) ]
                        else
                            Nothing
                    ])

    in
        div [] <|
            filterMaybe
                [ Just gameScene
                , Just scoreAndDroppedBalls
                , pausedDiv
                ]


renderFootball : (GameCoordinate -> WindowCoordinate) -> Football -> Svg.Styled.Svg msg
renderFootball g2w football =
    let
        (WindowCoordinate wcx wcy) = g2w (GameCoordinate football.x football.y)
        (WindowCoordinate wtlx wtly) = g2w (GameCoordinate (football.x - footballRadius) (football.y + footballRadius))
        (WindowCoordinate wbrx wbry) = g2w (GameCoordinate (football.x + footballRadius) (football.y - footballRadius))
        width = wbrx - wtlx
        height = wbry - wtly
        angle = football.r
        radius = width / 2

        footballImageSvg cx cy r rotation =
            Svg.Styled.image
                [ Svg.Styled.Attributes.x (toString (cx - r))
                , Svg.Styled.Attributes.y (toString (cy - r))
                , Svg.Styled.Attributes.width (toString (2 * r))
                , Svg.Styled.Attributes.height (toString (2 * r))
                , Svg.Styled.Attributes.transform ("rotate(" ++ (toString rotation) ++ ", " ++ (toString cx) ++ ", " ++ (toString cy) ++ ")")
                , Svg.Styled.Attributes.xlinkHref "/assets/football.png"
                , Svg.Styled.Attributes.imageRendering "pixelated"
                , Svg.Styled.Attributes.preserveAspectRatio "none"
                ]
                []

        result =
            if wbry < 0
            then
                let
                    cx =
                        wcx
                    outerRadius =
                        radius * 2
                    margin =
                        outerRadius * (sqrt 2 - 1) * 1.5
                    cy =
                        outerRadius + margin
                    d =
                        "M" ++ (toString cx) ++ "," ++ (toString margin) ++
                        "A" ++ (toString outerRadius) ++ " " ++ (toString outerRadius) ++ " 0 1 0 " ++ (toString (cx + outerRadius)) ++ " " ++ (toString cy) ++
                        "L" ++ (toString (cx + outerRadius)) ++ "," ++ (toString margin) ++
                        "Z"
                in
                    Svg.Styled.g
                        [ Svg.Styled.Attributes.transform ("rotate(-45, " ++ (toString cx) ++ ", " ++ (toString cy) ++ ")")
                        ]
                        [ Svg.Styled.path
                            [ Svg.Styled.Attributes.d d
                            , Svg.Styled.Attributes.fill "pink"
                            , Svg.Styled.Attributes.stroke "salmon"
                            , Svg.Styled.Attributes.strokeWidth "0.03"
                            ]
                            []
                        , footballImageSvg cx cy radius angle
                        ]

            else
                footballImageSvg wcx wcy radius angle

    in
        result



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
            , Svg.Styled.Attributes.preserveAspectRatio "none"
            ] []


renderDivider : (GameCoordinate -> WindowCoordinate) -> Float ->  Svg.Styled.Svg msg
renderDivider g2w x =
    let
        (WindowCoordinate wtlx wtly) =
            g2w (GameCoordinate (x - characterHeight / 2) characterHeight)

        (WindowCoordinate wbrx wbry) =
            g2w (GameCoordinate (x + characterHeight / 2) 0)

        width =
            wbrx - wtlx

        height =
            wbry - wtly
    in
        Svg.Styled.image
            [ Svg.Styled.Attributes.x (toString wtlx)
            , Svg.Styled.Attributes.y (toString wtly)
            , Svg.Styled.Attributes.width (toString width)
            , Svg.Styled.Attributes.height (toString height)
            , Svg.Styled.Attributes.xlinkHref "/assets/fence.png"
            , Svg.Styled.Attributes.imageRendering "pixelated"
            , Svg.Styled.Attributes.preserveAspectRatio "none"
            ] []


renderMenu : Menu -> Html Msg
renderMenu menu =
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
                , UI.btn [ onClick SinglePlayerFreeModeClicked] "Survival"
                , UI.btn [ onClick SinglePlayerFreeModeClicked] "Free Mode"
                , UI.btn [ onClick MainMenuClicked] "Main Menu"
                ]
        other -> UI.menu [ UI.menuTitle [] (toString other)]


renderMainMenu : Html Msg
renderMainMenu =
    UI.menu
        [ UI.menuTitle [] "Main Menu"
        , UI.btn [ onClick SinglePlayerMenuClicked] "Single Player"
        , UI.btn [ onClick SettingsClicked] "Settings"
        ]
