module Game.View exposing (renderGame)

import Constants exposing (..)
import Css exposing (..)
import Game exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (value, type_, action)
import Model exposing (..)
import RemoteData exposing (RemoteData(NotAsked))
import Svg.Styled
import Svg.Styled.Attributes
import UI
import Util exposing (filterMaybe, keyCodeToString)
import Window exposing (Size)


renderGame : Size -> Settings -> Game -> Html Msg
renderGame windowSize settings game =
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

        hud =
            renderHUD game

        overlay =
            case game.gameState of
                Paused ->
                    UI.menu
                        [ UI.menuTitle [] "Paused"
                        , UI.btn [ onClick ResumeClicked] "Resume"
                        , UI.btn [ onClick MainMenuClicked] "Main Menu"
                        ]
                GameOver ->
                    UI.menu
                        [ UI.menuTitle [] ("Score " ++ (toString game.score))
                        , if game.scoreboard == NotAsked
                            then
                                form
                                    [ action "#"]
                                    [ input
                                        [ value settings.username
                                        , onInput UpdateUsername
                                        ]
                                        []
                                    , input
                                        [ type_ "submit"
                                        , onClick (PostScore game.gameMode { score = game.score, username = settings.username })
                                        , value "Send"
                                        ]
                                        []
                                    ]
                            else
                                UI.scoreboard game.scoreboard
                        , UI.btn [ onClick (InitializeGame game.gameMode)] "Replay"
                        , UI.btn [ onClick MainMenuClicked] "Main Menu"
                        ]
                Running ->
                    text ""

    in
        div
            []
            [ gameScene
            , hud
            , overlay
            ]


renderHUD : Game -> Html Msg
renderHUD game =
    let
        score =
            styled div
                [ alignSelf start
                , fontSize (pc 4)
                , textShadow3 (px 1) (px 2) (rgb 0 0 0)
                , fontWeight bold
                , fontFamily sansSerif
                , color (rgb 255 255 255)
                ]
                []
                [ text (toString game.score)
                ]


        droppedBalls =
            if game.numberOfDroppedFootballs > 0 && (game.gameMode == SinglePlayerFree || game.gameMode == MultiplayerFree)
            then
                styled div
                    [ alignSelf start
                    , fontSize (pc 3)
                    , textShadow3 (px 1) (px 2) (rgb 0 0 0)
                    , fontWeight bold
                    , fontFamily sansSerif
                    , color (rgb 255 150 150)
                    , marginTop (px 8)
                    ]
                    []
                    [ text (toString game.numberOfDroppedFootballs)
                    ]
            else
                text ""

        countDown =
            if game.gameTime < 4
            then
                let
                    (countDownText, opacity_) =
                        if game.gameTime < 1 then
                            ("3", 1 - (game.gameTime - 0)^2)
                        else if game.gameTime < 2 then
                            ("2", 1 - (game.gameTime - 1)^2)
                        else if game.gameTime < 3 then
                            ("1", 1 - (game.gameTime - 2)^2)
                        else
                            ("Go", 1 - (game.gameTime - 3)^2)
                in
                    styled div
                        [ fontSize (pc 15)
                        , textShadow3 (px 1) (px 2) (rgb 0 0 0)
                        , fontWeight bold
                        , fontFamily sansSerif
                        , color (rgb 255 255 255)
                        , opacity (num opacity_)
                        ]
                        []
                        [ text countDownText]
            else
                text ""

        controls =
            if game.gameTime < 4
                then
                    let
                        opacity_ =
                            if game.gameTime < 3 then
                                1
                            else
                                1 - (game.gameTime - 3)^2
                    in
                        styled div
                            [ fontSize large
                            , textShadow3 (px 1) (px 2) (rgb 0 0 0)
                            , fontWeight bold
                            , fontFamily sansSerif
                            , color (rgb 255 255 255)
                            , opacity (num opacity_)
                            , displayFlex
                            , flexDirection row
                            , justifyContent spaceAround
                            , marginBottom (pct 9)
                            ]
                            []
                            ( game.characters
                                |> List.map
                                    (\ character ->
                                        styled div
                                            [ displayFlex
                                            , flexDirection row
                                            , justifyContent center
                                            , width (pct (95 / ((toFloat << List.length) game.characters)))
                                            ]
                                            []
                                            [ styled div
                                                [ backgroundColor (rgb 100 100 100)
                                                , padding (px 10)
                                                , borderRadius (px 10)
                                                , marginRight (px 10)
                                                ]
                                                []
                                                [ text ("← " ++ keyCodeToString character.leftKeyCode) ]
                                            , styled div
                                                [ backgroundColor (rgb 100 100 100)
                                                , padding (px 10)
                                                , borderRadius (px 10)
                                                , marginLeft (px 10)
                                                ]
                                                []
                                                [ text (keyCodeToString character.rightKeyCode ++ " →") ]
                                            ]
                                    )

                            )
                else
                    text ""

        lives =
            case game.lives of
                Just lives ->
                    styled div
                        [ displayFlex
                        , justifyContent center
                        ]
                        []
                        ( List.range 1 lives.max
                            |> List.map (\index -> if index <= lives.current then "/assets/red-heart.png" else "/assets/grey-heart.png")
                            |> List.map
                                ( \filePath ->
                                    styled img
                                        [ marginRight (px 8)
                                        , height (pc 3)
                                        , lastChild
                                            [ marginRight (px 0)
                                            ]
                                        ]
                                        [ Html.Styled.Attributes.src filePath]
                                        []
                                )
                        )

                Nothing ->
                    text ""

        topRow =
            styled div
                [textAlign center]
                []
                [ score
                , droppedBalls
                , lives
                ]


        middleRow =
            styled div
                [ textAlign center ]
                []
                [ countDown
                ]


        bottomRow =
            div
                []
                [ controls
                ]
    in
        styled div
            [ position fixed
            , width (pct 100)
            , height (pct 100)
            , top (px 0)
            , left (px 0)
            , right (px 0)
            , bottom (px 0)
            , displayFlex
            , flexDirection column
            , justifyContent spaceBetween
            ]
            []
            [ topRow
            , middleRow
            , bottomRow
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

        characterSprite =
            Svg.Styled.image
                [ Svg.Styled.Attributes.x (toString wtlx)
                , Svg.Styled.Attributes.y (toString wtly)
                , Svg.Styled.Attributes.width (toString width)
                , Svg.Styled.Attributes.height (toString height)
                , Svg.Styled.Attributes.xlinkHref currentFrame
                , Svg.Styled.Attributes.imageRendering "pixelated"
                , Svg.Styled.Attributes.preserveAspectRatio "none"
                ] []
    in
        characterSprite


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
