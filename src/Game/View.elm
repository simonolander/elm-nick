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
        windowWidth = toFloat windowSize.width
        windowHeight = toFloat windowSize.height
        gameWidth = getGameWidth game

        viewBoxWidth = gameWidth
        viewBoxHeight = windowHeight / windowWidth * viewBoxWidth

        gameToViewBoxCoordinate : GameCoordinate -> ViewBoxCoordinate
        gameToViewBoxCoordinate (GameCoordinate gameX gameY) =
            let
                viewBoxX = gameX / gameWidth * viewBoxWidth
                viewBoxY = viewBoxHeight - gameY
            in
                ViewBoxCoordinate viewBoxX viewBoxY

        footballs =
            game.footballs
            |> List.map (renderFootball gameToViewBoxCoordinate)

        dividers =
            List.range 0 (List.length game.characters)
            |> List.map (toFloat >> ((*) (boardWidth + 2 * boardMargin)) >> (renderDivider gameToViewBoxCoordinate))

        characters =
            game.characters
            |> List.map (renderCharacter gameToViewBoxCoordinate)

        elements =
            List.concat
                [ dividers
                , characters
                , footballs
                ]

        gameScene =
            Svg.Styled.svg
                [ Svg.Styled.Attributes.width (toString windowWidth)
                , Svg.Styled.Attributes.height (toString windowHeight)
                , Svg.Styled.Attributes.viewBox ("0 0 " ++ (toString viewBoxWidth) ++ " " ++ (toString viewBoxHeight))
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
                        , UI.btn [ onClick (InitializeGame game.gameMode)] "Restart"
                        , styled div [ height (px 20) ] [] []
                        , UI.btn [ onClick (MenuNavigation MainMenu)] "Back to Main Menu"
                        ]
                GameOver ->
                    UI.menu
                        [ UI.menuTitle [] ("Score " ++ (toString game.score))
                        , styled div
                            [ width (pct 100)
                            , color (rgb 255 255 255)
                            , textShadow3 (px 1) (px 2) (rgb 0 0 0)
                            , fontWeight bold
                            , fontFamily sansSerif
                            ]
                            []
                            [ text
                                ( if game.gameMode == LastManStanding then
                                      "Winner"
                                  else
                                      "Highscores"
                                )
                            ]
                        , if game.gameMode == LastManStanding
                          then
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
                                , flexShrink (int 0)
                                , fontFamily sansSerif
                                ]
                                []
                                ( game.characters
                                  |> List.sortWith sortTimeOfDeath
                                  |> List.reverse
                                  |> List.indexedMap
                                      ( \index character ->
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
                                                [text ("Player " ++ toString (character.boardIndex + 1))]
                                            , styled div
                                                [ display tableCell
                                                , verticalAlign middle
                                                , width (pct 25)
                                                , textAlign right
                                                ]
                                                []
                                                [text (toString character.numberOfNickedFootballs)]
                                            ]
                                    )
                                )
                          else
                            if game.scoreboard == NotAsked
                            then
                                styled div
                                    [ width (pct 100)
                                    , displayFlex
                                    , justifyContent spaceBetween
                                    ]
                                    []
                                    [ styled input
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
                                        [ value settings.username
                                        , onInput UpdateUsername
                                        ]
                                        []
                                    , styled button
                                        [ hover
                                            [ backgroundColor UI.secondaryTheme
                                            ]
                                        , fontSize large
                                        , color (rgb 255 255 255)
                                        , borderRadius (px 8)
                                        , cursor pointer
                                        , height (px 60)
                                        , lineHeight (px 57)
                                        , fontWeight bold
                                        , backgroundColor UI.primaryTheme
                                        , padding2 (px 0) (px 16)
                                        , border3 (px 1) solid UI.primaryTheme
                                        , borderBottom3 (px 3) solid UI.secondaryTheme
                                        , display inlineBlock
                                        , flexShrink (int 0)
                                        , textAlign center
                                        , textDecoration none
                                        , backgroundImage none
                                        , textShadow none
                                        ]
                                        [ onClick (PostScore game.gameMode { score = game.score, username = settings.username })
                                        ]
                                        [ text "Submit" ]
                                    ]
                            else
                                UI.scoreboard game.scoreboard
                        , styled div [ height (px 20) ] [] []
                        , UI.btn [ onClick (InitializeGame game.gameMode)] "Replay"
                        , UI.btn [ onClick (MenuNavigation MainMenu)] "Back to Main Menu"
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



renderFootball : (GameCoordinate -> ViewBoxCoordinate) -> Football -> Svg.Styled.Svg msg
renderFootball g2w football =
    let
        (ViewBoxCoordinate wcx wcy) = g2w (GameCoordinate football.x football.y)
        (ViewBoxCoordinate wtlx wtly) = g2w (GameCoordinate (football.x - footballRadius) (football.y + footballRadius))
        (ViewBoxCoordinate wbrx wbry) = g2w (GameCoordinate (football.x + footballRadius) (football.y - footballRadius))
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


renderCharacter : (GameCoordinate -> ViewBoxCoordinate) -> Character -> Svg.Styled.Svg msg
renderCharacter g2w character =
    let
        (GameCoordinate gcx gty) = getGameCharacterTop character.boardIndex character.lane
        (ViewBoxCoordinate wtlx wtly) = g2w (GameCoordinate (gcx - characterHeight / 2) (gty))
        (ViewBoxCoordinate wbrx wbry) = g2w (GameCoordinate (gcx + characterHeight / 2) (gty - characterHeight))
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

        renderHearts lives =
            let
                h =
                    characterHeight / toFloat lives.max * 0.95
                y index =
                    wbry - h - toFloat index * (h + characterHeight / toFloat lives.max * 0.05)
                image index =
                    if index < lives.current then
                        "/assets/red-heart.png"
                    else
                        "/assets/grey-heart.png"
                x =
                    wbrx + width * 0.01
            in
                List.range 0 (lives.max - 1)
                |> List.map
                    (\index ->
                        Svg.Styled.image
                            [ Svg.Styled.Attributes.x (toString x)
                            , Svg.Styled.Attributes.y (toString (y index))
                            , Svg.Styled.Attributes.width (toString h)
                            , Svg.Styled.Attributes.height (toString h)
                            , Svg.Styled.Attributes.xlinkHref (image index)
                            , Svg.Styled.Attributes.preserveAspectRatio "none"]
                            []
                    )
                |> Svg.Styled.g []
    in
        case character.lives of
            Just lives ->
                Svg.Styled.g
                    []
                    [ characterSprite
                    , renderHearts lives
                    ]
            Nothing ->
                characterSprite


renderDivider : (GameCoordinate -> ViewBoxCoordinate) -> Float ->  Svg.Styled.Svg msg
renderDivider g2w x =
    let
        (ViewBoxCoordinate wtlx wtly) =
            g2w (GameCoordinate (x - characterHeight / 2) characterHeight)

        (ViewBoxCoordinate wbrx wbry) =
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


sortTimeOfDeath : Character -> Character -> Order
sortTimeOfDeath c1 c2 =
    case (c1.timeOfDeath, c2.timeOfDeath) of
        (Just t1, Just t2) -> compare t1 t2
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT
        (Nothing, Nothing) -> EQ
