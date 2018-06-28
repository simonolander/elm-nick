module App exposing (..)

import Array exposing (Array)
import Color exposing (red)
import Html exposing (Html, div, program)
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode, downs)
import Model exposing (..)
import Random
import Task exposing (perform)
import Time exposing (Time, every, millisecond)
import Window exposing (Size, resizes)
import AnimationFrame
import Svg exposing (svg)
import Svg.Attributes


characterIdle : SpriteAnimation
characterIdle =
    { frames =
        Array.fromList
            [ "/assets/kattux-stomping/kattux-stomping_0.png"
            , "/assets/kattux-stomping/kattux-stomping_1.png"
            , "/assets/kattux-stomping/kattux-stomping_2.png"
            , "/assets/kattux-stomping/kattux-stomping_3.png"
            ]
    , repeating = True
    , currentFrameIndex = 0
    , currentFrame = "/assets/kattux-stomping/kattux-stomping_0.png"
    , frameTime = 1 / 12
    , currentFrameTime = 0
    }


characterNick : SpriteAnimation
characterNick =
    { frames =
        Array.fromList
            [ "/assets/kattux-nick/kattux-nick_0.png"
            , "/assets/kattux-nick/kattux-nick_1.png"
            , "/assets/kattux-nick/kattux-nick_2.png"
            , "/assets/kattux-nick/kattux-nick_3.png"
            ]
    , repeating = False
    , currentFrameIndex = 0
    , currentFrame = "/assets/kattux-nick/kattux-nick_0.png"
    , frameTime = 1 / 12
    , currentFrameTime = 0
    }


gravity : Float
gravity = -10.0


aspect : Float
aspect = 9/16


characterHeight : Float
characterHeight = 1


boardWidth : Float
boardWidth = characterHeight * 5


boardMargin : Float
boardMargin = boardWidth / 3


footballRadius : Float
footballRadius = characterHeight / 3 / 2


main : Program Never Model Msg
main = program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


init : (Model, Cmd Msg)
init =
    let windowSize = Size 0 0
        footballs =
            []
        character =
            { lane = Left
            , leftKeyCode = 37
            , rightKeyCode = 39
            , spriteAnimation = characterIdle
            , boardIndex = 0
            }
        characters =
            [ character
            , character
            ] |> List.indexedMap (\ index character -> { character | boardIndex = index })
        game =
            { score = 0
            , footballs = footballs
            , characters = characters
            }
        model =
            { windowSize = windowSize
            , game = game
            , frameRate = 0
            }
        cmd = Cmd.batch
            [ perform Resize Window.size
            , generateFootball 2.0 (GameCoordinate 0 0) game
            , generateFootball 2.2 (GameCoordinate 0 0) game
            , generateFootball 2.4 (GameCoordinate 0 0) game
            , generateFootball 2.6 (GameCoordinate 0 0) game
            , generateFootball 2.8 (GameCoordinate 0 0) game
            , generateFootball 3.0 (GameCoordinate 0 0) game
            , generateFootball 3.2 (GameCoordinate 0 0) game
            , generateFootball 3.4 (GameCoordinate 0 0) game
            , generateFootball 3.6 (GameCoordinate 0 0) game
            ]
    in
        ( model
        , cmd
        )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Resize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

        Tick diff ->
            updateOnTick (diff / 1000) model

        KeyDown keyCode ->
            updateOnKeyDown keyCode model

        FootballGenerated football ->
            updateOnFootballGenerated football model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ resizes Resize
        , AnimationFrame.diffs Tick
        , downs KeyDown
        ]


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
        svg
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


updateOnTick : Time -> Model -> (Model, Cmd Msg)
updateOnTick diff model =
    let
        (game, updateGameCommands) = updateGameOnTick diff model.game
        newModel =
            { model
            | frameRate = diff
            , game = game
            }
    in
        ( newModel
        , List.concat
            [ updateGameCommands
            ] |> Cmd.batch
        )


updateGameOnTick : Time -> Game -> (Game, List (Cmd Msg))
updateGameOnTick diff game =
    let
        nickFootball : Football -> List Character -> (Bool, List Character)
        nickFootball football characters =
            let
                isNickable = football.vy < 0 && football.y >= characterHeight && football.y + football.vy * diff <= characterHeight
            in
                if isNickable
                then
                    case characters of
                        (character :: tail) ->
                            let
                                x = getCharacterCenterX character
                                minX = x - characterHeight / 2
                                maxX = x + characterHeight / 2
                                isNicked = football.x >= minX && football.x <= maxX
                            in
                                if isNicked then
                                    ( True
                                    , { character
                                      | spriteAnimation = characterNick
                                      } :: tail
                                    )
                                else
                                    let
                                        (isNicked_, chars_) = nickFootball football tail
                                    in
                                        (isNicked_, character :: chars_)

                        [] ->
                            (False, characters)
                else
                    (False, characters)

        nickFootballs : List Football -> List Character -> (List Football, List Character, List (Cmd Msg))
        nickFootballs footballs characters =
            case footballs of
                (football :: tail) ->
                    let
                        (isNicked, chars_) = nickFootball football characters
                        (footballs__, chars__, commands) = nickFootballs tail chars_
                    in
                        if isNicked then
                            (footballs__, chars__, generateFootball 2.0 (GameCoordinate football.x football.y) game :: commands)
                        else
                            (football :: footballs__, chars__, commands)
                [] ->
                    (footballs, characters, [])

        (unnickedFootballs, nickedCharacters, commands) = nickFootballs game.footballs game.characters

        footballs = unnickedFootballs
            |> List.map (updateFootballOnTick diff)
        characters = nickedCharacters
            |> List.map (updateCharacterOnTick diff)
    in
        ( { game
          | footballs = footballs
          , characters = characters
          }
        , commands
        )


updateFootballOnTick : Float -> Football -> Football
updateFootballOnTick diff football =
    let
        dx = football.vx * diff
        dy = football.vy * diff
        x = football.x + dx
        y = football.y + dy
        vy = football.vy + gravity * diff
        r = football.r + football.vr * diff
    in
        { football
        | x = x
        , y = y
        , vy = vy
        , r = r
        }

updateCharacterOnTick : Time -> Character -> Character
updateCharacterOnTick diff character =
    let
        spriteAnimation = updateAnimationOnTick diff character.spriteAnimation characterIdle
    in
        { character
        | spriteAnimation = spriteAnimation
        }


updateAnimationOnTick : Time -> SpriteAnimation -> SpriteAnimation -> SpriteAnimation
updateAnimationOnTick diff spriteAnimation defaultAnimation =
    if spriteAnimation.repeating then
        let
            newCurrentFrameTime = spriteAnimation.currentFrameTime + diff
            newCurrentFrameIndex = (floor (newCurrentFrameTime / spriteAnimation.frameTime)) % (Array.length spriteAnimation.frames)
            newCurrentFrame = Array.get newCurrentFrameIndex spriteAnimation.frames
                |> Maybe.withDefault ""
        in
            { spriteAnimation
            | currentFrameTime = newCurrentFrameTime
            , currentFrameIndex = newCurrentFrameIndex
            , currentFrame = newCurrentFrame
            }
    else
        let
            newCurrentFrameTime = spriteAnimation.currentFrameTime + diff
            animationEnded = newCurrentFrameTime > spriteAnimation.frameTime * (spriteAnimation.frames |> Array.length |> toFloat)
        in
            if animationEnded then
                defaultAnimation
            else
                let
                    newCurrentFrameIndex = (floor (newCurrentFrameTime / spriteAnimation.frameTime)) % (Array.length spriteAnimation.frames)
                    newCurrentFrame = Array.get newCurrentFrameIndex spriteAnimation.frames
                        |> Maybe.withDefault ""
                in
                    { spriteAnimation
                    | currentFrameTime = newCurrentFrameTime
                    , currentFrameIndex = newCurrentFrameIndex
                    , currentFrame = newCurrentFrame
                    }


updateOnKeyDown : KeyCode -> Model -> (Model, Cmd Msg)
updateOnKeyDown keyCode model =
    let
        newModel =
            { model
            | game = updateGameOnKeyDown keyCode model.game
            }
        cmd = Cmd.none
    in
        (newModel, cmd)

updateGameOnKeyDown : KeyCode -> Game -> Game
updateGameOnKeyDown keyCode game =
    let
        characters = game.characters
            |> List.map (updateCharacterOnKeyDown keyCode)
    in
        { game
        | characters = characters
        }

updateCharacterOnKeyDown : KeyCode -> Character -> Character
updateCharacterOnKeyDown keyCode character =
    let
        lane =
            if character.leftKeyCode == keyCode
            then Left
            else if character.rightKeyCode == keyCode
            then Right
            else character.lane
    in
        { character
        | lane = lane
        }


updateOnFootballGenerated : Football -> Model -> (Model, Cmd Msg)
updateOnFootballGenerated football model =
    ( { model
      | game = updateGameOnFootballGenerated football model.game
      }
    , Cmd.none
    )


updateGameOnFootballGenerated : Football -> Game -> Game
updateGameOnFootballGenerated football game =
    let
        footballs = football :: game.footballs
    in
        { game
        | footballs = footballs
        }


getGameSize : Game -> GameSize
getGameSize game =
    let
        numberOfCharacters = game.characters
            |> List.length
            |> toFloat
        width = (boardMargin * 2 + boardWidth) * numberOfCharacters
        highestFootball = game.footballs
            |> List.map (.y)
            |> List.maximum
            |> Maybe.map ((+) footballRadius)
            |> Maybe.withDefault 0
        height = max (boardWidth / aspect) highestFootball
    in
       GameSize width height


getGameCharacterTop : Int -> Lane -> GameCoordinate
getGameCharacterTop boardIndex lane =
    let
        x : Float
        x = case lane of
            Left ->
                toFloat boardIndex * (boardWidth + 2 * boardMargin) + boardMargin
            Right ->
                toFloat boardIndex * (boardWidth + 2 * boardMargin) + boardMargin + boardWidth
        y : Float
        y = characterHeight
    in
        GameCoordinate x y


getCharacterCenterX : Character -> Float
getCharacterCenterX character =
    let
        (GameCoordinate x _) = getGameCharacterTop character.boardIndex character.lane
    in
        x


generateFootball : Float -> GameCoordinate -> Game -> Cmd Msg
generateFootball time pos game =
    let
        (GameCoordinate x y) = pos

        boardIndex : Random.Generator Int
        boardIndex = Random.int 0 (List.length game.characters - 1)

        lane : Random.Generator Lane
        lane = Random.bool
            |> Random.map (\left -> if left then Left else Right)

        destination : Random.Generator GameCoordinate
        destination = Random.map2 getGameCharacterTop boardIndex lane

        velocity : Random.Generator GameVelocity
        velocity = Random.map (getVelocity time pos) destination

        vx : Random.Generator Float
        vx = Random.map (\(GameVelocity vx _) -> vx) velocity

        vy : Random.Generator Float
        vy = Random.map (\(GameVelocity _ vy) -> vy) velocity

        vr : Random.Generator Float
        vr = Random.float 0 360

        football : Random.Generator Football
        football =
            Random.map3
                (Football x y 0)
                vx
                vy
                vr
    in
        Random.generate FootballGenerated football


{-
x' = dx / t
y'' = gravity
y' = gravity * t + c1
y(t) = gravity * t^2 / 2 + c1 * t + fromY
y(0) = 0 + 0 + c2 = fromY
y(time) = gravity * time^2 / 2 + c1 * time + fromY = toY => c1 = (toY - fromY - gravity * time^2 / 2) / time
y'(t) = gravity * t + (toY - fromY - gravity * time^2 / 2) / time
y'(0) = (toY - fromY - gravity * time^2 / 2) / time
-}
getVelocity : Float -> GameCoordinate -> GameCoordinate -> GameVelocity
getVelocity time (GameCoordinate fromX fromY) (GameCoordinate toX toY) =
    let
        vx : Float
        vx = (toX - fromX) / time

        vy : Float
        vy = (toY - fromY - gravity * time^2 / 2) / time
    in
        GameVelocity vx vy
