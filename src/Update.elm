module Update exposing (..)

import Array
import Keyboard exposing (KeyCode)
import Model exposing (..)
import Time exposing (Time)
import Constants exposing (..)
import Game exposing (..)
import Random


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

