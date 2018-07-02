module Game.Update exposing
    ( updateGameOnTick
    , updateGameOnKeyDown
    , updateGameOnVisibilityChanged
    , updateGameOnResumeClicked
    , updateGameOnFootballGenerated
    )

import Array
import Keyboard exposing (KeyCode)
import Model exposing (..)
import Constants exposing (..)
import Game exposing (..)
import PageVisibility exposing (Visibility)
import Random
import Time exposing (Time)


updateGameOnTick : Time -> Game -> (Game, Cmd Msg)
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
                            (footballs__, chars__, generateFootball (GameCoordinate football.x football.y) game :: commands)
                        else
                            (football :: footballs__, chars__, commands)
                [] ->
                    (footballs, characters, [])

        (unnickedFootballs, nickedCharacters, bounceFootballCommands) = nickFootballs game.footballs game.characters

        movedFootballs =
            unnickedFootballs
            |> List.map (updateFootballOnTick diff)

        undroppedFootballs =
            movedFootballs
            |> List.filter (\football -> football.y + footballRadius > 0)

        characters =
            nickedCharacters
            |> List.map (updateCharacterOnTick diff)

        score =
            game.score + (List.length game.footballs - List.length unnickedFootballs)

        numberOfDroppedFootballs =
            game.numberOfDroppedFootballs + (List.length movedFootballs - List.length undroppedFootballs)

        gameTime =
            game.gameTime + diff

        (remainingFootballGenerationTime, newFootballCommands) =
            if game.remainingFootballGenerationTime - diff < 0
            then
                ( game.remainingFootballGenerationTime - diff + game.footballGenerationTime
                , [ generateFootball (GameCoordinate -characterHeight characterHeight) game ]
                )
            else
                (game.remainingFootballGenerationTime - diff, [])

        commands =
            List.concat
                [ bounceFootballCommands
                , newFootballCommands
                ]
    in
        ( { game
          | footballs = undroppedFootballs
          , characters = characters
          , score = score
          , gameTime = gameTime
          , remainingFootballGenerationTime = remainingFootballGenerationTime
          , numberOfDroppedFootballs = numberOfDroppedFootballs
          }
        , Cmd.batch commands
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


updateGameOnKeyDown : KeyCode -> Game -> Game
updateGameOnKeyDown keyCode game =
    let
        characters =
            case game.gameState of
                Running ->
                    game.characters
                    |> List.map (updateCharacterOnKeyDown keyCode)
                Paused ->
                    game.characters

        gameState =
            if keyCode == keyCodes.escape
            then
                case game.gameState of
                    Running -> Paused
                    Paused -> Running
            else
                game.gameState

        newGame =
            { game
            | characters = characters
            , gameState = gameState
            }

    in
        newGame


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


updateGameOnVisibilityChanged : Visibility -> Game -> Game
updateGameOnVisibilityChanged visibility game =
    { game
    | gameState = Paused
    }


updateGameOnResumeClicked : Game -> Game
updateGameOnResumeClicked game =
    { game
    | gameState = Running
    }


updateGameOnFootballGenerated : Football -> Game -> Game
updateGameOnFootballGenerated football game =
    let
        footballs = football :: game.footballs
    in
        { game
        | footballs = footballs
        }


generateFootball : GameCoordinate -> Game -> Cmd Msg
generateFootball pos game =
    let
        (GameCoordinate x y) = pos

        boardIndex : Random.Generator Int
        boardIndex = Random.int 0 (List.length game.characters - 1)

        lane : Random.Generator Lane
        lane = Random.bool
            |> Random.map (\left -> if left then Left else Right)

        destination : Random.Generator GameCoordinate
        destination = Random.map2 getGameCharacterTop boardIndex lane

        time =
            Random.float 1.5 3.0

        velocity : Random.Generator GameVelocity
        velocity = Random.map2 (getVelocity pos) destination time

        vr : Random.Generator Float
        vr = Random.float 0 360

        f x y r (GameVelocity vx vy) vr =
            { x = x
            , y = y
            , r = r
            , vx = vx
            , vy = vy
            , vr = vr
            }

        football : Random.Generator Football
        football =
            Random.map2
                (f x y 0)
                velocity
                vr
    in
        Random.generate FootballGenerated football
