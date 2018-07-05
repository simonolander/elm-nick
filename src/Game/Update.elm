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
import RemoteData exposing (RemoteData(Loading))
import Rest exposing (postScore)
import Time exposing (Time)
import Util exposing (..)


updateGameOnTick : Time -> Game -> (Game, Cmd Msg)
updateGameOnTick diff game =
    let
        updateGameTime diff game =
            { game
            | gameTime = game.gameTime + diff
            }
    in
        (game, [])
        |> chain (nickFootballs diff)
        |> noCmd (moveFootballs diff)
        |> noCmd dropFootballs
        |> noCmd (updateSpriteAnimations diff)
        |> chain (updateFootballGenerationTimer diff)
        |> noCmd (updateGameTime diff)
        |> chain checkForGameOver
        |> batch


checkForGameOver : Game -> (Game, List (Cmd Msg))
checkForGameOver game =
    let
        outOfLives =
            game.lives
            |> Maybe.map (.current >> ((>=) 0))
            |> Maybe.withDefault False

        gameState =
            if outOfLives
            then
                GameOver
            else
                game.gameState

        cmds =
            if gameState == GameOver
            then
                [ postScore
                    { score = game.score
                    , username = "anonette"
                    }
                ]
            else
                []
    in
        ( { game
          | gameState = gameState
          , scoreboard = Loading
          }
        , cmds
        )

nickFootballs : Time -> Game -> (Game, List (Cmd Msg))
nickFootballs diff game =
    let
        nickFootball : Football -> List Character -> (Bool, List Character)
        nickFootball football characters =
            if isFootballNickable diff football
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

        numberOfNickedFootballs = List.length game.footballs - List.length unnickedFootballs
    in
        ( { game
          | footballs = unnickedFootballs
          , characters = nickedCharacters
          , score = game.score + numberOfNickedFootballs
          }
        , bounceFootballCommands
        )


moveFootballs : Time -> Game -> Game
moveFootballs dt game =
    { game
    | footballs = List.map (moveFootball dt) game.footballs
    }


dropFootballs : Game -> Game
dropFootballs game =
    let
        (droppedFootballs, undroppedFootballs) =
            game.footballs
            |> separate isFootballDropped

        numberOfDroppedFootballs =
            List.length droppedFootballs

        newCharacters =
            game.characters
            |> List.map (updateCharacterOnDroppedFootball droppedFootballs)

        newLives =
            Maybe.map (decreaseLives numberOfDroppedFootballs) game.lives


    in
        { game
        | footballs = undroppedFootballs
        , characters = newCharacters
        , lives = newLives
        , numberOfDroppedFootballs = game.numberOfDroppedFootballs + numberOfDroppedFootballs
        }


updateCharacterOnDroppedFootball : List Football -> Character -> Character
updateCharacterOnDroppedFootball footballs character =
    let
        characterCouldHaveNickedFootball character football =
            let
                xWhenNickable =
                    getXWhenNickable football
                leftX =
                    getLaneCenterX Left character.boardIndex
                rightX =
                    getLaneCenterX Right character.boardIndex
                dx = characterHeight / 2
                withinLeft =
                    leftX - dx <= xWhenNickable && xWhenNickable <= leftX + dx
                withinRight =
                    rightX - dx <= xWhenNickable && xWhenNickable <= rightX + dx
            in
                withinLeft || withinRight

        update football character =
            if characterCouldHaveNickedFootball character football
            then
                { character
                | lives = Maybe.map (decreaseLives 1) character.lives
                }
            else
                character
    in
        List.foldl update character footballs


updateFootballGenerationTimer : Time -> Game -> (Game, List (Cmd Msg))
updateFootballGenerationTimer dt game =
    let
        (remainingFootballGenerationTime, newFootballCommands) =
            if game.remainingFootballGenerationTime - dt < 0
            then
                ( game.remainingFootballGenerationTime - dt + game.footballGenerationTime
                , [ generateFootball (GameCoordinate (-characterHeight) characterHeight) game ]
                )
            else
                (game.remainingFootballGenerationTime - dt, [])
    in
        ( { game
          | remainingFootballGenerationTime = remainingFootballGenerationTime
          }
        , newFootballCommands
        )


updateSpriteAnimations : Time -> Game -> Game
updateSpriteAnimations diff game =
    let
        updateCharacterSpriteAnimation character =
            { character
            | spriteAnimation = updateAnimationOnTick diff character.spriteAnimation characterIdle
            }

        characters =
            List.map updateCharacterSpriteAnimation game.characters
    in
        { game
        | characters = characters
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

                GameOver ->
                    game.characters

        gameState =
            if keyCode == keyCodes.escape
            then
                case game.gameState of
                    Running -> Paused
                    Paused -> Running
                    GameOver -> GameOver
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
    case game.gameState of
        Running ->
            { game
            | gameState = Paused
            }

        Paused ->
            game

        GameOver ->
            game


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


isFootballDropped : Football -> Bool
isFootballDropped football =
    football.y < 0


decreaseLives : Int -> Lives -> Lives
decreaseLives numberOfLives lives =
    { lives
    | current = max 0 (lives.current - numberOfLives)
    }

moveFootball : Time -> Football -> Football
moveFootball dt football =
    { football
    | x = football.x + football.vx * dt
    , y = football.y + football.vy * dt
    , vy = football.vy + gravity * dt
    , r = football.r + football.vr * dt
    }

isFootballNickable : Time -> Football -> Bool
isFootballNickable dt football =
    football.vy < 0 && football.y >= characterHeight && football.y + football.vy * dt <= characterHeight
