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
import Game.Util exposing (..)
import PageVisibility exposing (Visibility)
import Random
import RemoteData exposing (RemoteData(Loading, NotAsked))
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
        |> noCmd setTimeOfDeath
        |> noCmd checkForGameOver
        |> batch


setTimeOfDeath : Game -> Game
setTimeOfDeath game =
    let
        setTimeOfDeathForCharacter character =
            case character.timeOfDeath of
                Just timeOfDeath ->
                    character
                Nothing ->
                    if isCharacterDead character
                    then
                        { character
                        | timeOfDeath = Just game.gameTime
                        }
                    else
                        character
    in
        { game
        | characters = List.map setTimeOfDeathForCharacter game.characters
        }


checkForGameOver : Game -> Game
checkForGameOver game =
    let
        outOfLives =
            game.lives
            |> Maybe.map (.current >> ((>=) 0))
            |> Maybe.withDefault False

        lastManStanding =
            game.gameMode == LastManStanding &&
                ( game.characters
                    |> List.filter isCharacterAlive
                    |> List.length
                    |> (==) 1
                )

        gameState =
            if outOfLives || lastManStanding
            then
                GameOver
            else
                game.gameState

    in
        { game
        | gameState = gameState
        , scoreboard = NotAsked
        }

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
                            x =
                                getCharacterCenterX character

                            minX =
                                x - characterHeight / 2

                            maxX =
                                x + characterHeight / 2

                            characterIsAlive =
                                isCharacterAlive character

                            isNicked =
                                characterIsAlive && football.position.x >= minX && football.position.x <= maxX
                        in
                            if isNicked then
                                ( True
                                , { character
                                  | spriteAnimation = characterNick
                                  , numberOfNickedFootballs = character.numberOfNickedFootballs + 1
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
                            (footballs__, chars__, generateFootball football.position game :: commands)
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
                withinReach =
                    withinLeft || withinRight
                characterIsAlive =
                    isCharacterAlive character
            in
                withinReach && characterIsAlive

        update football character =
            if characterCouldHaveNickedFootball character football
            then
                let
                    removeLife character =
                        { character
                        | lives = Maybe.map (decreaseLives 1) character.lives
                        }

                    setAnimationIfDead character =
                        { character
                        | spriteAnimation =
                            if isCharacterDead character
                            then
                                characterSitting
                            else
                                character.spriteAnimation
                        }
                in
                    character
                    |> removeLife
                    |> setAnimationIfDead
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
                , [ generateFootball { x = -characterHeight, y = characterHeight } game ]
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
        characterIsAlive =
            isCharacterAlive character
        lane =
            if characterIsAlive && character.leftKeyCode == keyCode
            then
                Left
            else if characterIsAlive && character.rightKeyCode == keyCode
            then
                Right
            else
                character.lane
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
generateFootball position game =
    let
        boardIndex : Random.Generator Int
        boardIndex =
            game.characters
            |> List.filter isCharacterAlive
            |> List.map .boardIndex
            |> randomSelection 0

        lane : Random.Generator Lane
        lane = Random.bool
            |> Random.map (\left -> if left then Left else Right)

        destination : Random.Generator GameCoordinate
        destination = Random.map2 getGameCharacterTop boardIndex lane

        time =
            Random.float 1.5 3.0

        velocity : Random.Generator GameVelocity
        velocity = Random.map2 (getVelocity position) destination time

        rotationSpeed : Random.Generator Float
        rotationSpeed = Random.float 0 360

        createFootball : GameCoordinate -> Float -> GameVelocity -> Float -> Football
        createFootball position rotation velocity rotationSpeed =
            { position = position
            , velocity = velocity
            , rotation = rotation
            , rotationSpeed = rotationSpeed
            }

        football : Random.Generator Football
        football =
            Random.map2
                (createFootball position 0)
                velocity
                rotationSpeed
    in
        Random.generate FootballGenerated football


isFootballDropped : Football -> Bool
isFootballDropped football =
    football.position.y < 0


decreaseLives : Int -> Lives -> Lives
decreaseLives numberOfLives lives =
    { lives
    | current = max 0 (lives.current - numberOfLives)
    }


moveFootball : Time -> Football -> Football
moveFootball dt football =
    let
        position =
            football.position

        velocity =
            football.velocity
    in
        { football
        | position =
            { x = position.x + velocity.x * dt
            , y = position.y + velocity.y * dt
            }
        , velocity =
            { velocity
            | y = velocity.y + gravity * dt
            }
        , rotation = football.rotation + football.rotationSpeed * dt
        }


isFootballNickable : Time -> Football -> Bool
isFootballNickable dt football =
    football.velocity.y < 0 && football.position.y >= characterHeight && football.position.y + football.velocity.y * dt <= characterHeight


isOutOfLives : Lives -> Bool
isOutOfLives lives =
    lives.current <= 0


isCharacterDead : Character -> Bool
isCharacterDead character =
    character.lives
    |> Maybe.map isOutOfLives
    |> Maybe.withDefault False


isCharacterAlive : Character -> Bool
isCharacterAlive = not << isCharacterDead
