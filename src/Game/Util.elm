module Game.Util exposing (..)

import Array
import Model exposing (..)
import Constants exposing (..)
import RemoteData exposing (RemoteData(NotAsked))


getGameWidth : Game -> GameWidth
getGameWidth game =
    let
        numberOfCharacters = game.characters
            |> List.length
            |> toFloat
        width = (boardMargin * 2 + boardWidth) * numberOfCharacters
    in
       width


getGameCharacterTop : Int -> Lane -> GameCoordinate
getGameCharacterTop boardIndex lane =
    let
        x =
            getLaneCenterX lane boardIndex
        y =
            characterHeight
    in
        GameCoordinate x y


getCharacterCenterX : Character -> Float
getCharacterCenterX character =
    getLaneCenterX character.lane character.boardIndex


getLaneCenterX : Lane -> Int -> Float
getLaneCenterX lane boardIndex =
    case lane of
        Left ->
            toFloat boardIndex * (boardWidth + 2 * boardMargin) + boardMargin
        Right ->
            toFloat boardIndex * (boardWidth + 2 * boardMargin) + boardMargin + boardWidth


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
getVelocity : GameCoordinate -> GameCoordinate -> Float -> GameVelocity
getVelocity (GameCoordinate fromX fromY) (GameCoordinate toX toY) time =
    let
        vx : Float
        vx = (toX - fromX) / time

        vy : Float
        vy = (toY - fromY - gravity * time^2 / 2) / time
    in
        GameVelocity vx vy

{-
y''(t) = gravity
y'(t) = gravity * t + vy
y(t) = gravity * t^2 / 2 + vy * t + py
characterHeight = gravity * t^2 / 2 + vy * t + py
t^2 + (2 * vy / gravity) * t + (py - characterHeight) * 2 / gravity = 0
t = -(vy / gravity) +/- sqrt( (vy / gravity)^2 + (characterHeight - py) * 2 / gravity )

x'(t) = vx
x(t) = vx * t + px

-}
getXWhenNickable : Football -> Float
getXWhenNickable football =
    let
        t = -(football.vy / gravity) + sqrt( (football.vy / gravity)^2 + (characterHeight - football.y) * 2 / gravity )
    in
        football.vx * t + football.x


settingsToCharacters : Int -> Maybe Lives -> List CharacterSetting -> List Character
settingsToCharacters numberOfCharacters lives settings =
    let
        characterSettingToCharacter index setting =
            { lane = Left
            , leftKeyCode = setting.leftKeyCode
            , rightKeyCode = setting.rightKeyCode
            , spriteAnimation = characterIdle
            , boardIndex = index
            , lives = lives
            , timeOfDeath = Nothing
            , numberOfNickedFootballs = 0
            }
    in
        settings
        |> List.take numberOfCharacters
        |> List.indexedMap characterSettingToCharacter


initializeGame : GameMode -> Model -> (Model, Cmd Msg)
initializeGame gameMode model =
    let
        settings = model.settings
        footballs =
            []
        defaultLives =
            { max = 3
            , current = 3
            }
        lives =
            case gameMode of
                SinglePlayerFree ->
                    Nothing
                SinglePlayerSurvival ->
                    Just defaultLives
                MultiplayerCooperation ->
                    Just defaultLives
                MultiplayerFree ->
                    Nothing
                LastManStanding ->
                    Nothing

        firstCharacterSetting =
            Array.get 0 model.settings.characterSettings
            |> Maybe.withDefault
                { leftKeyCode = 37
                , rightKeyCode = 39
                }

        characters =
            case gameMode of
                SinglePlayerFree ->
                    settingsToCharacters 2 Nothing [firstCharacterSetting, firstCharacterSetting]
                SinglePlayerSurvival ->
                    settingsToCharacters 2 Nothing [firstCharacterSetting, firstCharacterSetting]
                MultiplayerCooperation ->
                    settingsToCharacters model.settings.numberOfPlayers Nothing (Array.toList model.settings.characterSettings)
                MultiplayerFree ->
                    settingsToCharacters model.settings.numberOfPlayers Nothing (Array.toList model.settings.characterSettings)
                LastManStanding ->
                    settingsToCharacters model.settings.numberOfPlayers (Just defaultLives) (Array.toList model.settings.characterSettings)

        game =
            { score = 0
            , footballs = footballs
            , characters = characters
            , gameState = Running
            , gameTime = 0
            , footballGenerationTime = settings.footballGenerationTime
            , remainingFootballGenerationTime = 3
            , numberOfDroppedFootballs = 0
            , gameMode = gameMode
            , lives = lives
            , scoreboard = NotAsked
            }
    in
        ( { model
          | menu = Nothing
          , game = Just game
          }
        , Cmd.none
        )
