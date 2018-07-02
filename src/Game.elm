module Game exposing (..)

import Model exposing (..)
import Constants exposing (..)

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
