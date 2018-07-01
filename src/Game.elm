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
