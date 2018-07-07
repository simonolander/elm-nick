module Util exposing (..)

import Array
import Constants exposing (characterIdle)
import Keyboard exposing (KeyCode)
import Model exposing (..)
import Random


maybeCons : Maybe a -> List a -> List a
maybeCons maybe list =
    case maybe of
        Just value ->
            value :: list
        Nothing ->
            list


filterMaybe : List (Maybe a) -> List a
filterMaybe = List.foldr maybeCons []


separate : (a -> Bool) -> List a -> (List a, List a)
separate func list =
    case list of
        head :: tail ->
            let
                (true, false) = separate func tail
            in
                if func head then
                    (head :: true, false)
                else
                    (true, head :: false)

        [] -> ([], [])


chain : (a -> (a, List b)) -> (a, List (List b)) -> (a, List (List b))
chain func (a, list) =
    let
        (a_, list_) = func a
    in
        (a_, list_ :: list)


noCmd : (a -> a) -> (a, List (List b)) -> (a, List (List b))
noCmd func (a, list) =
    (func a, list)


batch : (a, List (List (Cmd b))) -> (a, Cmd b)
batch (a, list) =
    (a, Cmd.batch (List.concat list))


listGet : Int -> List a -> Maybe a
listGet index list =
    case list of
        (h::t) ->
            if index == 0 then Just h else listGet (index - 1) t

        [] ->
            Nothing


keyCodeToString : KeyCode -> String
keyCodeToString keyCode =
    case keyCode of
        37 -> "Left"
        39 -> "Right"
        17 -> "Control"
        18 -> "Alt"
        91 -> "Meta Left"
        93 -> "Meta Right"
        32 -> "Space"
        16 -> "Shift"
        9 -> "Tab"
        13 -> "Enter"
        8 -> "Backspace"
        189 -> "Dash"
        190 -> "Period"
        188 -> "Comma"
        221 -> "¨"
        187 -> "+"
        192 -> "§"
        65 -> "A"
        66 -> "B"
        67 -> "C"
        68 -> "D"
        69 -> "E"
        70 -> "F"
        71 -> "G"
        72 -> "H"
        73 -> "I"
        74 -> "J"
        75 -> "K"
        76 -> "L"
        77 -> "M"
        78 -> "N"
        79 -> "O"
        80 -> "P"
        81 -> "Q"
        82 -> "R"
        83 -> "S"
        84 -> "T"
        85 -> "U"
        86 -> "V"
        87 -> "W"
        88 -> "X"
        89 -> "Y"
        90 -> "Z"
        219 -> "Å"
        222 -> "Ä"
        186 -> "Ö"
        48 ->"0"
        49 ->"1"
        50 ->"2"
        51 ->"3"
        52 ->"4"
        53 ->"5"
        54 ->"6"
        55 ->"7"
        56 ->"8"
        57 ->"9"
        other -> toString other


goodKeys : List (Int, String)
goodKeys =
    [ (37, "Left")
    , (39, "Right")
    , (17, "Control")
    , (18, "Alt")
    , (91, "Meta Left")
    , (93, "Meta Right")
    , (32, "Space")
    , (16, "Shift")
    , (9, "Tab")
    , (13, "Enter")
    , (8, "Backspace")
    , (189, "Dash")
    , (190, "Period")
    , (188, "Comma")
    , (221, "¨")
    , (187, "+")
    , (192, "§")
    , (65, "A")
    , (66, "B")
    , (67, "C")
    , (68, "D")
    , (69, "E")
    , (70, "F")
    , (71, "G")
    , (72, "H")
    , (73, "I")
    , (74, "J")
    , (75, "K")
    , (76, "L")
    , (77, "M")
    , (78, "N")
    , (79, "O")
    , (80, "P")
    , (81, "Q")
    , (82, "R")
    , (83, "S")
    , (84, "T")
    , (85, "U")
    , (86, "V")
    , (87, "W")
    , (88, "X")
    , (89, "Y")
    , (90, "Z")
    , (219, "Å")
    , (222, "Ä")
    , (186, "Ö")
    , (48, "0")
    , (49, "1")
    , (50, "2")
    , (51, "3")
    , (52, "4")
    , (53, "5")
    , (54, "6")
    , (55, "7")
    , (56, "8")
    , (57, "9")
    ]


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
            }
    in
        settings
        |> List.take numberOfCharacters
        |> List.indexedMap characterSettingToCharacter


randomSelection : a -> List a -> Random.Generator a
randomSelection default list =
    let
        index =
            Random.int 0 (List.length list)

        get index =
            listGet index list
            |> Maybe.withDefault default
    in
        Random.map get index
