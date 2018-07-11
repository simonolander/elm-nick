module Constants exposing (..)

import Array
import Keyboard exposing (KeyCode)
import Model exposing (SpriteAnimation)


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


characterSitting : SpriteAnimation
characterSitting =
    { frames =
        Array.fromList
            [ "/assets/kattux-sitting/kattux-sitting_0.png"
            ]
    , repeating = True
    , currentFrameIndex = 0
    , currentFrame = "/assets/kattux-sitting/kattux-sitting_0.png"
    , frameTime = 1 / 12
    , currentFrameTime = 0
    }


redHeartImage : String
redHeartImage = "/assets/red-heart.png"


greyHeartImage : String
greyHeartImage = "/assets/grey-heart.png"


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


keyCodes : { escape : KeyCode }
keyCodes =
    { escape = 27
    }


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
