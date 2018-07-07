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
