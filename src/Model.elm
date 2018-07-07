module Model exposing (..)

import Array exposing (Array)
import Keyboard exposing (KeyCode)
import PageVisibility
import Time exposing (Time)
import Window exposing (Size)
import RemoteData exposing (WebData)


type GameState =
    Running
    | Paused
    | GameOver

type GameCoordinate = GameCoordinate Float Float

type WindowCoordinate = WindowCoordinate Float Float

type GameVelocity = GameVelocity Float Float

type GameSize = GameSize Float Float

type alias Board =
    { index: Int
    }

type alias SizeF =
    { width: Float
    , height: Float
    }

type alias MaxCurrent =
    { max: Int
    , current: Int
    }

type alias Lives = MaxCurrent

type alias Game =
    { score: Int
    , footballs: List Football
    , characters: List Character
    , gameState: GameState
    , gameTime: Time
    , footballGenerationTime: Time
    , remainingFootballGenerationTime: Time
    , numberOfDroppedFootballs: Int
    , gameMode: GameMode
    , lives: Maybe Lives
    , scoreboard : WebData (List Score)
    }

type alias Model =
    { windowSize: Size
    , game: Maybe Game
    , frameRate: Time
    , menu: Maybe Menu
    , settings: Settings
    }

type Msg =
    Resize Size
    | Tick Time
    | KeyDown KeyCode
    | FootballGenerated Football
    | VisibilityChanged PageVisibility.Visibility
    | ResumeClicked
    | MenuNavigation Menu
    | InitializeGame GameMode
    | ReceiveScores (WebData (List Score))
    | UpdateUsername String
    | UpdateNumberOfPlayers Int
    | UpdatePlayerControl Int Lane KeyCode
    | PostScore GameMode Score

type Lane = Left | Right

type alias SpriteAnimation =
    { frames: Array String
    , repeating: Bool
    , currentFrameIndex: Int
    , currentFrame: String
    , frameTime: Time
    , currentFrameTime: Time
    }

type alias Character =
    { lane: Lane
    , leftKeyCode: KeyCode
    , rightKeyCode: KeyCode
    , spriteAnimation: SpriteAnimation
    , boardIndex: Int
    , lives: Maybe MaxCurrent
    , timeOfDeath: Maybe Time
    }

type alias Football =
    { x: Float
    , y: Float
    , r: Float
    , vx: Float
    , vy: Float
    , vr: Float
    }

type GameMode =
    SinglePlayerSurvival
    | MultiplayerCooperation
    | MultiplayerFree
    | LastManStanding
    | SinglePlayerFree

type alias CharacterSetting =
    { leftKeyCode: KeyCode
    , rightKeyCode: KeyCode
    }

type alias Settings =
    { characterSettings: Array CharacterSetting
    , footballGenerationTime: Time
    , username: String
    , numberOfPlayers: Int
    }

type Menu =
    MainMenu
    | SettingsMenu
    | MultiplayerMenu
    | SinglePlayerMenu
    | HighscoresMenu


-- REST --

type alias Score =
    { score: Int
    , username: String
    }
