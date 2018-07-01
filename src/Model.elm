module Model exposing (..)

import Array exposing (Array)
import Keyboard exposing (KeyCode)
import PageVisibility
import Time exposing (Time)
import Window exposing (Size)


type GameState =
    Running
    | Paused

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

type alias Game =
    { score: Int
    , footballs: List Football
    , characters: List Character
    , gameState: GameState
    , gameTime: Float
    , footballGenerationTime: Float
    , remainingFootballGenerationTime: Float
    , numberOfDroppedFootballs: Int
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
    | OnResumeClicked
    | OnMainMenuClicked
    | OnSettingsClicked
    | OnSinglePlayerClicked

type Lane = Left | Right

type alias SpriteAnimation =
    { frames: Array String
    , repeating: Bool
    , currentFrameIndex: Int
    , currentFrame: String
    , frameTime: Float
    , currentFrameTime: Float
    }

type alias Character =
    { lane: Lane
    , leftKeyCode: KeyCode
    , rightKeyCode: KeyCode
    , spriteAnimation: SpriteAnimation
    , boardIndex: Int
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
    SinglePlayer
    | Collaboration
    | LastManStanding
    | SinglePlayerFree

type alias CharacterSetting =
    { leftKeyCode: KeyCode
    , rightKeyCode: KeyCode
    }

type alias Settings =
    { characterSettings: List CharacterSetting
    , footballGenerationTime: Float
    }

type Menu =
    MainMenu
    | SettingsMenu
    | MultiPlayerMenu
    | SinglePlayerMenu
