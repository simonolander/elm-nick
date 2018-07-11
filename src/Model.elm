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

type alias XY =
    { x: Float
    , y: Float
    }

{-
GameCoordinate represents a coordinate in game space and is used in the game physics.
-}
type alias GameCoordinate = XY

{-
ViewBoxCoordinate represents a coordinate in SVG space and is used in rendering.
-}
type alias ViewBoxCoordinate = XY

type alias GameVelocity = XY

type alias GameWidth = Float

type alias Lives =
    { max: Int
    , current: Int
    }

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
    , lives: Maybe Lives
    , timeOfDeath: Maybe Time
    , numberOfNickedFootballs: Int
    }

type alias Football =
    { position: GameCoordinate
    , velocity: GameVelocity
    , rotation: Float
    , rotationSpeed: Float
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
    | MultiplayerMenu
    | SinglePlayerMenu
    | SelectHighscoreMenu
    | HighscoreMenu GameMode (WebData (List Score))

type alias Score =
    { score: Int
    , username: String
    }
