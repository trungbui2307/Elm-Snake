module Model exposing (..)

import Time exposing (Posix)

type alias Model =
  { 
    snake : List Point
  , apples : List Point
  , nextDirections : List Direction
  , currentDirection : Direction
  , options : Options
  , gameStatus : GameStatus
  , score : Int
  }

type Direction
    = Up
    | Down
    | Left
    | Right

type GameStatus
    = Initializing
    | Playing
    | Paused
    | GameOver

type alias Options =
    { 
      gameBoardSize : Int
    }


type alias Point =
    { x : Int, y : Int }

type CellState
    = Snake
    | Food
    | Empty


type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space

{-| Step 2: Define Msg  -}
type Msg
  = NextFrame Posix
  | TogglePause
  | KeyDown String
  | StartGame
  | NewApple Int Int
  | GameBoardSizeChanged Int
  | DirectionChange Direction
