module Logo where

import Prelude
import Control.Monad.RWS (RWS, get, put, tell)
import Math (Radians, cos, pi, sin)

type TurtleM = RWS Unit (Array Tag) Turtle

initialTurtle :: Turtle
initialTurtle =
  { angle: 0.0
  , position: Position { x: 0.0, y: 0.0 }
  }

newtype Position = Position
  { x :: Number
  , y :: Number
  }

instance positionShow :: Show Position where
  show (Position {x, y}) =
    "(" <> show x <> "," <> show y <> ")"

type Turtle =
  { angle :: Radians
  , position :: Position
  }

data Tag = Line Position Position

forward :: Number -> TurtleM Unit
forward n = do
  startTurtle <- get
  let startPosition = startTurtle.position
  let angle = startTurtle.angle
  let endPosition = move angle n startPosition
  put $ startTurtle { position = endPosition }
  tell [ Line startPosition endPosition ]
  pure unit

right :: Degrees -> TurtleM Unit
right delta = do
  startTurtle <- get
  let angle = startTurtle.angle
  let newAngle = angle + (degreesToRadians delta)
  put $ startTurtle { angle = newAngle }
  pure unit

type Degrees = Number

degreesToRadians :: Degrees -> Radians
degreesToRadians =
  (*) (pi / 180.0)

move :: Radians -> Number -> Position -> Position
move angle distance (Position {x, y}) =
  Position { x: x + newX, y: y + newY }
  where
    newX = cos angle * distance
    newY = sin angle * distance
