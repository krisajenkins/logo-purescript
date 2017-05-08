module Logo where

import Prelude
import Control.Monad.RWS (RWS, get, put, tell)
import Math (Radians, cos, sin)

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

type Tag = String

forward :: Number -> TurtleM Unit
forward n = do
  startTurtle <- get
  let startPosition = startTurtle.position
  let angle = startTurtle.angle
  let endPosition = move angle n startPosition
  put $ startTurtle { position = endPosition }
  tell [ "From " <> show startPosition <> " -> " <> show endPosition ]
  pure unit

right :: Radians -> TurtleM Unit
right delta = do
  startTurtle <- get
  let angle = startTurtle.angle
  let newAngle = angle + delta
  put $ startTurtle { angle = newAngle }
  pure unit

move :: Radians -> Number -> Position -> Position
move angle distance (Position {x, y}) =
  Position { x: newX, y: newY }
  where
    newX = cos angle * distance
    newY = sin angle * distance

drawing1 :: TurtleM Unit
drawing1 = do
  forward 100.0
  right 90.0
  forward 100.0
  right 135.0
  forward 50.0
