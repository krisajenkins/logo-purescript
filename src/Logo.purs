module Logo where

import Prelude
import Control.Monad.RWS (RWS, tell)
import Data.Lens (assign, lens, modifying, use)
import Data.Lens.Types (Lens')
import Math (Radians, cos, pi, sin)

type TurtleM = RWS Unit (Array Tag) Turtle

initialTurtle :: Turtle
initialTurtle = Turtle
  { angle: 0.0
  , position: Position { x: 0.0, y: 0.0 }
  }

newtype Position = Position
  { x :: Number
  , y :: Number
  }

_x :: Lens' Position Number
_x = lens get set
  where get (Position {x}) = x
        set (Position position) x = Position $ position {x = x}

_y :: Lens' Position Number
_y = lens get set
  where get (Position {y}) = y
        set (Position position) y = Position $ position {y = y}

_position :: Lens' Turtle Position
_position = lens get set
  where get (Turtle {position}) = position
        set (Turtle turtle) position = Turtle $ turtle {position = position}


_angle :: Lens' Turtle Radians
_angle = lens get set
  where get (Turtle {angle}) = angle
        set (Turtle turtle) angle = Turtle $ turtle {angle = angle}

data Turtle = Turtle
  { angle :: Radians
  , position :: Position
  }

data Tag = Line Position Position

forward :: Number -> TurtleM Unit
forward n = do
  startPosition <- use _position
  startAngle <- use _angle
  let endPosition = move n startAngle startPosition
  assign _position endPosition
  tell [ Line startPosition endPosition ]
  pure unit

right :: Degrees -> TurtleM Unit
right delta = do
  modifying _angle ((+) (degreesToRadians delta))
  pure unit

left :: Degrees -> TurtleM Unit
left delta = do
  modifying _angle ((-) (degreesToRadians delta))
  pure unit

type Degrees = Number

degreesToRadians :: Degrees -> Radians
degreesToRadians =
  (*) (pi / 180.0)

move :: Number -> Radians -> Position -> Position
move distance angle (Position {x, y}) =
  Position { x: x + newX, y: y + newY }
  where
    newX = cos angle * distance
    newY = sin angle * distance
