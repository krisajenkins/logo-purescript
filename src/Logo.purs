module Logo where

import Prelude
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.RWS (tell)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (WriterT, execWriter)
import Control.Monad.Writer.Trans (class MonadTell)
import Data.Identity (Identity)
import Data.Lens (assign, lens, modifying, use)
import Data.Lens.Types (Lens')
import Math (Radians, cos, pi, sin)

------------------------------------------------------------
-- The Monad.
------------------------------------------------------------

data TurtleF a
  = Forward Number a
  | TurnLeft Degrees a
  | TurnRight Degrees a

type TurtleT m a = FreeT TurtleF m a
type Turtle a = FreeT TurtleF Identity a

derive instance turtleTFunctor :: Functor TurtleF
derive instance turtleTEq :: Eq a => Eq (TurtleF a)


forward :: forall m. Monad m => Number -> FreeT TurtleF m Unit
forward n = liftFreeT $ Forward n unit

turnRight :: forall m. Monad m => Number -> TurtleT m Unit
turnRight degrees = liftFreeT $ TurnRight degrees unit

------------------------------------------------------------
-- The toString interpreter.
------------------------------------------------------------

logCommands :: forall m a. MonadTell (Array String) m => TurtleF a -> m a
logCommands (Forward n next) = do
  tell [ "Forward " <> show n ]
  pure next
logCommands (TurnLeft n next) = do
  tell [ "TurnLeft " <> show n ]
  pure next
logCommands (TurnRight n next) = do
  tell [ "TurnRight " <> show n ]
  pure next

runToStrings :: forall a. FreeT TurtleF (WriterT (Array String) Identity) a -> Array String
runToStrings =
  execWriter <<< runFreeT logCommands

------------------------------------------------------------
-- The to "thing that can be easily rendered" interpreter.
------------------------------------------------------------

data TurtleState = TurtleState
  { angle :: Radians
  , position :: Position
  }

type Degrees = Number

initialState :: TurtleState
initialState = TurtleState
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

_position :: Lens' TurtleState Position
_position = lens get set
  where get (TurtleState {position}) = position
        set (TurtleState turtle) position = TurtleState $ turtle {position = position}

_angle :: Lens' TurtleState Radians
_angle = lens get set
  where get (TurtleState {angle}) = angle
        set (TurtleState turtle) angle = TurtleState $ turtle {angle = angle}

degreesToRadians :: Degrees -> Radians
degreesToRadians =
  (*) (pi / 180.0)

move :: Number -> Radians -> Position -> Position
move distance angle (Position {x, y}) =
  Position { x: x + newX, y: y + newY }
  where
    newX = cos angle * distance
    newY = sin angle * distance

data Drawing = Line Position Position

toDrawings :: forall m a. MonadState TurtleState m => MonadTell (Array Drawing) m => TurtleF a -> m a
toDrawings (Forward n next) = do
  startPosition <- use _position
  startAngle <- use _angle
  let endPosition = move n startAngle startPosition
  assign _position endPosition
  tell [ Line startPosition endPosition ]
  pure next

toDrawings (TurnRight delta next) = do
  modifying _angle ((+) (degreesToRadians delta))
  pure next

toDrawings (TurnLeft delta next) = do
  modifying _angle ((-) (degreesToRadians delta))
  pure next

runToDrawings :: forall a m. MonadRec m => MonadState TurtleState m => MonadTell (Array Drawing) m => TurtleT m a -> m a
runToDrawings = runFreeT toDrawings
