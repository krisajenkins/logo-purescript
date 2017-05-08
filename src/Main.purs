module Main where

import Prelude (Unit, discard, show, unit, ($), (<>))
import Control.Monad.RWS (RWSResult(..), runRWS)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (intercalate)
import Logo

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let (RWSResult state result writer) = runRWS drawing1 unit initialTurtle
  log $ "Final: " <> show state.position <> " " <> show state.angle
  log "Steps"
  log "-----"
  log $ intercalate "\n" writer
