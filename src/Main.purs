module Main where

import Prelude
import Data.Array as Array
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.State (execStateT)
import Control.Monad.Writer (execWriter)
import Control.Safely (replicateM_)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Component (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML (HTML, div_, h1_, li_, text)
import Halogen.VDom.Driver (runUI)
import Logo (Drawing(..), Position(..), TurtleT, forward, initialState, runToDrawings, runToStrings, turnRight)
import Svg (height, line, stroke, strokeWidth, svg, viewBox, width, x1, x2, y1, y2)

main :: forall eff. Eff (console :: CONSOLE, avar :: AVAR, dom :: DOM, exception :: EXCEPTION, ref :: REF | eff) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI root unit body

data Query a =
  Noop a

type State = {}

root :: forall aff. Component HTML Query Unit Void (Aff aff)
root = component
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }

init :: State
init = {}

eval :: forall eff. Query ~> ComponentDSL State Query Void (Aff eff)
eval (Noop next) = pure next

render :: State -> ComponentHTML Query
render state =
  div_
    [ h1_ [ text "Logo" ]
    , svg [ width 500
          , height 500
          , viewBox "-150 -150 300 300"
          ]
          drawing1Group
    , div_ (li_ <<< Array.singleton <<< text <$> drawingLog)
    ]
  where
    drawing1Group :: forall p i. Array (HTML p i)
    drawing1Group = map toSvgTag $ execWriter $ execStateT (runToDrawings drawing1) initialState
    drawingLog = runToStrings drawing1

toSvgTag :: forall p i. Drawing -> HTML p i
toSvgTag (Line (Position from) (Position to)) =
  line [ x1 from.x
       , y1 from.y
       , x2 to.x
       , y2 to.y
       , strokeWidth 2
       , stroke "black"
       ]
       []

drawing1 :: forall m. Monad m => TurtleT m Unit
drawing1 = replicateM_ 6 $ do
  turnRight 60.0
  star

star :: forall m. Monad m => TurtleT m Unit
star = replicateM_ 5 starPart

starPart :: forall m. Monad m => TurtleT m Unit
starPart = do
  forward 100.0
  turnRight 144.0
