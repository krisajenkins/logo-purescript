module Svg where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML.Core (ElemName(..), AttrName(..), Namespace(..), attr, element)

svgNamespace = Just $ Namespace "http://www.w3.org/2000/svg"

svg = element svgNamespace (ElemName "svg")
line = element svgNamespace (ElemName "line")

viewBox = attr Nothing (AttrName "viewBox")
width = show >>> attr Nothing (AttrName "width")
height = show >>> attr Nothing (AttrName "height")
x1 = show >>> attr Nothing (AttrName "x1")
y1 = show >>> attr Nothing (AttrName "y1")
x2 = show >>> attr Nothing (AttrName "x2")
y2 = show >>> attr Nothing (AttrName "y2")
strokeWidth = show >>> attr Nothing (AttrName "stroke-width")
stroke = attr Nothing (AttrName "stroke")
