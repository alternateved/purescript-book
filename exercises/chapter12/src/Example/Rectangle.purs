module Example.Rectangle where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Graphics.Canvas
  ( rect
  , fillPath
  , setFillStyle
  , getContext2D
  , getCanvasElementById
  )
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#2b6bff"

  fillPath ctx $ rect ctx
    { x: 140.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  fillPath ctx $ rect ctx
    { x: 280.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }
