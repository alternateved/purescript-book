module Example.Random where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (random)
import Graphics.Canvas (Context2D, arc, fillPath, getCanvasElementById, getContext2D, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  -- (Easy) Write a higher-order function which strokes and fills a path
  -- simultaneously. Rewrite the Random.purs example using your function.
  let
    strokeAndFill :: Context2D -> Effect Unit -> Effect Unit
    strokeAndFill context path = do
      fillPath context path
      strokePath context path

  setFillStyle ctx "#2b6bff"
  setStrokeStyle ctx "#000"

  for_ (1 .. 100) \_ -> do
    x <- random
    y <- random
    r <- random

    let
      path = arc ctx
        { x: x * 600.0
        , y: y * 600.0
        , radius: r * 50.0
        , start: 0.0
        , end: Math.tau
        }

    strokeAndFill ctx path
