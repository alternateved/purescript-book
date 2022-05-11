module Example.LSystem where

import Prelude

import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle)
import Math as Math
import Partial.Unsafe (unsafePartial)

-- (Medium) Break the lsystem function into two smaller functions. The first should
-- build the final sentence using repeated application of concatMap, and the second
-- should use foldM to interpret the result.

lsystem
  :: forall a m s
   . Monad m
  => Array a
  -> (a -> Array a)
  -> (s -> a -> m s)
  -> Int
  -> s
  -> m s
lsystem init prod interpret 0 state = foldM interpret state init
lsystem init prod interpret n state = lsystem (concatMap prod init) prod interpret (n - 1) state

data Letter = L | R | F

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initial :: Sentence
initial = [ F, R, R, F, R, R, F, R, R ]

productions :: Letter -> Sentence
productions L = [ L ]
productions R = [ R ]
productions F = [ F, L, F, R, R, F, L, F ]

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    interpret :: State -> Letter -> Effect State
    interpret state L = pure $ state { theta = state.theta - Math.tau / 6.0 }
    interpret state R = pure $ state { theta = state.theta + Math.tau / 6.0 }
    interpret state F = do
      let
        x = state.x + Math.cos state.theta * 1.5
        y = state.y + Math.sin state.theta * 1.5
      lineTo ctx x y
      pure { x, y, theta: state.theta }

  -- (Easy) Modify the L-system example above to use fillPath instead of strokePath.
  -- Hint: you will need to include a call to closePath, and move the call to moveTo
  -- outside of the interpret function.

  setFillStyle ctx "#3168ff"

  fillPath ctx do
    moveTo ctx initialState.x initialState.y
    _ <- lsystem initial productions interpret 5 initialState
    closePath ctx
