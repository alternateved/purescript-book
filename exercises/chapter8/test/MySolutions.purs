module Test.MySolutions where

import Prelude

import Control.Monad.ST (for, run)
import Control.Monad.ST.Ref (modify, new, read)
import Data.Array (foldM, head, nub, sort, tail, (!!))
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (error, throwException)

-- Easy) Write a function third which returns the third element of an array with
-- three or more elements. Your function should return an appropriate Maybe type.
-- Hint: Look up the types of the head and tail functions from the Data.Array
-- module in the arrays package. Use do notation with the Maybe monad to combine
-- these functions.

third :: forall a. Array a -> Maybe a
third = (_ !! 2)

third' :: forall a. Array a -> Maybe a
third' xs = do
  xs1 <- tail xs
  xs2 <- tail xs1
  head xs2

-- (Medium) Write a function possibleSums which uses foldM to determine all
-- possible totals that could be made using a set of coins. The coins will be
-- specified as an array which contains the value of each coin. Hint: This function
-- can be written as a one-liner using foldM. You might want to use the nub and
-- sort functions to remove duplicates and sort the result respectively.

possibleSums :: Array Int -> Array Int
possibleSums = nub <<< sort <<< foldM (\b a -> [ b, b + a ]) 0

-- (Medium) Write a function filterM which generalizes the filter function on
-- lists. Your function should have the following type signature:

-- filterM f (x : xs) = do
--   b <- f x
--   xs' <- filterM f xs
--   pure if b then x : xs' else xs'

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM p (x : xs) = do
  b <- p x
  xs' <- filterM p xs
  pure if b then x : xs' else xs'

-- (Medium) Rewrite the safeDivide function as exceptionDivide and throw an
-- exception using throwException with the message "div zero" if the denominator is
-- zero.

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide a b = pure (a / b)

-- (Medium) Write a function estimatePi :: Int -> Number that uses n terms of the
-- Gregory Series to calculate an approximation of pi. Hints: You can pattern your
-- answer like the definition of simulate above. You might need to convert an Int
-- into a Number using toNumber :: Int -> Number from Data.Int.

estimatePi :: Int -> Number
estimatePi n = run do
  ref <- new { r: 0.0, d: 1.0, s: 1.0 }
  for 0 n \_ ->
    modify
      ( \o ->
          { r: o.r + (o.s / o.d)
          , d: o.d + 2.0
          , s: negate o.s
          }
      )
      ref
  final <- read ref
  pure $ 4.0 * final.r

-- (Medium) Write a function fibonacci :: Int -> Int to compute the nth Fibonacci
-- number, using ST to track the values of the previous two Fibonacci numbers.
-- Using PSCi, compare the speed of your new ST-based implementation against the
-- recursive implementation (fib) from Chapter 4.

fibonacci :: Int -> Int
fibonacci n = run do
  ref <- new { prev: 0, curr: 1 }
  for 2 (n + 1) \_ ->
    modify
      ( \o ->
          { prev: o.curr
          , curr: o.prev + o.curr
          }
      )
      ref
  final <- read ref
  pure final.curr

fibTailRec :: Int -> Int
fibTailRec n = recCheck n 0 1
  where
  recCheck n' previous current =
    if n' == 0 then previous
    else if n' == 1 then current
    else recCheck (n' - 1) current (previous + current)
