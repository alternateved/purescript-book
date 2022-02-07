module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Person (Person)
import Data.Picture (Point, Shape(..), getCenter)
import Math (pi)

-- (Easy) Write the factorial function using pattern matching. Hint: Consider the
-- two corner cases of zero and non-zero inputs. Note: This is a repeat of an
-- example from the previous chapter, but see if you can rewrite it here on your
-- own.

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- (Medium) Write a function binomial which finds the coefficient of the x^kth
-- term in the polynomial expansion of (1 + x)^n. This is the same as the number of
-- ways to choose a subset of k elements from a set of n elements. Use the formula
-- n! / k! (n - k)!, where ! is the factorial function written earlier. Hint: Use
-- pattern matching to handle corner cases. If it takes a long time to complete or
-- crashes with an error about the call stack, try adding more corner cases.

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k
  | n < k = 0
  | otherwise = factorial n / (factorial k * factorial (n - k))

-- (Medium) Write a function pascal which uses Pascal`s Rule for computing the same
-- binomial coefficients as the previous exercise.

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

-- (Easy) Write a function sameCity which uses record patterns to test whether two
-- Person records belong to the same city.

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: a } } { address: { city: b } } = a == b

-- (Medium) Write a function fromSingleton which uses an array literal pattern to
-- extract the sole member of a singleton array. If the array is not a singleton,
-- your function should return a provided default value. Your function should have
-- type forall a. a -> Array a -> a

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x
fromSingleton y _ = y

-- (Easy) Write a function circleAtOrigin which constructs a Circle (of type Shape)
-- centered at the origin with radius 10.0.

origin :: Point
origin = { x: 0.0, y: 0.0 }

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

-- (Medium) Write a function doubleScaleAndCenter which scales the size of a Shape
-- by a factor of 2.0 and centers it at the origin.

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ s1 s2) = Rectangle origin s1 s2
centerShape line@(Line s e) = Line (s - delta) (e - delta)
  where
  delta = getCenter line
centerShape (Text _ text) = Text origin text

doubleShape :: Shape -> Shape
doubleShape (Circle p r) = Circle p (r * 2.0)
doubleShape (Rectangle p s1 s2) = Rectangle p (s1 * 2.0) (s2 * 2.0)
doubleShape (Line p1 p2) = Line (p1 * double) (p2 * double)
  where
  double = { x: 2.0, y: 2.0 }
doubleShape text = text

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< doubleShape

-- (Medium) Write a function shapeText which extracts the text from a Shape. It
-- should return Maybe String, and use the Nothing constructor if the input is not
-- constructed using Text.

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _ = Nothing

-- (Easy) Define Watt as a newtype of Number. Then define a calculateWattage
-- function using this new Watt. A wattage in Watts can be calculated as the product
-- of a given current in Amps and a given voltage in Volts.

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)

-- (Medium) Extend the vector graphics library with a new operation area which
-- computes the area of a Shape. For the purpose of this exercise, the area of a
-- line or a piece of text is assumed to be zero.

area :: Shape -> Number
area (Circle _ r) = pi * r * r
area (Rectangle _ s1 s2) = (s1 * s2) / 2.0
area _ = 0.0
