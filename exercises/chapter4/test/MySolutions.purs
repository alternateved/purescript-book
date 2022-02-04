module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (all, filter, foldl, head, null, range, tail, (..), (:))
import Data.Maybe (Maybe, fromMaybe)
import Data.Path (Path, filename, isDirectory, ls)
import Math (pow)
import Test.Examples (allFiles)

-- (Easy) Write a recursive function isEven which returns true if and only if its
-- input is an even integer.

isEven :: Int -> Boolean
isEven = eq 0 <<< flip mod 2

-- (Medium) Write a recursive function countEven which counts the number of even
-- integers in an array. Hint: the function head (also available in Data.Array) can
-- be used to find the first element in a non-empty array.

-- FIXME This implementation causes stack overflow with inputs exceeding 7123 elements
countEven :: Array Int -> Int
countEven arr =
  if null arr then
    0
  else if isEven $ fromMaybe 1 $ head arr then
    1 + (countEven $ fromMaybe [] $ tail arr)
  else
    countEven $ fromMaybe [] $ tail arr

-- (Easy) Write a function squared which calculates the squares of an array of
-- numbers. Hint: Use the map or <$> function.

squared :: Array Number -> Array Number
squared = map $ flip pow 2.0

-- (Easy) Write a function keepNonNegative which removes the negative numbers from
-- an array of numbers. Hint: Use the filter function.

isPositive :: Number -> Boolean
isPositive = (_ >= 0.0)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter isPositive

-- (Medium) Define an infix synonym <$?> for filter. Note: Infix synonyms may not
-- be defined in the REPL, but you can define it in a file. Write a
-- keepNonNegativeRewrite function, which is the same as keepNonNegative, but
-- replaces filter with your new infix operator <$?>. Experiment with the
-- precedence level and associativity of your operator in PSCi. Note: There are no
-- unit tests for this step.

infixl 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = (isPositive <$?> _)

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [ i, j ]

-- (Easy) Write a function isPrime which tests if its integer argument is prime or
-- not. Hint: Use the factors function.

isPrime :: Int -> Boolean
isPrime n =
  if n <= 1 then false
  else factors n == [ [ 1, n ] ]

-- (Medium) Write a function cartesianProduct which uses do notation to find the
-- cartesian product of two arrays, i.e. the set of all pairs of elements a, b,
-- where a is an element of the first array, and b is an element of the second.

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct as bs = do
  a <- as
  b <- bs
  pure [ a, b ]

-- (Medium) Write a function triples :: Int -> Array (Array Int) which takes a
-- number n and returns all Pythagorean triples whose components (the a, b and c
-- values) are each less than or equal to n. A Pythagorean triple is an array of
-- numbers [a, b, c] such that a² + b² = c². Hint: Use the guard function in an
-- array comprehension.

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

-- (Difficult) Write a function primeFactors which produces the prime factorization
-- of n, i.e. the array of prime integers whose product is n. Hint: for an integer
-- greater than 1, break the problem down into two subproblems: finding the first
-- factor, and finding the remaining factors.

primeFactors :: Int -> Array Int
primeFactors n = findFactor n 2
  where
  findFactor n' d =
    if n' <= 1 then []
    else if n' `mod` d == 0 then d : findFactor (n' `div` d) d
    else findFactor n' (d + 1)

-- (Easy) Write a function allTrue which uses foldl to test whether an array of
-- boolean values are all true.

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

-- (Medium - No Test) Characterize those arrays xs for which the function foldl
-- (==) false xs returns true. In other words, complete the sentence.

-- "The function returns true when xs contains odd number of false."

-- (Medium) Write a function fibTailRec which is the same as fib but in tail
-- recursive form. Hint: Use an accumulator parameter.

fibTailRec :: Int -> Int
fibTailRec n = recCheck n 0 1
  where
  recCheck n' previous current =
    if n' == 0 then previous
    else if n' == 1 then current
    else recCheck (n' - 1) current (previous + current)

-- (Medium) Write reverse in terms of foldl.

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [ x ] <> xs) []

-- (Easy) Write a function onlyFiles which returns all files (not directories) in
-- all subdirectories of a directory.

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

-- (Medium) Write a function whereIs to search for a file by name. The function
-- should return a value of type Maybe Path, indicating the directory containing
-- the file, if it exists.
-- Hint: Try to write this function as an array comprehension using do notation.

onlyDirectories :: Path -> Array Path
onlyDirectories = filter isDirectory <<< allFiles

whereIs :: Path -> String -> Maybe Path
whereIs path name = head $ do
  path' <- onlyDirectories path
  child <- ls path'
  guard $ filename child == filename path' <> name
  pure path'
