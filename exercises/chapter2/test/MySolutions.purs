module Test.MySolutions where

import Prelude
import Data.Int (rem)
import Math (sqrt, pi)

-- (Medium) Write a diagonal function to compute the length of the diagonal (or
-- hypotenuse) of a right-angled triangle when given the lengths of the two other
-- sides.

diagonal :: Number -> Number -> Number
diagonal a b = sqrt $ square a + square b

square :: Number -> Number
square x = x * x

-- (Easy) Write a function circleArea which computes the area of a circle with a
-- given radius. Use the pi constant, which is defined in the Math module. Hint:
-- don't forget to import pi by modifying the import Math statement.

circleArea :: Number -> Number
circleArea r = square r * pi

-- (Medium) Write a function leftoverCents which takes an Int and returns what's
-- leftover after dividing by 100. Use the rem function. Search Pursuit for this
-- function to learn about usage and which module to import it from. Note: Your IDE
-- may support auto-importing of this function if you accept the auto-completion
-- suggestion.

leftoverCents :: Int -> Int
leftoverCents n = rem n 100
