module Test.MySolutions where

import Prelude

import Data.Array (length, nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Formatter.Internal (repeat)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hashCode, hashEqual)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)

-- (Easy) Define a Show instance for Point. Match the same output as the showPoint
-- function from the previous chapter. Note: Point is now a newtype (instead of a
-- type synonym), which allows us to customize how to show it. Otherwise, we'd be
-- stuck with the default Show instance for records.

newtype Point = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

-- (Easy) Define a Show instance for Complex. Match the output format expected by
-- the tests (e.g. 1.2+3.4i, 5.6-7.8i, etc.).

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary })
    | imaginary < 0.0 = show real <> show imaginary <> "i"
    | otherwise = show real <> "+" <> show imaginary <> "i"

-- (Easy) Derive an Eq instance for Complex. Note: You may instead write this
-- isntance manually, but why do more work if you don't have to?

derive instance eqComplex :: Eq Complex

-- (Medium) Define a Semiring instance for Complex. Note: You can use wrap and
-- over2 from Data.Newtype to create a more concise solution. If you do so, you
-- will also need to import class Newtype from Data.Newtype and derive a Newtype
-- instance for Complex.

instance semiringComplex :: Semiring Complex where
  add (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) =
    Complex { real: r1 + r2, imaginary: i1 + i2 }
  mul (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) =
    Complex { real: r3, imaginary: i3 }
    where
    r3 = firsts + lasts
    i3 = outers + inners
    firsts = r1 * r2
    outers = r1 * i2
    inners = r2 * i1
    lasts = i1 * i2 * -1.0
  zero = Complex { real: 0.0, imaginary: 0.0 }
  one = Complex { real: 1.0, imaginary: 0.0 }

-- (Easy) Derive (via newtype) a Ring instance for Complex. Note: You may instead
-- write this instance manually, but that's not as convenient.

derive newtype instance ringComplex :: Ring Complex

-- (Medium) Derive (via Generic) a Show instance for Shape. How does the amount of
-- code written and String output compare to showShape from the previous chapter?
-- Hint: See the Deriving from Generic section of the Type Class Deriving guide.

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

-- (Easy) Write an Eq instance for the type NonEmpty a which reuses the instances
-- for Eq a and Eq (Array a). Note: you may instead derive the Eq instance.

data NonEmpty a = NonEmpty a (Array a)

derive instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty e1 a1) = show e1 <> " " <> show a1

-- (Medium) Write a Semigroup instance for NonEmpty a by reusing the Semigroup
-- instance for Array.

instance semigroupNonEmpty :: Semigroup (Array a) => Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (append a1 (append [ e2 ] a2))

-- (Medium) Write a Functor instance for NonEmpty.

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty e1 a1) = NonEmpty (f e1) (map f a1)

-- (Medium) Write an Ord instance for Extended a which reuses the Ord instance for a.

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare (Finite a) (Finite b) = compare a b

-- (Difficult) Write a Foldable instance for NonEmpty. Hint: reuse the Foldable
-- instance for arrays.

-- class Foldable f where
--   foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
--   foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
--   foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f z (NonEmpty e a) = foldr f z (append [ e ] a)
  foldl f z (NonEmpty e a) = foldl f z (append [ e ] a)
  foldMap f (NonEmpty e a) = f e <> foldMap f a

-- (Difficult) Given a type constructor f which defines an ordered container (and
-- so has a Foldable instance), we can create a new container type which includes
-- an extra element at the front. The container OneMore f also has an ordering,
-- where the new element comes before any element of f. Write a Foldable instance
-- for OneMore f.

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f z (OneMore e c) = f e (foldr f z c)
  foldl f z (OneMore e c) = foldl f (f z e) c
  foldMap f (OneMore e c) = f e <> foldMap f c

-- (Medium) Write a dedupShapes :: Array Shape -> Array Shape function which
-- removes duplicate Shapes from an array using the nubEq function.

derive instance eqPoint :: Eq Point
derive instance eqShape :: Eq Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

-- (Medium) Write a dedupShapesFast function which is the same as dedupShapes, but
-- uses the more efficient nub function.

derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

-- (Medium) Define a partial function unsafeMaximum :: Partial => Array Int -> Int
-- which finds the maximum of a non-empty array of integers. Test out your function
-- in PSCi using unsafePartial. Hint: Use the maximum function from Data.Foldable.

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs = case maximum xs of
  Just max -> max

-- (Medium) Write an instance which implements this action:
-- instance actionMultiplyInt :: Action Multiply Int where

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

derive instance genericMultiply :: Generic Multiply _
instance showMultiply :: Show Multiply where
  show = genericShow

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply n) m = n * m

-- (Difficult) There are actually multiple ways to implement an instance of Action
-- Multiply Int. How many can you think of? Purescript does not allow multiple
-- implementations of a same instance, so you will have to replace your original
-- implementation. Note: the tests cover 4 implementations.

-- instance actionMultiplyInt :: Action Multiply Int where
--   act (Multiply n) m = m / n

-- (Medium) Write an Action instance which repeats an input string some number of
-- times. Hint: Search Pursuit for a helper-function with the signature String ->
-- Int -> String. Note that String might appear as a more generic type (such as
-- Monoid). Does this instance satisfy the laws listed above?

instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) s = repeat s n

-- (Medium) Write an instance Action m a => Action m (Array a), where the action on
-- arrays is defined by acting on each array element independently.

instance Action m a => Action m (Array a) where
  act m xs = map (act m) xs

-- (Medium) Write a function arrayHasDuplicates which tests if an array has any
-- duplicate elements based on both hash and value equality. First check for hash
-- equality with the hashEqual function, then check for value equality with == if a
-- duplicate pair of hashes is found. Hint: the nubByEq function in Data.Array
-- should make this task much simpler.

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates xs = length xs /= (length $ nubByEq checkHash xs)
  where
  checkHash a b = hashEqual a b && a == b

-- (Medium) Write a Hashable instance for the following newtype which satisfies the
-- type class law. The newtype Hour and its Eq instance represent the type of
-- integers modulo 12, so that 1 and 13 are identified as equal, for example. Prove
-- that the type class law holds for your instance.

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour h) = hashCode $ mod h 12
