module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Array ((..))
import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)

-- (Medium) Write versions of the numeric operators +, -, * and / which work with
-- optional arguments (i.e. arguments wrapped in Maybe) and return a value wrapped
-- in Maybe. Name these functions addMaybe, subMaybe, mulMaybe, and divMaybe. Hint:
-- Use lift2.

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+)

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 (-)

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 (*)

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 (/)

-- (Medium) Extend the above exercise to work with all Apply types (not just
-- Maybe). Name these new functions addApply, subApply, mulApply, and divApply.

addApply :: forall a f. Applicative f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

subApply :: forall a f. Applicative f => Ring a => f a -> f a -> f a
subApply = lift2 (-)

mulApply :: forall a f. Applicative f => Semiring a => f a -> f a -> f a
mulApply = lift2 (*)

divApply :: forall a f. Applicative f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 (/)

-- (Difficult) Write a function combineMaybe which has type forall a f. Applicative
-- f => Maybe (f a) -> f (Maybe a). This function takes an optional computation
-- with side-effects, and returns a side-effecting computation which has an
-- optional result.

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just xs) = map Just xs

-- (Easy) Write a regular expression stateRegex :: Regex to check that a string
-- only contains two alphabetic characters. Hint: see the source code for
-- phoneNumberRegex.

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

-- (Medium) Write a regular expression nonEmptyRegex :: Regex to check that a
-- string is not entirely whitespace. Hint: If you need help developing this regex
-- expression, check out RegExr which has a great cheatsheet and interactive test
-- environment.

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

-- (Medium) Write a function validateAddressImproved that is similar to
-- validateAddress, but uses the above stateRegex to validate the state field and
-- nonEmptyRegex to validate the street and city fields. Hint: see the source for
-- validatePhoneNumber for an example of how to use matches.

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = address
  <$> matches "Street" nonEmptyRegex a.street
  <*> matches "City" nonEmptyRegex a.city
  <*> matches "State" stateRegex a.state

-- (Easy) Write Eq and Show instances for the following binary tree data structure.
-- Recall from the previous chapter that you may either write these instances
-- manually or let the compiler derive them for you. There are many "correct"
-- formatting options for Show output. The test for this exercise expects the
-- following whitespace style. This happens to match the default formatting of
-- generic show, so you only need to make note of this if you're planning on
-- writing this instance manually.

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance eqTree :: Eq a => Eq (Tree a) where
  eq Leaf Leaf = true
  eq (Branch t1 a t2) (Branch t3 b t4) =
    t1 == t3 && a == b && t2 == t4
  eq _ _ = false

instance showTree :: Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch t1 a t2) = "(" <> "Branch " <> show t1 <> " " <> show a <> " " <> show t2 <> ")"

-- (Medium) Write a Traversable instance for Tree a, which combines side-effects
-- from left-to-right. Hint: There are some additional instance dependencies that
-- need to be defined for Traversable.

derive instance functorTree :: Functor Tree

instance foldableTree :: Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Branch t1 a t2) = foldr f (f a (foldr f z t2)) t1

  foldl _ z Leaf = z
  foldl f z (Branch t1 a t2) = foldl f (f (foldl f z t1) a) t2

  foldMap _ Leaf = mempty
  foldMap f (Branch t1 a t2) = foldMap f t1 <> f a <> foldMap f t2

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch t1 v t2) = Branch <$> traverse f t1 <*> f v <*> traverse f t2

  sequence Leaf = pure Leaf
  sequence (Branch t1 v t2) = Branch <$> sequence t1 <*> v <*> sequence t2

-- (Medium) Write a function traversePreOrder :: forall a m b. Applicative m => (a
-- -> m b) -> Tree a -> m (Tree b) that performs a pre-order traversal of the tree.
-- This means the order of effect execution is root-left-right, instead of
-- left-root-right as was done for the previous in-order traverse exercise. Hint:
-- No additional instances need to be defined, and you don't need to call any of
-- the the functions defined earlier. Applicative do notation (ado) is the easiest
-- way to write this function.

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch t1 v t2) = ado
  mv <- f v
  mt1 <- traversePreOrder f t1
  mt2 <- traversePreOrder f t2
  in Branch mt1 mv mt2

-- (Medium) Write a function traversePostOrder that performs a post-order traversal
-- of the tree where effects are executed left-right-root.

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch t1 v t2) = ado
  mt1 <- traversePostOrder f t1
  mt2 <- traversePostOrder f t2
  mv <- f v
  in Branch mt1 mv mt2

-- (Medium) Create a new version of the Person type where the homeAddress field is
-- optional (using Maybe). Then write a new version of validatePerson (renamed as
-- validatePersonOptionalAddress) to validate this new Person. Hint: Use traverse
-- to validate a field of type Maybe a.

type PersonOptionalAddress =
  { firstName :: String
  , lastName :: String
  , homeAddress :: Maybe Address
  , phones :: Array PhoneNumber
  }

personOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p =
  personOptionalAddress
    <$> nonEmpty "First Name" p.firstName
    <*> nonEmpty "Last Name" p.lastName
    <*> traverse validateAddress p.homeAddress
    <*> validatePhoneNumbers "Phone Numbers" p.phones

-- (Difficult) Write a function sequenceUsingTraverse which behaves like sequence,
-- but is written in terms of traverse.

sequenceUsingTraverse :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse = traverse identity

-- (Difficult) Write a function traverseUsingSequence which behaves like traverse,
-- but is written in terms of sequence.

traverseUsingSequence :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f = sequence <<< map f

-- Notes

-- class (Functor t, Foldable t) <= Traversable t where
--   traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
--   sequence :: forall a m. Applicative m => t (m a) -> m (t a)

-- l = 1 .. 5
-- f x = [ x, x ]
-- t = traverse f l
