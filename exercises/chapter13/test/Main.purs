module Test.Main where

import Prelude

import Data.Array (length, null, sort, sortBy)
import Data.Foldable (foldr)
import Data.Function (on)
import Data.List (List(..), fromFoldable)
import Effect (Effect)
import Merge (mergeWith, mergePoly, merge)
import Sorted (sorted)
import Test.QuickCheck (quickCheck, (<?>))
import Tree (Tree, member, insert, toArray, anywhere)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

ints :: Array Int -> Array Int
ints = identity

-- (Easy) Write a function bools which forces the types of xs and ys to be Array
-- Boolean, and add additional properties which test mergePoly at that type.

bools :: Array Boolean -> Array Boolean
bools = identity

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = identity

treeOfInt :: Tree Int -> Tree Int
treeOfInt = identity

main :: Effect Unit
main = do
  -- Tests for module 'Merge'

  quickCheck \xs ys ->
    let
      result = merge (sort xs) (sort ys)
      expected = sort $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  -- (Easy) Write a property which asserts that merging an array with the empty array
  -- does not modify the original array.

  -- (Easy) Add an appropriate error message to the remaining property for merge.

  quickCheck \xs ->
    eq (merge (sort xs) []) (sort xs)
      <?> "Expected: merging with empty array does not modify original array"

  quickCheck \xs ys ->
    eq (merge (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)
      <?> "Expected: merge works with newtype"

  quickCheck \xs ys ->
    eq (ints $ mergePoly (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)
      <?> "Expected: merge works with arrays of polymorphic types if they have an Ord instance (variant Int)"

  quickCheck \xs ys ->
    eq (bools $ mergePoly (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)
      <?> "Expected: merge works with arrays of polymorphic types if they have an Ord instance (variant Boolean)"

  -- (Medium) Choose a pure function from the core libraries (for example, from the
  -- arrays package), and write a QuickCheck property for it, including an
  -- appropriate error message. Your property should use a helper function to fix any
  -- polymorphic type arguments to either Int or Boolean.

  quickCheck \xs ->
    eq (null $ bools xs) (length xs == 0) <?> "Expected: null returns true if array is empty"

  quickCheck \xs ys f ->
    let
      result = map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
      expected = map f $ sortBy (compare `on` f) $ xs <> ys
    in
      eq result expected

  -- Tests for module 'Tree'

  quickCheck \t a -> member a $ insert a $ treeOfInt t
  quickCheck \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfInt t) || anywhere g t
