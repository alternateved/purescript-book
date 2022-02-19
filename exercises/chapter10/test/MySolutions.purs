module Test.MySolutions where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Function.Uncurried (Fn3)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Test.Examples (Complex, Quadratic, Undefined)

-- (Medium) Write a JavaScript function volumeFn in the Test.MySolutions module
-- that finds the volume of a box. Use an Fn wrapper from Data.Function.Uncurried.

foreign import volumeFn :: Fn3 Number Number Number Number

-- (Medium) Rewrite volumeFn with arrow functions as volumeArrow.

foreign import volumeArrow :: Number -> Number -> Number -> Number

-- (Medium) Write a JavaScript function cumulativeSumsComplex (and corresponding
-- PureScript foreign import) that takes an Array of Complex numbers and returns
-- the cumulative sum as another array of complex numbers.

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

-- (Medium) Write the function toMaybe :: forall a. Undefined a -> Maybe a. This
-- function converts undefined to Nothing and a values to Justs.

foreign import toMaybeImpl :: forall a. (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Undefined a -> Maybe a

toMaybe :: forall a. Undefined a -> Maybe a
toMaybe = toMaybeImpl Just Nothing

-- (Medium) Write a JavaScript function and PureScript wrapper valuesOfMap :: Map
-- String Int -> Either JsonDecodeError (Set Int) that returns a Set of all the
-- values in a Map. Hint: The .values() instance method for Map may be useful in
-- your JavaScript code.

foreign import valuesOfMapJson :: Json -> Json

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = decodeJson <<< valuesOfMapJson <<< encodeJson

-- (Easy) Write a new wrapper for the previous JavaScript function with the
-- signature valuesOfMapGeneric :: forall k v. Map k v -> Either JsonDecodeError
-- (Set v) so it works with a wider variety of maps. Note that you'll need to add
-- some type class constraints for k and v. The compiler will guide you.

valuesOfMapGeneric :: forall k v. Ord v => DecodeJson v => EncodeJson (Map k v) => Map k v -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = decodeJson <<< valuesOfMapJson <<< encodeJson

-- (Medium) Write a parseAndDecodeArray2D :: String -> Either String (Array (Array
-- Int)) function to parse and decode a JSON string containing a 2D array, such as
-- "[[1, 2, 3], [4, 5], [6]]". Hint: You'll need to use jsonParser to convert the
-- String into Json before decoding.

parseAndDecodeArray2D :: String -> Either String (Array (Array Int))
parseAndDecodeArray2D s =
  case jsonParser s of
    Left err -> Left err
    Right result -> case decodeJson result of
      Left err -> Left $ show err
      Right arr -> Right arr

-- (Medium) Derive generic EncodeJson and DecodeJson instances for the Tree type.
-- Consult the argonaut docs for instructions on how to do this. Note that you'll
-- also need generic instances of Show and Eq to enable unit testing for this
-- exercise, but those should be straightforward to implement after tackling the
-- JSON instances.

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

-- Recursive instances cannot be written in point free style
instance encodeJsonTree :: EncodeJson a => EncodeJson (Tree a) where
  encodeJson tree = genericEncodeJson tree

instance decodeJsonTree :: DecodeJson a => DecodeJson (Tree a) where
  decodeJson tree = genericDecodeJson tree

instance showTree :: Show a => Show (Tree a) where
  show tree = genericShow tree

instance eqTree :: Eq a => Eq (Tree a) where
  eq tree = genericEq tree
