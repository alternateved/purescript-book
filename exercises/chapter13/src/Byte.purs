module Byte where

import Prelude

import Test.QuickCheck (class Coarbitrary, coarbitrary)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype Byte = Byte Int

instance arbitraryByte :: Arbitrary Byte where
  arbitrary = map intToByte arbitrary
    where
    intToByte n
      | n >= 0 = Byte (n `mod` 256)
      | otherwise = intToByte (-n)

instance coarbitraryByte :: Coarbitrary Byte where
  coarbitrary (Byte b) = coarbitrary b
