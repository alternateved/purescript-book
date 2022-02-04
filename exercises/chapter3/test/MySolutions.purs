module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook, filterByName)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

-- (Medium) Write a function findEntryByStreet :: String -> AddressBook -> Maybe
-- Entry which looks up an Entry given a street address. Hint reusing the existing
-- code in findEntry. Test your function in PSCi and by running spago test.

-- (Medium) Rewrite findEntryByStreet to replace filterEntry with the composition
-- (using <<< or >>>) of: a property accessor (using the _. notation); and a
-- function that tests whether its given string argument is equal to the given
-- street address.

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet st = head <<< filterByStreet st

filterByStreet :: String -> AddressBook -> AddressBook
filterByStreet street = filter byStreet
  where
  byStreet = eq street <<< _.address.street

-- (Medium) Write a function isInBook which tests whether a name appears in a
-- AddressBook, returning a Boolean value. Hint: Use PSCi to find the type of the
-- Data.List.null function, which tests whether a list is empty or not.

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fn ln = not <<< null <<< filterByName fn ln

-- (Difficult) Write a function removeDuplicates which removes "duplicate" address
-- book entries. We'll consider entries duplicated if they share the same first and
-- last names, while ignoring address fields. Hint: Use PSCi to find the type of
-- the Data.List.nubByEq function, which removes duplicate elements from a list
-- based on an equality predicate. Note that the first element in each set of
-- duplicates (closest to list head) is the one that is kept.

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq f
  where
  f :: Entry -> Entry -> Boolean
  f e1 e2 = e1.firstName == e2.firstName
    && e1.lastName == e2.lastName
