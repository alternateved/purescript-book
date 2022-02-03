module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook, filterByName)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet st = head <<< filterByStreet st

filterByStreet :: String -> AddressBook -> AddressBook
filterByStreet street = filter byStreet
  where
  byStreet = eq street <<< _.address.street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fn ln = not <<< null <<< filterByName fn ln

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq f
  where
  f :: Entry -> Entry -> Boolean
  f e1 e2 = e1.firstName == e2.firstName
    && e1.lastName == e2.lastName
