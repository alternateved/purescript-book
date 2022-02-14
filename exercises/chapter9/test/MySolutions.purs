module Test.MySolutions where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parOneOf, parTraverse)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (fold, traverse)
import Effect.Aff (Aff, Error, attempt, delay)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)

-- (Easy) Write a concatenateFiles function which concatenates two text files.

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles file1 file2 target = do
  f1 <- readTextFile UTF8 file1
  f2 <- readTextFile UTF8 file2
  writeTextFile UTF8 target (f1 <> f2)

-- (Medium) Write a function concatenateMany to concatenate multiple text files,
-- given an array of input file names and an output file name. Hint: use traverse.

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany files target = do
  fs <- traverse (readTextFile UTF8) files
  writeTextFile UTF8 target (fold fs)

-- (Medium) Write a function countCharacters :: FilePath -> Aff (Either Error Int)
-- that returns the number of characters in a file, or an error if one is
-- encountered.

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters file = attempt do
  result <- readTextFile UTF8 file
  pure $ length result

-- (Easy) Write a function writeGet which makes an HTTP GET request to a provided
-- url, and writes the response body to a file.

writeGet :: String -> FilePath -> Aff Unit
writeGet url target = do
  result <- AX.get ResponseFormat.string url
  case result of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> writeTextFile UTF8 target response.body

-- (Easy) Write a concatenateManyParallel function which has the same signature as
-- the earlier concatenateMany function, but reads all input files in parallel.

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel files target = do
  fs <- parTraverse (readTextFile UTF8) files
  writeTextFile UTF8 target (fold fs)

-- (Medium) Write a getWithTimeout :: Number -> String -> Aff (Maybe String)
-- function which makes an HTTP GET request at the provided URL and returns either:
-- Nothing: if the request takes longer than the provided timeout .
-- The string response: if the request succeeds before the timeout elapses.

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout ms url =
  parOneOf
    [ map (map _.body) $ map hush $ AX.get ResponseFormat.string url
    , Nothing <$ delay (Milliseconds ms)
    ]
