module Test.MySolutions where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (Reader, ReaderT, ask, local, runReader, runReaderT)
import Control.Monad.State (StateT, execState, get, modify_, put)
import Control.Monad.Writer (Writer, WriterT, execWriterT, runWriter, tell)
import Data.Array (fold, some)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), drop, joinWith, take)
import Data.String.CodeUnits (stripPrefix, toCharArray)
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple)

-- (Medium) A string of parentheses is balanced if it is obtained by either
-- concatenating zero-or-more shorter balanced strings, or by wrapping a shorter
-- balanced string in a pair of parentheses. Use the State monad and the traverse_
-- function to write a function which tests whether or not a String of parentheses
-- is balanced, by keeping track of the number of opening parentheses which have
-- not been closed.

testParens :: String -> Boolean
testParens s =
  compareParens
    $ flip execState 0
    $ traverse_ (\c -> modify_ $ countParens c) s'

  where
  s' = toCharArray s

  compareParens :: Int -> Boolean
  compareParens 0 = true
  compareParens _ = false

  countParens :: Char -> Int -> Int
  countParens '(' parens | parens >= 0 = parens + 1
  countParens ')' parens = parens - 1
  countParens _ parens = parens

-- (Easy) Write a function line which renders a function at the current indentation
-- level. Hint: use the ask function to read the current indentation level. The
-- power function from Data.Monoid may be helpful too.

type Level = Int

type Doc = Reader Level String

line :: String -> Doc
line s = do
  level <- ask
  pure $ (power "  " level) <> s

-- (Easy) Use the local function to write a function which increases the
-- indentation level for a block of code.

indent :: Doc -> Doc
indent = local (_ + 1)

-- (Medium) Use the sequence function defined in Data.Traversable to write a
-- function which concatenates a collection of documents, separating them with new
-- lines.

cat :: Array Doc -> Doc
cat = pure <<< joinWith "\n" <=< sequence

-- (Medium) Use the runReader function to write a function which renders a document
-- as a String.

render :: Doc -> String
render = flip runReader 0

-- (Medium) Rewrite the sumArray function above using the Writer monad and the
-- Additive Int monoid from the monoid package.

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \n -> tell $ Additive n

-- (Medium) The Collatz function is defined on natural numbers n as n / 2 when n is
-- even, and 3 * n + 1 when n is odd. For example, the iterated Collatz sequence
-- starting at 10 is as follows: 10, 5, 16, 8, 4, 2, 1, ...

collatz :: Int -> Tuple Int (Array Int)
collatz c = runWriter $ clatz c 0
  where
  clatz :: Int -> Int -> Writer (Array Int) Int
  clatz 1 i = do
    tell [ 1 ]
    pure i
  clatz n i = do
    tell [ n ]
    if mod n 2 == 0 then clatz (n / 2) (i + 1)
    else clatz (3 * n + 1) (i + 1)

-- (Easy) Use the ExceptT monad transformer over the Identity functor to write a
-- function safeDivide which divides two numbers, throwing an error (as the String
-- "Divide by zero!") if the denominator is zero.

safeDivide :: Int -> Int -> ExceptT String Identity Int
safeDivide a b
  | b == 0 = throwError "Divide by zero!"
  | otherwise = pure $ a / b

-- (Medium) Write a parser which matches a string as a prefix of the current state,
-- or fails with an error message. Your parser should work as follows:

-- > runParser (string "abc") "abcdef" (Right (Tuple (Tuple "abc" "def") ["The
-- state is abcdef"]))

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split :: Parser String
split = do
  s <- get
  tell [ "The state is " <> s ]
  case s of
    "" -> throwError [ "Empty string" ]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

string :: String -> Parser String
string prefix = do
  state <- get
  tell [ "The state is " <> state ]
  case stripPrefix (Pattern prefix) state of
    Nothing -> throwError [ "Could not parse" ]
    Just r -> put r *> pure prefix

-- (Difficult) Use the ReaderT and WriterT monad transformers to reimplement the
-- document printing library which we wrote earlier using the Reader monad.

-- Instead of using line to emit strings and cat to concatenate strings, use the
-- Array String monoid with the WriterT monad transformer, and tell to append a
-- line to the result. Use the same names as in the original implementation but
-- ending with an apostrophe (').

type Level' = Int
type Doc' = WriterT (Array String) (ReaderT Level' Identity) Unit

line' :: String -> Doc'
line' s = do
  level <- ask
  tell [ (power "  " level) <> s ]
  pure unit

indent' :: Doc' -> Doc'
indent' = local (_ + 1)

render' :: Doc' -> String
render' = joinWith "\n" <<< unwrap <<< flip runReaderT 0 <<< execWriterT

-- (Easy) Remove the calls to the lift function from your implementation of the
-- string parser. Verify that the new implementation type checks, and convince
-- yourself that it should.

-- (Medium) Use your string parser with the some combinator to write a parser
-- asFollowedByBs which recognizes strings consisting of several copies of the
-- string "a" followed by several copies of the string "b".

asFollowedByBs :: Parser String
asFollowedByBs = do
  as <- some $ string "a"
  bs <- some $ string "b"
  pure $ fold $ as <> bs

-- (Medium) Use the <|> operator to write a parser asOrBs which recognizes strings
-- of the letters a or b in any order.

asOrBs :: Parser String
asOrBs = fold <$> some (string "a" <|> string "b")
