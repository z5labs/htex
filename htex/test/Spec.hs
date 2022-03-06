import           Test.HUnit

import qualified Test.HTeX.Language.Tokenizer as Tokenizer

main :: IO ()
main = let
  tests = Tokenizer.tests
  in runTestTTAndExit tests
