module Main where

import           HTeX.Language.Tokenizer

main :: IO ()
main = let
  cases = fmap (tokenize "test")
    [ "\\word"
    , "\\ "
    , "\\3"
    , "\\\\"
    , "\\"
    , "\\word     \n\\    \\3 \\\\"
    , "{"
    , "}"
    , "{}"
    , "{\\word}"
    , "hello"
    -- , "\\word hello"
    -- , "\\321\\hello"
    -- , "\\begin{document}"
    ]
  in foldMap (putStrLn . show) cases
