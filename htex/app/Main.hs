module Main where

import           Control.Monad

import           HTeX.Language.Tokenizer

import           Options.Applicative

data Source = FilePath FilePath | StdInput

readTeX :: Source -> IO String
readTeX src = case src of
  StdInput      -> getContents
  FilePath path -> return path

tryReadSource s = case s of
  []  -> Nothing
  "-" -> Just StdInput
  _   -> Just $ FilePath s

tokensCmd = command "tokens" $ info opts $ progDesc "convert TeX to lexigraphical tokens."
  where
    opts = toTokens <$> argument (maybeReader tryReadSource) (metavar "SRC")

    toTokens :: Source -> IO ()
    toTokens src = (putStrLn . show . tokenize "src") =<< readTeX src

toCmd = command "to" $ info opts $ progDesc "convert TeX to..."
  where
    opts = hsubparser tokensCmd

htexCmd = info (opts <**> helper) $ progDesc "htex is a tool for working with TeX, written in Haskell."
  where
    opts = hsubparser toCmd

main :: IO ()
main = join $ execParser htexCmd
