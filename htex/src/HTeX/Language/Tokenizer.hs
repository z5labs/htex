module HTeX.Language.Tokenizer
    ( Token(..)
    , CtrlSeq(..)
    , Item
    , Tokenizer
    , tokenize
    ) where

import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           Text.Megaparsec      hiding (Token)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Pos  (SourcePos)

data CtrlSeq = CtrlWord String | CtrlSymbol Char
  deriving (Show, Eq)

data Token = CtrlSeq Char CtrlSeq | StartGroup Char | EndGroup Char | Literal String | EOF
  deriving (Show, Eq)

data Item = Item SourcePos Token

type Tokenizer = Parsec () String Token

ctrlSeq :: Char -> Tokenizer
ctrlSeq escapeChar = do
  char escapeChar
  fmap (CtrlSeq escapeChar) (ctrlSymbol <|> ctrlWord)
  where
    ctrlSymbol = fmap CtrlSymbol (satisfy $ not .  isLetter)
    ctrlWord = fmap CtrlWord (many letterChar)

startGroup :: Char -> Tokenizer
startGroup startChar = fmap StartGroup $ char startChar

endGroup :: Char -> Tokenizer
endGroup endChar = fmap EndGroup $ char endChar

literal :: [Char] -> Tokenizer
literal specialChars = fmap Literal $ manyTill anySingle $ satisfy isSpaceOrSpecialChar
  where
    isSpaceOrSpecialChar c = if isSpace c
      then True
      else any ((==) c) specialChars

tokenize :: String -> String -> Either (ParseErrorBundle String ()) [Token]
tokenize = runParser tokenize'
  where
    tokenize' = many $ token' '\\' '{' '}'
    token' escChar startChar endChar = do
      space
      ctrlSeq escChar
        <|> (startGroup startChar)
        <|> (endGroup endChar)
        <|> (literal [escChar, startChar, endChar])
