module HTeX.Language.Tokenizer
    ( Token(..)
    , CtrlSeq(..)
    , Item
    , Tokenizer
    , tokenize
    , literal
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
ctrlSeq escapeChar = char escapeChar *> fmap (CtrlSeq escapeChar) (ctrlSymbol <|> ctrlWord)
  where
    ctrlSymbol = fmap CtrlSymbol (satisfy $ not .  isLetter)
    ctrlWord = fmap CtrlWord (takeWhileP Nothing isLetter)

startGroup :: Char -> Tokenizer
startGroup = fmap StartGroup . char

endGroup :: Char -> Tokenizer
endGroup = fmap EndGroup . char

literal :: [Char] -> Tokenizer
literal specialChars = fmap Literal $ takeWhile1P Nothing (not . isSpaceOrSpecialChar)
  where
    isSpaceOrSpecialChar c = isSpace c || c `elem` specialChars

tokenize :: String -> String -> Either (ParseErrorBundle String ()) [Token]
tokenize = runParser tokenize'
  where
    tokenize' = many $ (token' '\\' '{' '}')
    token' escChar startChar endChar = do
      space
      (ctrlSeq escChar)
        <|> (startGroup startChar)
        <|> (endGroup endChar)
        <|> (literal [escChar, startChar, endChar])
