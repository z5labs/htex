module Test.HTeX.Language.Tokenizer
    ( tests
    ) where

import           HTeX.Language.Tokenizer

import           Test.HUnit

assertEither :: (a -> Assertion) -> (b -> Assertion) -> Either a b -> Assertion
assertEither l r = either l r

tests = TestList $ fmap toTest testData
  where
    toTest (lbl, src, toks) = TestLabel lbl $ toTestCase lbl toks src
    toTestCase lbl toks = TestCase . assertEither (assertFailure . show) (assertEqual "" toks) . tokenize lbl

testData =
  [ ( "ctrlWord"
    , "\\word"
    , [CtrlSeq '\\' (CtrlWord "word")]
    )
  , ( "backToBackCtrlWords"
    , "\\hello\\world"
    , [CtrlSeq '\\' (CtrlWord "hello"), CtrlSeq '\\' (CtrlWord "world")]
    )
  , ( "ctrlSymbolSpace"
    , "\\ "
    , [CtrlSeq '\\' (CtrlSymbol ' ')]
    )
  , ( "ctrlSymbolNumber"
    , "\\3"
    , [CtrlSeq '\\' (CtrlSymbol '3')]
    )
  , ( "ctrlSymbolSpecialChar"
    , "\\\\"
    , [CtrlSeq '\\' (CtrlSymbol '\\')]
    )
  , ( "backToBackCtrlSymbols"
    , "\\1\\2"
    , [CtrlSeq '\\' (CtrlSymbol '1'), CtrlSeq '\\' (CtrlSymbol '2')]
    )
  , ( "ctrlWordWithoutWith"
    , "\\"
    , [CtrlSeq '\\' (CtrlWord "")]
    )
  , ( "spaceSeparatedCtrlSeqs"
    , "\\word     \n\\    \\3 \\\\"
    , [CtrlSeq '\\' (CtrlWord "word"), CtrlSeq '\\' (CtrlSymbol ' '), CtrlSeq '\\' (CtrlSymbol '3'), CtrlSeq '\\' (CtrlSymbol '\\')]
    )
  , ( "spaces"
    , "     "
    , []
    )
  , ( "startWithWhitespace"
    , "\n   \t \r\n \\hello"
    , [CtrlSeq '\\' (CtrlWord "hello")]
    )
  , ( "endWithWhitespace"
    , "\\hello   \n"
    , [CtrlSeq '\\' (CtrlWord "hello")]
    )
  , ( "empty"
    , ""
    , []
    )
  , ( "groupStart"
    , "{"
    , [StartGroup '{']
    )
  , ( "groupEnd"
    , "}"
    , [EndGroup '}']
    )
  , ( "emptyGroup"
    , "{}"
    , [StartGroup '{', EndGroup '}']
    )
  , ( "groupFilledWithSpaces"
    , "{    }"
    , [StartGroup '{', EndGroup '}']
    )
  , ( "groupContainingCtrlSeq"
    , "{\\word}"
    , [StartGroup '{', CtrlSeq '\\' (CtrlWord "word"), EndGroup '}']
    )
  , ( "basicLiteral"
    , "hello"
    , [Literal "hello"]
    )
  , ( "literalAfterCtrlWord"
    , "\\word hello"
    , [CtrlSeq '\\' (CtrlWord "word"), Literal "hello"]
    )
  , ( "literalAfterCtrlSymbol"
    , "\\word hello"
    , [CtrlSeq '\\' (CtrlWord "word"), Literal "hello"]
    )
  , ( "literalBetweenCtrlSeqs"
    , "\\321\\hello"
    , [CtrlSeq '\\' (CtrlSymbol '3'), Literal "21", CtrlSeq '\\' (CtrlWord "hello")]
    )
  , ( "literalInGroupAfterCtrlSeq"
    , "\\begin{document}"
    , [CtrlSeq '\\' (CtrlWord "begin"), StartGroup '{', Literal "document", EndGroup '}']
    )
  ]
