module Scheme.TestUtils where

import Data.Text (Text)
import Scheme (SchemeVal, pExpr, run, showVal)
import Text.Megaparsec (ParseErrorBundle, Parsec, parse)

-- | Test utility to run a parser on some input
testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse p = parse p ""

-- | Run parser and return AST
testRun :: Text -> SchemeVal
testRun input = case parse pExpr "" input of
  Right val -> case run val of
    Right out -> out
    Left _ -> error "Error"
  Left _ -> error "Error"

-- | Run parser and return output
testRunOutput :: Text -> Text
testRunOutput input = case parse pExpr "" input of
  Right val -> case run val of
    Right out -> showVal out
    Left _ -> error "Error"
  Left _ -> error "Error"
