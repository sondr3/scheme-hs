module Scheme.TestUtils where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
import Scheme (SchemeVal, buildEnvironment, evalLineForm, runWithEnv, showError, showVal)
import Text.Megaparsec (ParseErrorBundle, Parsec, parse)

-- | Test utility to run a parser on some input
testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse p = parse p ""

-- | Run parser and return AST
testRun :: Text -> IO SchemeVal
testRun input = do
  env <- buildEnvironment
  case unsafePerformIO $ runWithEnv env (evalLineForm input) of
    Right val -> pure val
    Left err -> error $ T.unpack $ showError err

-- | Run parser and return output
testRunOutput :: Text -> IO Text
testRunOutput input = do
  res <- testRun input
  return $ showVal res
