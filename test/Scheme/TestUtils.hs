module Scheme.TestUtils where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
import Scheme (SchemeVal, buildEnvironment, lineToSchemeM, runWithEnv, showError, showVal)
import Text.Megaparsec (ParseErrorBundle, Parsec, parse)

-- | Test utility to run a parser on some input
testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse p = parse p ""

-- | Run parser and return AST
testRun :: Text -> SchemeVal
testRun input = case unsafePerformIO $ runWithEnv buildEnvironment (lineToSchemeM input) of
  Right val -> val
  Left err -> error $ T.unpack $ showError err

-- | Run parser and return output
testRunOutput :: Text -> Text
testRunOutput input = showVal (testRun input)
