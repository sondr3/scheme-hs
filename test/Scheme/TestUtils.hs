module Scheme.TestUtils where

import Data.Text (Text)
import GHC.IO (unsafePerformIO)
import Scheme (SchemeVal, buildEnvironment, lineToEvalForm, runInEnv, showVal)
import Text.Megaparsec (ParseErrorBundle, Parsec, parse)

-- | Test utility to run a parser on some input
testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse p = parse p ""

-- | Run parser and return AST
testRun :: Text -> SchemeVal
testRun input = unsafePerformIO $ runInEnv buildEnvironment (lineToEvalForm input)

-- | Run parser and return output
testRunOutput :: Text -> Text
testRunOutput input = showVal (testRun input)
