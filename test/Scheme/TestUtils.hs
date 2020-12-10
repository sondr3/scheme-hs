module Scheme.TestUtils where

import Control.Exception (throw)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
import Scheme (SchemeError (..), SchemeVal, buildEnvironment, evalLineForm, runWithEnv, showError, showVal)
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
    Left err -> throw err

-- | Run parser and return AST
testFail :: Text -> IO SchemeError
testFail input = do
  env <- buildEnvironment
  case unsafePerformIO $ runWithEnv env (evalLineForm input) of
    Right val -> error (T.unpack $ showVal val)
    Left err -> return err

-- | Run parser and return output
testRunOutput :: Text -> IO Text
testRunOutput input = do
  res <- testRun input
  return $ showVal res

isSchemeError :: SchemeError -> Bool
isSchemeError (Generic _) = True
isSchemeError (UnboundSymbol _) = True
isSchemeError (ArgumentLengthMismatch _ _) = True
isSchemeError (ParserError _) = True
isSchemeError (NotFunction _) = True
isSchemeError (InvalidOperation _) = True
isSchemeError (ReservedName _) = True
isSchemeError EmptyList = True
isSchemeError _ = False
