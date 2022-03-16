module Scheme.TestUtils
  ( testParse,
    testRun,
    testRunOutput,
    isSchemeError,
  )
where

import Control.Exception (throw)
import Control.Monad (void)
import Data.Text (Text)
import GHC.IO (unsafePerformIO)
import Scheme (SchemeError (..), SchemeVal, buildEnvironment, evalLineForm, loadStdLib, runWithEnv, showVal)
import Text.Megaparsec (ParseErrorBundle, Parsec, parse)

-- | Test utility to run a parser on some input
testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse p = parse p ""

-- | Run parser and return AST
testRun :: Text -> IO SchemeVal
testRun input = do
  env <- buildEnvironment
  void (loadStdLib env)
  case unsafePerformIO $ runWithEnv env (evalLineForm input) of
    Right val -> pure val
    Left err -> throw err

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
