module Main (main) where

import Control.Exception (throw)
import Control.Monad (void)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
import Scheme (SchemeVal (..), bindVariables, buildEnvironment, loadStdLib, runWithEnv, showVal)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Spec"
    [ tastyGoldenRun "procedure" "spec/in/procedure.scm" "spec/ans/procedure.scm"
    ]

tastyGoldenRun :: TestName -> T.Text -> FilePath -> TestTree
tastyGoldenRun testName testFile correct =
  goldenVsFile correct tName correct (testRunOutput testFile)
  where
    tName = "spec/out/" <> testName <> ".golden"

testRunOutput :: T.Text -> IO ()
testRunOutput file = testRunFile file >>= print . showVal

testRunFile :: T.Text -> IO SchemeVal
testRunFile file = do
  env <- buildEnvironment >>= flip bindVariables [("args", List [String file])]
  void (loadStdLib env)
  case unsafePerformIO $ runWithEnv env (List [Symbol "load", String file]) of
    Right val -> pure val
    Left err -> throw err
