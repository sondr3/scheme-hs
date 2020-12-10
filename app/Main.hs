module Main where

import Control.Monad.State.Strict
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Scheme
import System.Console.Haskeline
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

help :: IO ()
help =
  putStrLn
    "scheme \n\
    \Usage:\n\
    \  scheme <filename>\n\
    \  scheme -e <expr>\n\
    \  scheme\n"

evalStringAndShow :: Env -> String -> IO String
evalStringAndShow env expr =
  liftIOThrows $
    (liftThrows . readManyExpr) (T.pack expr)
      >>= fmap nilOrLast . mapM (eval env)
      <&> showVal
      <&> T.unpack
  where
    nilOrLast x = if null x then List [] else last x

schemeSettings :: IO (Settings (StateT [String] IO))
schemeSettings =
  return $
    Settings
      { complete = completer,
        historyFile = Just ".schistory",
        autoAddHistory = True
      }
  where
    completer :: CompletionFunc (StateT [String] IO)
    completer = completeWord Nothing " \t" $ \w -> do map simpleCompletion . filter (isPrefixOf w) <$> get

runRepl :: Env -> IO ()
runRepl env = do
  conf <- schemeSettings
  void (loadStdLib env)
  flip evalStateT primitiveNames . runInputT conf $ loop
  where
    loop :: InputT (StateT [String] IO) ()
    loop = do
      input <- getInputLine "λ "
      case input of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "exit" -> return ()
        Just (';' : _) -> loop
        Just "" -> loop
        Just x -> puts x >> loop
    puts :: String -> InputT (StateT [String] IO) ()
    puts input = do
      out <- lift $ lift $ evalStringAndShow env input
      outputStrLn out >> loop

runFile :: Text -> IO ()
runFile file = do
  env <- buildEnvironment >>= flip bindVariables [("args", List [String file])]
  liftIOThrows (show <$> eval env (List [Symbol "load", String file])) >>= hPutStrLn stderr

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      env <- buildEnvironment
      runRepl env
    ["-h"] -> help
    ["--help"] -> help
    (f : _) -> runFile (T.pack f)
