module Main where

import Control.Monad.Cont (MonadTrans (lift))
import Data.Functor ((<&>))
import qualified Data.Text as T
import Scheme
import System.Console.Haskeline
import System.Environment (getArgs)

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

evalAndPrint :: Env -> String -> InputT IO ()
evalAndPrint env expr = do
  out <- lift $ evalStringAndShow env expr
  outputStrLn out

schemeSettings :: Settings IO
schemeSettings = defaultSettings {historyFile = Just ".schistory"}

repl' :: (String -> InputT IO ()) -> IO ()
repl' a = runInputT schemeSettings $ loop a
  where
    loop :: (String -> InputT IO ()) -> InputT IO ()
    loop action = do
      input <- getInputLine "λ "
      case input of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "exit" -> return ()
        Just (';' : _) -> loop action
        Just "" -> loop action
        Just x -> action x >> loop action

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      env <- buildEnvironment
      repl' (evalAndPrint env)
    ["-h"] -> help
    ["--help"] -> help
    _ -> help
