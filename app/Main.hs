module Main where

import Control.Monad.Trans
import Data.List (isPrefixOf)
import Scheme (primitiveNames)
import System.Console.Repline

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd input = liftIO $ print input

completer :: Monad m => WordCompleter m
completer n = do
  let names = primitiveNames
  return $ filter (isPrefixOf n) names

help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

ini :: Repl ()
ini = liftIO $ putStrLn "SCHEME"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

repl :: IO ()
repl =
  evalReplOpts $
    ReplOpts
      { banner = const $ pure "λ ",
        command = cmd,
        options = [],
        prefix = Just ':',
        multilineCommand = Just "paste",
        tabComplete = Word0 completer,
        initialiser = ini,
        finaliser = final
      }

main :: IO ()
main = repl
