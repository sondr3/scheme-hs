{-# LANGUAGE OverloadedStrings #-}

module Scheme
  ( module Scheme.Parser,
    module Scheme.Types,
    module Scheme.Eval,
    module Scheme.Primitives,
    module Scheme.Environment,
    repl,
  )
where

import Data.Char (isSpace)
import Data.Functor ((<&>))
import qualified Data.Text as T
import Scheme.Environment
import Scheme.Eval
import Scheme.Parser
import Scheme.Primitives
import Scheme.Types
import System.IO (hFlush, stdout)

-- | Prints out a string and immediately flush the stream.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- | Prints a prompt and read a line of input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalStringAndShow :: Env -> String -> IO String
evalStringAndShow env expr =
  liftIOThrows $
    (liftThrows . readManyExpr) (T.pack expr)
      >>= fmap nilOrLast . mapM (eval env)
      <&> showVal
      <&> T.unpack
  where
    nilOrLast x = if null x then List [] else last x

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalStringAndShow env expr >>= putStrLn

loopRepl :: Monad m => m String -> (String -> m ()) -> m ()
loopRepl prompt action = do
  res <- prompt
  case dropWhile isSpace res of
    "quit" -> return ()
    "exit" -> return ()
    ';' : _ -> loopRepl prompt action
    "" -> loopRepl prompt action
    _ -> action res >> loopRepl prompt action

repl :: IO ()
repl = do
  env <- buildEnvironment
  loopRepl (readPrompt "λ ") (evalAndPrint env)
