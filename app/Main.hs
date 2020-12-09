module Main where

import Scheme (repl)
import System.Environment (getArgs)

help :: IO ()
help =
  putStrLn
    "scheme \n\
    \Usage:\n\
    \  scheme <filename>\n\
    \  scheme -e <expr>\n\
    \  scheme\n"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    ["-h"] -> help
    ["--help"] -> help
    _ -> help
