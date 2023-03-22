module Scheme.Primitives.IO (ioPrimitives) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Scheme.Parser (readExpr)
import Scheme.Types (IOSchemeResult, SchemeError (..), SchemeVal (..), showVal)
import Scheme.Utils (liftThrows, load)
import System.IO

ioPrimitives :: [(Text, [SchemeVal] -> IOSchemeResult SchemeVal)]
ioPrimitives =
  [ ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("display", printVal),
    ("displayln", printValLn),
    ("newline", newline),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

printVal :: [SchemeVal] -> IOSchemeResult SchemeVal
printVal [xs] = printVal [xs, Port stdout]
printVal [String s, Port port] = liftIO $ TIO.hPutStr port s >> return Nil
printVal [Character c, Port port] = liftIO $ hPutStr port (c : "") >> return Nil
printVal [val, Port port] = liftIO $ TIO.hPutStr port (showVal val) >> return Nil
printVal xs = throwError $ ArgumentLengthMismatch 2 xs

printValLn :: [SchemeVal] -> IOSchemeResult SchemeVal
printValLn = foldr (\val -> (>>) (printVal [val])) (newline [])

newline :: [SchemeVal] -> IOSchemeResult SchemeVal
newline [] = printVal [Character '\n', Port stdout]
newline [Port port] = printVal [Character '\n', Port port]
newline xs = throwError $ ArgumentLengthMismatch 1 xs

makePort :: IOMode -> [SchemeVal] -> IOSchemeResult SchemeVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile (T.unpack filename) mode
makePort _ [] = throwError $ ArgumentLengthMismatch 1 []
makePort _ x = throwError $ TypeMismatch "String" (head x)

closePort :: [SchemeVal] -> IOSchemeResult SchemeVal
closePort [Port port] = liftIO $ hClose port >> return (Boolean True)
closePort _ = return $ Boolean False

readProc :: [SchemeVal] -> IOSchemeResult SchemeVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (TIO.hGetLine port) >>= liftThrows . readExpr
readProc [x] = throwError $ TypeMismatch "Port" x
readProc _ = throwError $ ArgumentLengthMismatch 1 []

writeProc :: [SchemeVal] -> IOSchemeResult SchemeVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Boolean True)
writeProc [] = throwError $ ArgumentLengthMismatch 1 []
writeProc x = throwError $ TypeMismatch "Port" (head x)

readContents :: [SchemeVal] -> IOSchemeResult SchemeVal
readContents [String filename] = do
  file <- liftIO $ readFile (T.unpack filename)
  return $ String (T.pack file)
readContents [] = throwError $ ArgumentLengthMismatch 1 []
readContents x = throwError $ TypeMismatch "String" (head x)

readAll :: [SchemeVal] -> IOSchemeResult SchemeVal
readAll [String filename] = List <$> load filename
readAll [] = throwError $ ArgumentLengthMismatch 1 []
readAll x = throwError $ TypeMismatch "String" (head x)
