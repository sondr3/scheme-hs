module Scheme.Primitives.IO (ioPrimitives) where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Scheme.Parser (readExpr)
import Scheme.Types (IOSchemeResult, SchemeError (..), SchemeVal (..))
import Scheme.Utils (liftThrows, load)
import System.IO

ioPrimitives :: [(Text, [SchemeVal] -> IOSchemeResult SchemeVal)]
ioPrimitives =
  [ ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

makePort :: IOMode -> [SchemeVal] -> IOSchemeResult SchemeVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile (T.unpack filename) mode
makePort _ [] = throwError $ ArgumentLengthMismatch 1 []
makePort _ x = throwError $ TypeMismatch "String" (head x)

closePort :: [SchemeVal] -> IOSchemeResult SchemeVal
closePort [Port port] = liftIO $ hClose port >> return (Boolean True)
closePort _ = return $ Boolean False

readProc :: [SchemeVal] -> IOSchemeResult SchemeVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (T.pack <$> hGetLine port) >>= liftThrows . readExpr
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
