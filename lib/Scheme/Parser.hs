module Scheme.Parser where

import Control.Monad (void)
import Data.Array (listArray)
import qualified Data.ByteString as BS
import qualified Data.Char as C
import Data.Complex (Complex ((:+)))
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (hexadecimal)
import Data.Word (Word8)
import Scheme.Types (SchemeError (..), SchemeVal (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype ParserError = Unimplemented Text deriving (Show, Eq, Ord)

instance ShowErrorComponent ParserError where
  showErrorComponent (Unimplemented text) = T.unpack text <> " is not implemented"

type Parser = Parsec ParserError Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockCommentNested "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

pInteger :: Parser Integer
pInteger = L.signed (return ()) L.decimal

pBinaryInteger :: Parser Integer
pBinaryInteger = chunk "#b" >> L.binary

pOctalInteger :: Parser Integer
pOctalInteger = chunk "#o" >> L.octal

pDecimalInteger :: Parser Integer
pDecimalInteger = chunk "#d" >> L.decimal

pHexadecimalInteger :: Parser Integer
pHexadecimalInteger = chunk "#x" >> L.hexadecimal

integer :: Parser SchemeVal
integer =
  Integer
    <$> ( choice
            [ pInteger,
              pBinaryInteger,
              pOctalInteger,
              pDecimalInteger,
              pHexadecimalInteger
            ]
            <?> "integer"
        )

pReal :: Parser Double
pReal = L.signed (return ()) L.float

pDouble :: Parser SchemeVal
pDouble = Real <$> (pReal <?> "double")

pRational :: Parser SchemeVal
pRational = do
  numerator <- pInteger
  void (char '/')
  denominator <- pInteger
  -- TODO: Fix error if denominator is 0
  return $ Rational (numerator % denominator)

pComplex :: Parser SchemeVal
pComplex = do
  real <- pNan' <|> pInfinity' <|> try pReal <|> (fromInteger <$> pInteger)
  void (char '+')
  imag <- try pReal <|> fromInteger <$> pInteger <|> read "NaN" <$ chunk "nan.0" <|> read "Infinity" <$ chunk "inf.0"
  void (char 'i')
  return $ Complex (real :+ imag)

pInfinity' :: Parser Double
pInfinity' =
  read "Infinity" <$ chunk "+inf.0"
    <|> read "-Infinity" <$ chunk "-inf.0"

pInfinity :: Parser SchemeVal
pInfinity = Real <$> pInfinity'

pNan' :: Parser Double
pNan' = read "NaN" <$ chunk "+nan.0"

pNan :: Parser SchemeVal
pNan = Real <$> pNan'

pExact :: Parser SchemeVal
pExact = do
  void (try $ chunk "#e")
  num <- number
  case num of
    x@(Integer _) -> return x
    x@(Rational _) -> return x
    (Real x) -> return $ Rational (toRational x)
    _ -> customFailure $ Unimplemented "Exactness for complex numbers"

pInexact :: Parser SchemeVal
pInexact = do
  void (try $ chunk "#i")
  num <- number
  case num of
    x@(Real _) -> return x
    (Integer x) -> return $ Real (fromInteger x)
    (Rational x) -> return $ Real (fromRational x)
    _ -> customFailure $ Unimplemented "Exactness for complex numbers"

number :: Parser SchemeVal
number = choice (map (try . lexeme) [pComplex, pRational, pDouble, integer, pInfinity, pNan, pExact, pInexact])

pSymbol :: Parser SchemeVal
pSymbol = try $ do
  ident <- lexeme (T.pack <$> start <> rest <?> "identifier")
  if ident == "."
    then empty
    else return $ Symbol ident
  where
    extendedSymbols = satisfy (`elem` ['!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '^', '_', '~'])
    start = some (letterChar <|> extendedSymbols)
    rest = many (alphaNumChar <|> extendedSymbols)

pString :: Parser SchemeVal
pString = lexeme $ String . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

pChar :: Parser SchemeVal
pChar = do
  void (try $ chunk "#\\")
  chr <- many asciiChar
  case chr of
    "alarm" -> return $ Character '\BEL'
    "backspace" -> return $ Character '\BS'
    "delete" -> return $ Character '\DEL'
    "escape" -> return $ Character '\ESC'
    "newline" -> return $ Character '\LF'
    "null" -> return $ Character '\NUL'
    "return" -> return $ Character '\CR'
    "space" -> return $ Character ' '
    "tab" -> return $ Character '\HT'
    _ -> pChar' chr

pChar' :: [Char] -> Parser SchemeVal
pChar' [c] = pure (Character c) <?> "char"
pChar' ('x' : hex) = case hexadecimal (T.pack hex) of
  Right n -> return (Character <$> C.chr $ fst n) <?> "hex char"
  _ -> empty
pChar' _ = empty

boolean :: Parser SchemeVal
boolean =
  choice $
    map
      lexeme
      [ Boolean True <$ chunk "#t",
        Boolean False <$ chunk "#f"
      ]

pPairList :: Parser SchemeVal
pPairList = try $
  parens $ do
    ls <- pExpr `sepEndBy` sc <?> "dotted pair car"
    dot <- optional (symbol "." >> pExpr) <?> "pair"
    pure $ case dot of
      Nothing -> List ls
      Just (List c) -> List (ls ++ c)
      Just (PairList car cdr) -> PairList (ls ++ car) cdr
      Just val -> PairList ls val

pList :: Parser SchemeVal
pList = List <$> try (parens (pExpr `sepEndBy` sc)) <?> "list"

pVector :: Parser SchemeVal
pVector = do
  void (try $ symbol "#(")
  ls <- pExpr `sepEndBy` sc
  void (symbol ")")
  return $ Vector (listArray (0, length ls - 1) ls)

pBytevector :: Parser SchemeVal
pBytevector = do
  void (try $ symbol "#u8(")
  ls <- pInteger `sepEndBy` sc
  void (symbol ")")
  return $ Bytevector (BS.pack $ map toByte ls)
  where
    toByte num = fromInteger num :: Word8

pQuote :: Parser SchemeVal
pQuote = do
  void (try $ lexeme $ char '\'')
  expr <- pExpr
  return $ List [Symbol "quote", expr]

pQuasiquote :: Parser SchemeVal
pQuasiquote = do
  void (try $ lexeme $ char '`')
  expr <- pExpr
  return $ List [Symbol "quasiquote", expr]

pUnquote :: Parser SchemeVal
pUnquote = do
  void (try $ lexeme $ char ',')
  expr <- pExpr
  return $ List [Symbol "unquote", expr]

pUnquoteSplicing :: Parser SchemeVal
pUnquoteSplicing = do
  void (try $ lexeme $ chunk ",@")
  expr <- pExpr
  return $ List [Symbol "unquote-splicing", expr]

pExpr :: Parser SchemeVal
pExpr =
  number
    <|> pChar
    <|> pSymbol
    <|> pString
    <|> boolean
    <|> pList
    <|> pPairList
    <|> pVector
    <|> pBytevector
    <|> pQuote
    <|> pQuasiquote
    <|> pUnquoteSplicing
    <|> pUnquote
    <?> "expression"

parseExpr :: FilePath -> Text -> Either SchemeError SchemeVal
parseExpr file input = case runParser pExpr file input of
  Right out -> Right out
  Left err -> Left $ ParserError (T.pack $ errorBundlePretty err)

parseInput :: Text -> Either SchemeError SchemeVal
parseInput = parseExpr "file"

parseFile :: FilePath -> Text -> Either SchemeError SchemeVal
parseFile = parseExpr
