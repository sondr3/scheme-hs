{-# LANGUAGE OverloadedStrings #-}

module Scheme.Parser where

import Control.Monad (void)
import Data.Complex (Complex ((:+)))
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Scheme.Types (Number (..), SchemeVal (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

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

integer :: Parser Number
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

pDouble :: Parser Number
pDouble = Real <$> (pReal <?> "double")

pRational :: Parser Number
pRational = do
  numerator <- pInteger
  void (char '/')
  denominator <- pInteger
  -- TODO: Fix error if denominator is 0
  return $ Rational (numerator % denominator)

pComplex :: Parser Number
pComplex = do
  real <- pReal
  void (char '+')
  imag <- try (fromInteger <$> pInteger <|> pReal)
  void (char 'i')
  return $ Complex (real :+ imag)

number :: Parser SchemeVal
number = Number <$> choice (map (try . lexeme) [pComplex, pRational, pDouble, integer])

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
pString = try $ lexeme $ String . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

pChar :: Parser SchemeVal
pChar = try $ lexeme $ Character <$> between (char '\'') (char '\'') L.charLiteral

boolean :: Parser SchemeVal
boolean =
  choice
    ( map
        lexeme
        [ Boolean True <$ chunk "#t",
          Boolean False <$ chunk "#f"
        ]
    )

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

pExpr :: Parser SchemeVal
pExpr =
  number
    <|> pChar
    <|> pSymbol
    <|> pString
    <|> boolean
    <|> pList
    <|> pPairList
    <?> "expression"
