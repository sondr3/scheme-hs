{-# LANGUAGE OverloadedStrings #-}

module Scheme.Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Scheme.Types (SchemeVal (..))
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
pInteger = L.decimal

pSignedInteger :: Parser Integer
pSignedInteger = L.signed sc pInteger

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
  lexeme $
    Integer
      <$> ( choice
              [ pInteger,
                pSignedInteger,
                pBinaryInteger,
                pOctalInteger,
                pDecimalInteger,
                pHexadecimalInteger
              ]
              <?> "integer"
          )

pDouble :: Parser Double
pDouble = L.float

pSignedDouble :: Parser Double
pSignedDouble = L.signed sc pDouble

float :: Parser SchemeVal
float = lexeme $ Double <$> (pDouble <|> pSignedDouble <?> "double")

identifier :: Parser Text
identifier = lexeme (T.pack <$> start <> rest) <?> "identifier"
  where
    extendedSymbols = satisfy (`elem` ['!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '^', '_', '~'])
    start = many (letterChar <|> extendedSymbols)
    rest = many (alphaNumChar <|> extendedSymbols)

string :: Parser SchemeVal
string = String . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

boolean :: Parser SchemeVal
boolean =
  choice
    [ Boolean True <$ chunk "#t",
      Boolean False <$ chunk "#f"
    ]
