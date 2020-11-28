{-# LANGUAGE OverloadedStrings #-}

module Scheme.Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
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

integer :: Parser Integer
integer = lexeme L.decimal <?> "integer"

signedInteger :: Parser Integer
signedInteger = L.signed sc integer <?> "signed integer"

identifier :: Parser Text
identifier = lexeme (T.pack <$> start <> rest) <?> "identifier"
  where
    extendedSymbols = satisfy (`elem` ['!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '^', '_', '~'])
    start = many (letterChar <|> extendedSymbols)
    rest = many (alphaNumChar <|> extendedSymbols)

string :: Parser Text
string = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))
