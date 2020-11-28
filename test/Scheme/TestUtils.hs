module Scheme.TestUtils where

import Text.Megaparsec (ParseErrorBundle, Parsec, parse)

-- | Test utility to run a parser on some input
testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse p = parse p ""
