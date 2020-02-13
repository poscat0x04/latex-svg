module Text.LaTeX.SVG.Util where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer


type Parser = Parsec Void String

pageSizeParser :: Parser (Float, Float)
pageSizeParser = do
    string "Page size:      "
    f1 <- takeWhileP Nothing (/= ' ')
    string " x "
    f2 <- takeWhileP Nothing (/= ' ')
    takeRest
    return (read f1, read f2)

boundingBoxParser :: Parser (Float, Float, Float, Float)
boundingBoxParser = do
    string "%%HiResBoundingBox: "
    f1 <- float
    string " "
    f2 <- float
    string " "
    f3 <- float
    string " "
    f4 <- float
    takeRest
    return (f1, f2, f3, f4)
