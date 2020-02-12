module Main where

import Text.LaTeX.SVG.Pandoc
import Text.LaTeX.SVG
import Data.Default
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter $ filterPandocInline def def
