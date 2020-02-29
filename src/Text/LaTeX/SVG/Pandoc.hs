{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Text.LaTeX.SVG.Pandoc where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Default
import Data.Text.Encoding.Base64
import System.IO
import Control.Exception
import Text.LaTeX.SVG
import Text.HTMLEntity
import qualified Data.Text as T


data PandocFormulaOptions
    = PandocFormulaOptions
      { errorHandler :: [Handler Inline]
      , formulaOptions :: MathType -> FormulaOptions
      }

instance Default PandocFormulaOptions where
    def = PandocFormulaOptions
          { errorHandler = [defaultHandler]
          , formulaOptions = \case DisplayMath -> simpleAmsDisplay
                                   InlineMath -> simpleAmsInline
          }

defaultHandler :: Handler Inline
defaultHandler = Handler $ \e -> do
    hPrint stderr e
    return $ displayError e

abort :: Handler Inline
abort = Handler $ \e -> do
    hPrint stderr (e :: SomeException)
    throwIO e

hideError :: Handler Inline
hideError = Handler $ \e -> do
    hPrint stderr (e :: SomeException)
    return $ Str "Error"

filterPandocInlineWith
  :: (FormulaOptions -> Formula -> IO T.Text)
  -> PandocFormulaOptions
  -> Inline -> IO Inline
filterPandocInlineWith f opt (Math mt t) =
    handles (errorHandler opt) $ do
      svg <- f (formulaOptions opt mt) t
      let b64 = encodeBase64 svg
      let alt = encode t
      let html = "<img src=\"data:image/svg+xml;base64," <> b64 <> "\""
                 <> " alt=\"" <> alt <> "\""
                 <> " class=" <> (case mt of InlineMath -> "inline-math"; DisplayMath -> "display-math")
                 <> " style=\"margin:0\"" <> "/>"
      return $ RawInline (Format "html") html
    where
      handles = flip catches
filterPandocInlineWith _ _ x = return x

filterPandocInline
  :: CompilerOptions
  -> PandocFormulaOptions
  -> Inline -> IO Inline
filterPandocInline copt =
    filterPandocInlineWith (compileSvg copt)

filterPandoc
  :: CompilerOptions
  -> PandocFormulaOptions
  -> Pandoc -> IO Pandoc
filterPandoc copt =
    walkM . filterPandocInline copt


displayError :: CompilerError -> Inline
displayError (LaTeXError s)         =
    pandocError [Str "LaTeX failed:", LineBreak, Code nullAttr (T.pack s)]
displayError (PDFConversionError s) =
    pandocError [Str "pdf2svg failed:", LineBreak, Code nullAttr (T.pack s)]
displayError SVGFormatError         =
    pandocError [Str "Generated SVG has invalid format"]

pandocError :: [Inline] -> Inline
pandocError = Strong . (Emph [Str "Error:"] :)
