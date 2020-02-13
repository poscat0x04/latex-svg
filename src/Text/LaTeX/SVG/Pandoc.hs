{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Text.LaTeX.SVG.Pandoc where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Default
import System.IO
import Control.Exception
import Text.LaTeX.SVG
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
      txt <- f (formulaOptions opt mt) t
      return $ RawInline (Format "html") txt
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
