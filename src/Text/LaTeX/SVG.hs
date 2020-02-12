{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.LaTeX.SVG
       ( FormulaOptions(..)
       , CompilerError(..)
       , CompilerOptions(..)
       , Formula
       , compileSvg
       , simpleAmsInline
       , simpleAmsDisplay
       ) where


import System.FilePath
import System.Directory
import System.Exit
import System.Process
import Control.Exception
import Control.Monad
import Data.Default
import qualified Data.ByteString as BS
import Data.Dynamic
import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Graphics.Svg ( parseSvgFile
                    , Document)

data CompilerError
    = LaTeXError String
    | SVGFormatError
    | PDFConversionError String
    deriving Typeable

instance Show CompilerError where
    show (LaTeXError s) = "LaTeX compilation failed with error: " <> s
    show SVGFormatError = "Generated SVG has invalid format, please file a bug report"
    show (PDFConversionError s) = "PDF conversion failed with error: " <> s

instance Exception CompilerError


data CompilerOptions
    = CompilerOptions
      { latexCommand :: String    -- ^ Command used for compiling LaTeX to PDF
      , pdf2svgCommand :: String  -- ^ Command used for compiling PDF to SVG
      , latexArgs :: [String]
      , pdf2svgArgs :: [String]
      , basePath :: FilePath
      }

instance Default CompilerOptions where
    def = CompilerOptions
          { latexCommand = "xelatex"
          , pdf2svgCommand = "pdf2svg"
          , latexArgs = []
          , pdf2svgArgs = []
          , basePath = "/tmp/latex-svg"
          }

data FormulaOptions
    = FormulaOptions
      { preamble :: Text , environment :: Text
      }

type Formula = Text

withPackage :: [Text] -> Text
withPackage ps =
    T.intercalate "\n" $
      map (\p -> "\\usepackage{" <> p <> "}") ps

simpleAmsInline :: FormulaOptions
simpleAmsInline =
    FormulaOptions (withPackage ["amsmath"]) "math"

simpleAmsDisplay :: FormulaOptions
simpleAmsDisplay =
    FormulaOptions (withPackage ["amsmath"]) "displaymath"

formatDoc :: FormulaOptions -> Formula -> Text
formatDoc FormulaOptions{..} f =
    let fmt = T.intercalate "\n"
    in fmt
      [ "\\nonstopmode"
      , "\\documentclass[12pt]{article}"
      , "\\usepackage[]{geometry}"
      , "\\geometry{paperwidth=\\dimexpr\\textwidth+3mm," <>
        "paperheight=\\dimexpr\\textheight+3mm," <>
        "margin=1.5mm}"
      , preamble
      , "\\begin{document}"
      , "\\begin{" <> environment <> "}"
      , f
      , "\\end{" <> environment <> "}"
      , "\\end{document}"
      ]

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = do
    fileExist <- doesFileExist fileName
    when fileExist (removeFile fileName)

compileSvg :: CompilerOptions -> FormulaOptions -> Formula -> IO Document
compileSvg CompilerOptions{..} opt eqn =
    let texF = basePath <.> "tex"
        auxF = basePath <.> "aux"
        logF = basePath <.> "log"
        pdfF = basePath <.> "pdf"
        svgF = basePath <.> "svg"
        cleanUp = forM_ [texF, auxF, logF, pdfF] removeIfExists
    in handle (\e -> do {cleanUp; throwIO (e :: SomeException)}) $ do
        let doc = formatDoc opt eqn
        T.writeFile texF doc
        (c, o, e) <- readProcessWithExitCode latexCommand (latexArgs ++ [texF]) ""
        when (c /= ExitSuccess) $
            throwIO $ LaTeXError $ o <> "\n" <> e
        (c, o, e) <- readProcessWithExitCode pdf2svgCommand [pdfF, svgF] ""
        when (c /= ExitSuccess) $
            throwIO $ PDFConversionError $ o <> "\n" <> e
        svg <- BS.readFile svgF
        let d = parseSvgFile "" svg
        case d of
          Nothing -> throwIO SVGFormatError
          Just r -> do
              cleanUp
              return r
