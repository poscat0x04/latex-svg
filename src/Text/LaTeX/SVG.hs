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
import Data.Dynamic
import Data.List
import Text.LaTeX.SVG.Util
import Text.Megaparsec
import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

data CompilerError
    = LaTeXError String
    | SVGFormatError
    | PDFConversionError String
    deriving Typeable

data Crop = Full
          | Vertical
          | None
          deriving (Show, Eq)

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
      { preamble :: Text
      , environment :: Text
      , crop :: Crop
      }

type Formula = Text

withPackage :: [Text] -> Text
withPackage ps =
    T.intercalate "\n" $
      map (\p -> "\\usepackage{" <> p <> "}") ps

simpleAmsInline :: FormulaOptions
simpleAmsInline =
    FormulaOptions (withPackage ["amsmath"]) "math" Full

simpleAmsDisplay :: FormulaOptions
simpleAmsDisplay =
    FormulaOptions (withPackage ["amsmath"]) "displaymath" Vertical

formatDoc :: FormulaOptions -> Formula -> Text
formatDoc FormulaOptions{..} f =
    let fmt = T.intercalate "\n"
    in fmt
      [ "\\nonstopmode"
      , "\\documentclass[12pt]{article}"
      , "\\pagestyle{empty}"
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

compileSvg :: CompilerOptions -> FormulaOptions -> Formula -> IO Text
compileSvg CompilerOptions{..} opt eqn =
    let texF = basePath <.> "tex"
        auxF = basePath <.> "aux"
        logF = basePath <.> "log"
        pdfF = basePath <.> "pdf"
        svgF = basePath <.> "svg"
        cleanUp = forM_ [texF, auxF, logF, pdfF, svgF] removeIfExists
    in handle (\e -> do {cleanUp; throwIO (e :: SomeException)}) $ do
        let doc = formatDoc opt eqn
        T.writeFile texF doc
        (c, o, e) <- readProcessWithExitCode latexCommand (latexArgs ++ [texF]) ""
        when (c /= ExitSuccess) $
            throwIO $ LaTeXError $ o <> "\n" <> e
        when (crop opt == Full) (fullCropPDF pdfF)
        when (crop opt == Vertical) (verticalCropPDF pdfF)
        (c, o, e) <- readProcessWithExitCode pdf2svgCommand [pdfF, svgF] ""
        when (c /= ExitSuccess) $
            throwIO $ PDFConversionError $ o <> "\n" <> e
        t <- T.readFile svgF
        cleanUp
        return $ T.intercalate "\n" $ tail $ T.lines t

verticalCropPDF :: FilePath -> IO ()
verticalCropPDF f = do
    (_, o, _) <- readProcessWithExitCode "pdfinfo" [f] ""
    let l = head $ filter ("Page size:" `isPrefixOf`) $ lines o
    print l
    let Right (len, _) = runParser pageSizeParser "" l
    (_, o, _) <- readProcessWithExitCode "pdfcrop" ["--verbose", f] ""
    let l = head $ filter ("%%HiResBoundingBox" `isPrefixOf`) $ lines o
    let Right (f1, _, f3, _) = runParser boundingBoxParser "" l
    let arg = unwords [show f1, "10", show (len - f3), "10"]
    readProcessWithExitCode "pdfcrop" ["--margins", arg, f, f] ""
    return ()

fullCropPDF :: FilePath -> IO ()
fullCropPDF f = do
    readProcessWithExitCode "pdfcrop" [f, f] ""
    return ()
