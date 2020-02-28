module Text.LaTeX.SVG.Hakyll
       ( initPandocFilter
       ) where

import Text.LaTeX.SVG
import Text.LaTeX.SVG.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Cache.LRU.IO (AtomicLRU)
import qualified Data.Cache.LRU.IO as LRU


type Cache = AtomicLRU Pandoc Pandoc

withLRUCache
  :: Ord a
  => AtomicLRU a b
  -> (a -> IO b)
  -> (a -> IO b)
withLRUCache cache f a = do
    l <- LRU.lookup a cache
    case l of
      Nothing -> do
        b <- f a
        LRU.insert a b cache
        return b
      Just b -> return b

initPandocFilter
  :: CompilerOptions
  -> PandocFormulaOptions
  -> Pandoc -> IO Pandoc
initPandocFilter copt fopt pandoc = do
    lru <- LRU.newAtomicLRU Nothing
    let inlineF = withLRUCache lru (filterPandocInline copt fopt)
    walkM inlineF pandoc
