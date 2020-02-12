module Text.LaTeX.SVG.Hakyll
       ( initPandocFilter
       ) where

import Text.LaTeX.SVG
import Text.LaTeX.SVG.Pandoc
import Text.Pandoc.Definition
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
  :: Maybe Integer
  -> CompilerOptions
  -> PandocFormulaOptions
  -> IO (Pandoc -> IO Pandoc)
initPandocFilter size copt fopt = do
    lru <- LRU.newAtomicLRU size
    return $ withLRUCache lru (filterPandoc copt fopt)
