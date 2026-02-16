module Utils.Debug (traceTree, tracePandoc) where

import Data.Tree (Tree (..), drawTree)
import qualified Debug.Trace (trace)
import Text.Pandoc (Pandoc)
import Text.Show.Pretty (ppShow)

traceTree :: (Show a) => Tree a -> Tree a
traceTree tree = Debug.Trace.trace (drawTree $ fmap show tree) tree

tracePandoc :: Pandoc -> Pandoc
tracePandoc doc = Debug.Trace.trace (ppShow doc) doc