module DocTree.Error (MalformedTreeError (..)) where

import qualified Data.Text as T

-- A tree that isn't one of the shapes the library builds, so it can't be mapped back to Pandoc.
newtype MalformedTreeError = MalformedTreeError T.Text deriving (Show, Eq)
