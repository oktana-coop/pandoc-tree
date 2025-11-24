module Utils.Sequence (firstValue) where

import Data.Sequence as Seq (ViewL (EmptyL, (:<)), viewl)
import Text.Pandoc.Builder (Many (unMany))

-- Gets the first value of a sequence wrapped in Many
firstValue :: Many a -> Maybe a
firstValue many = case Seq.viewl xs of
  Seq.EmptyL -> Nothing
  x Seq.:< _ -> Just x
  where
    xs = unMany many
