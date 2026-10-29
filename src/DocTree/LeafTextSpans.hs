module DocTree.LeafTextSpans (DocNode (..), RichTextNode (..)) where

import DocTree.Common (BlockNode (..), InlineSpan (..))
import Text.Pandoc.Definition as Pandoc (Meta (..))

data RichTextNode = BlockNode BlockNode | InlineNode | InlineContent InlineSpan deriving (Show, Eq)

data DocNode = Root Pandoc.Meta | RichTextNode RichTextNode deriving (Show, Eq)
