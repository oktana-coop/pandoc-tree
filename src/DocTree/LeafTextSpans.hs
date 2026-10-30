module DocTree.LeafTextSpans (DocNode (..), RichTextNode (..)) where

import DocTree.Common (BlockNode (..), InlineSpan (..), MetaNode)

data RichTextNode = BlockNode BlockNode | InlineNode | InlineContent InlineSpan deriving (Show, Eq)

data DocNode = Root | Metadata | Body | MetaNode MetaNode | RichTextNode RichTextNode deriving (Show, Eq)
