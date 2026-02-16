module DocTree.LeafTextSpans (DocNode (..), TreeNode (..)) where

import DocTree.Common (BlockNode (..), InlineSpan (..))
import Text.Pandoc.Definition as Pandoc (Meta (..))

data TreeNode = BlockNode BlockNode | InlineNode | InlineContent InlineSpan deriving (Show, Eq)

data DocNode = Root Pandoc.Meta | TreeNode TreeNode deriving (Show, Eq)
