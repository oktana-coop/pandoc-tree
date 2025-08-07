module DocTree.LeafTextSpans (DocNode (..), TreeNode (..)) where

import DocTree.Common (BlockNode (..), InlineSpan (..))

data TreeNode = BlockNode BlockNode | InlineNode | InlineContent InlineSpan deriving (Show, Eq)

data DocNode = Root | TreeNode TreeNode deriving (Show, Eq)
