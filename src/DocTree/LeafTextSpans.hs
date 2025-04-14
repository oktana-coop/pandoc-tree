module DocTree.LeafTextSpans (DocNode (..), TreeNode (..)) where

import DocTree.Common (BlockNode (..), TextSpan (..))

data TreeNode = BlockNode BlockNode | InlineNode | InlineContent TextSpan deriving (Show, Eq)

data DocNode = Root | TreeNode TreeNode deriving (Show, Eq)
