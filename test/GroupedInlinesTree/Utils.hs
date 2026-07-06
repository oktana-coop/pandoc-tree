module GroupedInlinesTree.Utils (treeCaptionNode, treeFigureContentNode, treeInlineSpansNode, treePandocBlockNode, treeTextSpansNode) where

import qualified DocTree.Common as RichText (BlockNode (Caption, FigureContent), TextSpan)
import DocTree.GroupedInlines (BlockNode (PandocBlock), DocNode (..), InlineNode (..), InlineSpan (..), TreeNode (..))
import qualified Text.Pandoc.Builder as Pandoc (Block, Caption (..))

treePandocBlockNode :: Pandoc.Block -> DocNode
treePandocBlockNode = TreeNode . BlockNode . PandocBlock

treeCaptionNode :: DocNode
treeCaptionNode = TreeNode (BlockNode (RichText.Caption (Pandoc.Caption Nothing [])))

treeFigureContentNode :: DocNode
treeFigureContentNode = TreeNode (BlockNode (RichText.FigureContent []))

treeTextSpansNode :: [RichText.TextSpan] -> DocNode
treeTextSpansNode = TreeNode . InlineNode . InlineContent . (fmap InlineText)

treeInlineSpansNode :: [InlineSpan] -> DocNode
treeInlineSpansNode = TreeNode . InlineNode . InlineContent
