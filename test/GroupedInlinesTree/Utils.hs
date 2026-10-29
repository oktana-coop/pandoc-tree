module GroupedInlinesTree.Utils (documentTree, toPandocOrFail, treeCaptionNode, treeFigureContentNode, treeInlineSpansNode, treePandocBlockNode, treeTextSpansNode) where

import Data.Tree (Tree (Node))
import qualified DocTree.Common as RichText (BlockNode (Caption, FigureContent), TextSpan)
import DocTree.GroupedInlines (BlockNode (PandocBlock), DocNode (..), InlineNode (..), InlineSpan (..), RichTextNode (..), toPandoc)
import qualified Text.Pandoc.Builder as Pandoc (Block, Caption (..), Pandoc)
import Text.Pandoc.Definition (nullMeta)

toPandocOrFail :: Tree DocNode -> IO Pandoc.Pandoc
toPandocOrFail = either (fail . show) pure . toPandoc

-- The tree of a document without metadata, from the subtrees of its body.
documentTree :: [Tree DocNode] -> Tree DocNode
documentTree bodyTrees = Node (Root nullMeta) [Node Body bodyTrees]

treePandocBlockNode :: Pandoc.Block -> DocNode
treePandocBlockNode = RichTextNode . BlockNode . PandocBlock

treeCaptionNode :: DocNode
treeCaptionNode = RichTextNode (BlockNode (RichText.Caption (Pandoc.Caption Nothing [])))

treeFigureContentNode :: DocNode
treeFigureContentNode = RichTextNode (BlockNode (RichText.FigureContent []))

treeTextSpansNode :: [RichText.TextSpan] -> DocNode
treeTextSpansNode = RichTextNode . InlineNode . InlineContent . (fmap InlineText)

treeInlineSpansNode :: [InlineSpan] -> DocNode
treeInlineSpansNode = RichTextNode . InlineNode . InlineContent
