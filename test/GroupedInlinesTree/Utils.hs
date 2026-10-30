module GroupedInlinesTree.Utils (documentTree, toPandocOrFail, treeCaptionNode, treeFigureContentNode, treeInlineSpansNode, treeMetaKeyNode, treeMetaValueNode, treeNoteContentNode, treePandocBlockNode, treeTextSpansNode) where

import qualified Data.Text as T
import Data.Tree (Tree (Node))
import qualified DocTree.Common as RichText (BlockNode (Caption, FigureContent, NoteContent), NoteId (..), TextSpan)
import DocTree.GroupedInlines (BlockNode (PandocBlock), DocNode (..), InlineNode (..), InlineSpan (..), MetaNode (..), RichTextNode (..), toPandoc)
import qualified Text.Pandoc.Builder as Pandoc (Block, Caption (..), MetaValue, Pandoc)

toPandocOrFail :: Tree DocNode -> IO Pandoc.Pandoc
toPandocOrFail = either (fail . show) pure . toPandoc

-- The tree of a document without metadata, from the subtrees of its body.
documentTree :: [Tree DocNode] -> Tree DocNode
documentTree bodyTrees = Node Root [Node Metadata [], Node Body bodyTrees]

treePandocBlockNode :: Pandoc.Block -> DocNode
treePandocBlockNode = RichTextNode . BlockNode . PandocBlock

treeCaptionNode :: DocNode
treeCaptionNode = RichTextNode (BlockNode (RichText.Caption (Pandoc.Caption Nothing [])))

treeFigureContentNode :: DocNode
treeFigureContentNode = RichTextNode (BlockNode (RichText.FigureContent []))

treeNoteContentNode :: T.Text -> DocNode
treeNoteContentNode noteId = RichTextNode (BlockNode (RichText.NoteContent (RichText.NoteId noteId) []))

treeMetaKeyNode :: T.Text -> DocNode
treeMetaKeyNode = MetaNode . MetaKey

treeMetaValueNode :: Pandoc.MetaValue -> DocNode
treeMetaValueNode = MetaNode . PandocMetaValue

treeTextSpansNode :: [RichText.TextSpan] -> DocNode
treeTextSpansNode = RichTextNode . InlineNode . InlineContent . (fmap InlineText)

treeInlineSpansNode :: [InlineSpan] -> DocNode
treeInlineSpansNode = RichTextNode . InlineNode . InlineContent
