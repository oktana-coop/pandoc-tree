{-# LANGUAGE OverloadedStrings #-}

module DocTree.GroupedInlines (BlockNode (..), MetaNode (..), InlineSpan (..), InlineNode (..), DocNode (..), RichTextNode (..), toTree, toPandoc) where

import Control.Monad.State (State, evalState, get, gets, modify)
import Data.List (partition)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tree (Tree (Node), foldTree, unfoldForestM, unfoldTreeM)
import DocTree.Common (BlockNode (..), Image (..), InlineSpan (..), LinkMark (..), Mark (..), MetaNode (..), NoteId (..), TextSpan (..))
import DocTree.Error (MalformedTreeError (..))
import qualified Text.Pandoc.Builder as Pandoc
  ( Block (..),
    Inlines,
    Pandoc,
    code,
    emph,
    emptyCaption,
    fromList,
    imageWith,
    linkWith,
    singleton,
    str,
    strong,
    toList,
  )
import qualified Text.Pandoc.Definition as Pandoc (Caption (..), Inline (..), Meta (..), MetaValue (..), Pandoc (..), QuoteType (..))
import Utils.Sequence (firstValue)

data InlineNode = InlineContent [InlineSpan] deriving (Show, Eq)

instance Semigroup InlineNode where
  (<>) (InlineContent spans1) (InlineContent spans2) = InlineContent (spans1 <> spans2)

instance Monoid InlineNode where
  mempty = InlineContent []

data RichTextNode = BlockNode BlockNode | InlineNode InlineNode deriving (Show, Eq)

data DocNode = Root | Metadata | Body | MetaNode MetaNode | RichTextNode RichTextNode deriving (Show, Eq)

data NoteData = NoteData
  { noteCounter :: Int,
    -- Accumulated note content block nodes
    noteContents :: [Tree DocNode]
  }

type NotesState = State NoteData

toTree :: Pandoc.Pandoc -> Tree DocNode
toTree (Pandoc.Pandoc meta blocks) = evalState documentTree initialState
  where
    initialState = NoteData 0 []

    -- The notes are numbered in one sequence, the metadata's first.
    documentTree :: NotesState (Tree DocNode)
    documentTree = do
      metadataTree <- toMetaTree meta
      bodyTree <- toBodyTree blocks
      return $ Node Root [metadataTree, bodyTree]

    toMetaTree :: Pandoc.Meta -> NotesState (Tree DocNode)
    toMetaTree (Pandoc.Meta entries) = fmap (Node Metadata) (toMetaKeyTrees entries)

    toBodyTree :: [Pandoc.Block] -> NotesState (Tree DocNode)
    toBodyTree bodyBlocks = do
      mainForest <- blocksToRichTextForest bodyBlocks
      -- All the note contents belong to the body (even if the respective note refs are in metadata).
      noteTrees <- gets noteContents
      return $ Node Body (mainForest <> noteTrees)

toMetaKeyTrees :: M.Map T.Text Pandoc.MetaValue -> NotesState [Tree DocNode]
toMetaKeyTrees entries = traverse toMetaKeyTree (M.toList entries)
  where
    toMetaKeyTree :: (T.Text, Pandoc.MetaValue) -> NotesState (Tree DocNode)
    toMetaKeyTree (key, metaValue) = do
      valueTree <- toMetaValueTree metaValue
      return $ Node (MetaNode $ MetaKey key) [valueTree]

-- Maps, lists, inlines and blocks are held empty, with their content as children. Booleans and strings are held whole.
toMetaValueTree :: Pandoc.MetaValue -> NotesState (Tree DocNode)
toMetaValueTree metaValue = case metaValue of
  Pandoc.MetaMap entries -> fmap (Node $ metaValueNode $ Pandoc.MetaMap M.empty) (toMetaKeyTrees entries)
  Pandoc.MetaList metaValues -> fmap (Node $ metaValueNode $ Pandoc.MetaList []) (traverse toMetaValueTree metaValues)
  Pandoc.MetaInlines inlines -> do
    inlineNode <- buildInlineNode inlines
    fmap (Node $ metaValueNode $ Pandoc.MetaInlines []) (unfoldForestM richTextNodeUnfolder [inlineNode])
  Pandoc.MetaBlocks blocks -> fmap (Node $ metaValueNode $ Pandoc.MetaBlocks []) (blocksToRichTextForest blocks)
  Pandoc.MetaBool _ -> return $ Node (metaValueNode metaValue) []
  Pandoc.MetaString _ -> return $ Node (metaValueNode metaValue) []
  where
    metaValueNode :: Pandoc.MetaValue -> DocNode
    metaValueNode = MetaNode . PandocMetaValue

blocksToRichTextForest :: [Pandoc.Block] -> NotesState [Tree DocNode]
blocksToRichTextForest blocks = unfoldForestM richTextNodeUnfolder (map (BlockNode . PandocBlock) blocks)

richTextNodeUnfolder :: RichTextNode -> NotesState (DocNode, [RichTextNode])
richTextNodeUnfolder (BlockNode blockNode) = blockNodeUnfolder blockNode
richTextNodeUnfolder (InlineNode inlineNode) = return $ inlineNodeUnfolder inlineNode

blockNodeUnfolder :: BlockNode -> NotesState (DocNode, [RichTextNode])
blockNodeUnfolder (PandocBlock block) = case block of
  Pandoc.Plain inlines -> do
    inlineNode <- buildInlineNode inlines
    return ((RichTextNode . BlockNode . PandocBlock . Pandoc.Plain) [], [inlineNode])
  Pandoc.Para inlines -> do
    inlineNode <- buildInlineNode inlines
    return ((RichTextNode . BlockNode . PandocBlock . Pandoc.Para) [], [inlineNode])
  Pandoc.Header level attrs inlines -> do
    inlineNode <- buildInlineNode inlines
    return (RichTextNode . BlockNode $ PandocBlock $ Pandoc.Header level attrs [], [inlineNode])
  Pandoc.CodeBlock attrs text -> return (RichTextNode . BlockNode $ PandocBlock $ Pandoc.CodeBlock attrs T.empty, textToRichTextNodes text)
  Pandoc.RawBlock format text -> return (RichTextNode . BlockNode . PandocBlock $ Pandoc.RawBlock format T.empty, textToRichTextNodes text)
  Pandoc.BulletList items -> return ((RichTextNode . BlockNode . PandocBlock . Pandoc.BulletList) [], map (BlockNode . ListItem) items)
  Pandoc.OrderedList attrs items -> return (RichTextNode $ BlockNode $ PandocBlock $ Pandoc.OrderedList attrs [], map (BlockNode . ListItem) items)
  Pandoc.BlockQuote children -> return ((RichTextNode . BlockNode . PandocBlock . Pandoc.BlockQuote) [], map (BlockNode . PandocBlock) children)
  -- Figure, FigureContent and Caption are separate tree nodes.
  Pandoc.Figure attr cap children ->
    return
      ( (RichTextNode . BlockNode . PandocBlock) $ Pandoc.Figure attr Pandoc.emptyCaption [],
        [BlockNode (FigureContent children)] ++ [BlockNode (Caption cap) | not (isEmptyCaption cap)]
      )
    where
      isEmptyCaption (Pandoc.Caption Nothing []) = True
      isEmptyCaption _ = False
  Pandoc.Div attr children -> return ((RichTextNode . BlockNode . PandocBlock) $ Pandoc.Div attr [], map (BlockNode . PandocBlock) children)
  Pandoc.HorizontalRule -> return (RichTextNode . BlockNode . PandocBlock $ Pandoc.HorizontalRule, [])
  -- Not modeled as trees yet, so kept whole, as leaves.
  Pandoc.Table {} -> return (RichTextNode . BlockNode $ PandocBlock block, [])
  Pandoc.LineBlock _ -> return (RichTextNode . BlockNode $ PandocBlock block, [])
  Pandoc.DefinitionList _ -> return (RichTextNode . BlockNode $ PandocBlock block, [])
blockNodeUnfolder (ListItem children) = return ((RichTextNode . BlockNode . ListItem) [], map (BlockNode . PandocBlock) children)
blockNodeUnfolder (NoteContent noteId children) = return (RichTextNode $ BlockNode $ NoteContent noteId [], map (BlockNode . PandocBlock) children)
blockNodeUnfolder (Caption (Pandoc.Caption short blocks)) = return (RichTextNode . BlockNode . Caption $ Pandoc.Caption short [], map (BlockNode . PandocBlock) blocks)
blockNodeUnfolder (FigureContent blocks) = return (RichTextNode . BlockNode . FigureContent $ [], map (BlockNode . PandocBlock) blocks)

textToRichTextNodes :: T.Text -> [RichTextNode]
textToRichTextNodes text = [InlineNode $ InlineContent [InlineText $ TextSpan {value = text, marks = []}] | not (T.null text)]

buildInlineNode :: [Pandoc.Inline] -> NotesState RichTextNode
buildInlineNode inlines = fmap (InlineNode . InlineContent) $ pandocInlinesToSpans inlines

pandocInlinesToSpans :: [Pandoc.Inline] -> NotesState [InlineSpan]
pandocInlinesToSpans inlines =
  -- Use fmap to lift `mergeSameMarkSpans . concat` over the State structure
  fmap (mergeSameMarkSpans . concat) (perInlineSpans inlines)
  where
    -- Convert each inline into a list of spans, inside the State monad.
    perInlineSpans :: [Pandoc.Inline] -> NotesState [[InlineSpan]]
    perInlineSpans = mapM inlineToSpans

mergeSameMarkSpans :: [InlineSpan] -> [InlineSpan]
mergeSameMarkSpans = foldr mergeOrAppendAdjacent []
  where
    mergeOrAppendAdjacent :: InlineSpan -> [InlineSpan] -> [InlineSpan]
    mergeOrAppendAdjacent x [] = [x]
    -- We only merge if the adjacent spans are **text** spans and they have the same marks.
    mergeOrAppendAdjacent (InlineText xTextSpan) (InlineText firstOrRestTextSpan : rest)
      | marks xTextSpan == marks firstOrRestTextSpan =
          InlineText (xTextSpan <> firstOrRestTextSpan) : rest
    mergeOrAppendAdjacent x rest = x : rest

inlineToSpans :: Pandoc.Inline -> NotesState [InlineSpan]
inlineToSpans inline = case inline of
  Pandoc.Str s -> return [InlineText $ TextSpan s []]
  Pandoc.Space -> return [InlineText $ TextSpan (T.pack " ") []]
  Pandoc.SoftBreak -> return [InlineText $ TextSpan (T.pack " ") []]
  Pandoc.LineBreak -> return [InlineText $ TextSpan (T.pack "\n") []]
  Pandoc.Strong inlines -> do
    wrappedSpans <- pandocInlinesToSpans inlines
    return $ addMark StrongMark wrappedSpans
  Pandoc.Emph inlines -> do
    wrappedSpans <- pandocInlinesToSpans inlines
    return $ addMark EmphMark wrappedSpans
  Pandoc.Quoted quoteType inlines -> do
    innerSpans <- pandocInlinesToSpans inlines
    let (openQuote, closeQuote) = quoteChars quoteType
    return $ [InlineText $ TextSpan openQuote []] <> innerSpans <> [InlineText $ TextSpan closeQuote []]
  Pandoc.Link attrs inlines target -> do
    wrappedSpans <- pandocInlinesToSpans inlines
    return $ addMark (LinkMark $ DocTree.Common.Link attrs target) wrappedSpans
  Pandoc.Image attrs altInlines target -> return [InlineImage $ DocTree.Common.Image attrs altInlines target]
  -- TODO: Handle code attrs
  Pandoc.Code _ s -> return [InlineText $ TextSpan s [CodeMark]]
  Pandoc.Note noteBlocks -> do
    -- Generate note ID and create note content
    notesState <- get
    let newNoteId = noteCounter notesState + 1
        noteIdText = T.pack $ show newNoteId
        noteId = NoteId noteIdText

    -- Convert note blocks to spans
    noteContentTree <- unfoldTreeM richTextNodeUnfolder (BlockNode $ NoteContent noteId noteBlocks)

    -- Update state
    modify
      ( \currentNotestState ->
          -- Getting a new state using the record update syntax.
          currentNotestState
            { noteCounter = newNoteId,
              noteContents = noteContents currentNotestState <> [noteContentTree]
            }
      )

    -- Return note ref node
    return [NoteRef noteId]
  -- Not modeled yet, so kept whole: Math, RawInline, Cite, Span, and Strikeout, Underline, Superscript, Subscript
  -- and SmallCaps, which could become marks.
  _ -> return [PandocInline inline]

quoteChars :: Pandoc.QuoteType -> (T.Text, T.Text)
quoteChars Pandoc.SingleQuote = (T.pack "\8216", T.pack "\8217") -- ‘ ’
quoteChars Pandoc.DoubleQuote = (T.pack "\8220", T.pack "\8221") -- “ ”

addMark :: Mark -> [InlineSpan] -> [InlineSpan]
addMark mark spans = fmap (addMarkToSpan mark) spans
  where
    addMarkToSpan :: Mark -> InlineSpan -> InlineSpan
    addMarkToSpan m (InlineText textSpan) = InlineText $ TextSpan T.empty [m] <> textSpan
    -- Leave non-text spans (like note refs) unchanged
    addMarkToSpan _ otherSpan = otherSpan

inlineNodeUnfolder :: InlineNode -> (DocNode, [RichTextNode])
inlineNodeUnfolder inlineNode = (RichTextNode $ InlineNode inlineNode, [])

data PandocElement = BlockElement Pandoc.Block | InlineElement Pandoc.Inlines | CaptionElement Pandoc.Caption

toPandoc :: Tree DocNode -> Either MalformedTreeError Pandoc.Pandoc
toPandoc (Node Root [metadataTree, bodyTree]) = Pandoc.Pandoc <$> toMeta metadataTree <*> toBlocks bodyTree
  where
    -- The contents of all the notes, the metadata's included, are in the body.
    noteContentsMap = buildNoteContentsMap bodyTree

    toMeta :: Tree DocNode -> Either MalformedTreeError Pandoc.Meta
    toMeta (Node Metadata keyTrees) = fmap Pandoc.Meta (toMetaEntries noteContentsMap keyTrees)
    toMeta _ = Left $ MalformedTreeError "Expected a metadata node"

    toBlocks :: Tree DocNode -> Either MalformedTreeError [Pandoc.Block]
    toBlocks tree = treeToPandocElements noteContentsMap tree >>= getBlocks
toPandoc _ = Left $ MalformedTreeError "Expected a root with the metadata and the body as its children"

toMetaEntries :: NoteContentsMap -> [Tree DocNode] -> Either MalformedTreeError (M.Map T.Text Pandoc.MetaValue)
toMetaEntries noteContentsMap keyTrees = fmap M.fromList (traverse toMetaEntry keyTrees)
  where
    toMetaEntry :: Tree DocNode -> Either MalformedTreeError (T.Text, Pandoc.MetaValue)
    toMetaEntry (Node (MetaNode (MetaKey key)) [valueTree]) = fmap ((,) key) (toMetaValue noteContentsMap valueTree)
    toMetaEntry _ = Left $ MalformedTreeError "Expected a metadata key with one value"

toMetaValue :: NoteContentsMap -> Tree DocNode -> Either MalformedTreeError Pandoc.MetaValue
toMetaValue noteContentsMap valueTree@(Node (MetaNode (PandocMetaValue metaValue)) childTrees) = case metaValue of
  Pandoc.MetaMap _ -> fmap Pandoc.MetaMap (toMetaEntries noteContentsMap childTrees)
  Pandoc.MetaList _ -> fmap Pandoc.MetaList (traverse (toMetaValue noteContentsMap) childTrees)
  Pandoc.MetaInlines _ -> fmap (Pandoc.MetaInlines . Pandoc.toList . mconcat) (treeToPandocElements noteContentsMap valueTree >>= traverse assertInlines)
  Pandoc.MetaBlocks _ -> fmap Pandoc.MetaBlocks (treeToPandocElements noteContentsMap valueTree >>= getBlocks)
  Pandoc.MetaBool _ -> Right metaValue
  Pandoc.MetaString _ -> Right metaValue
toMetaValue _ _ = Left $ MalformedTreeError "Expected a metadata value"

treeToPandocBlocks :: Tree DocNode -> Either MalformedTreeError [Pandoc.Block]
treeToPandocBlocks tree = treeToPandocElements (buildNoteContentsMap tree) tree >>= getBlocks

treeToPandocElements :: NoteContentsMap -> Tree DocNode -> Either MalformedTreeError [PandocElement]
treeToPandocElements noteContentsMap tree = sequenceA (foldTree (treeNodeToPandocElement noteContentsMap) tree)

treeNodeToPandocElement :: NoteContentsMap -> DocNode -> [[Either MalformedTreeError PandocElement]] -> [Either MalformedTreeError PandocElement]
treeNodeToPandocElement noteContentsMap node childrenNodes = case node of
  Root -> concat childrenNodes
  Metadata -> concat childrenNodes
  Body -> concat childrenNodes
  -- A metadata key or value just returns its children's elements. In practice, only the `PandocMetaValue` node of an
  -- inlines or blocks value gets here, as the root of its subtree.
  MetaNode _ -> concat childrenNodes
  -- TODO: Consider just concatenating children in the case of `Plain`.
  RichTextNode (BlockNode (PandocBlock (Pandoc.Plain _))) -> [fmap (BlockElement . Pandoc.Plain . Pandoc.toList) (concatChildrenInlines childrenNodes)]
  RichTextNode (BlockNode (PandocBlock (Pandoc.Para _))) -> [fmap (BlockElement . Pandoc.Para . Pandoc.toList) (concatChildrenInlines childrenNodes)]
  RichTextNode (BlockNode (PandocBlock (Pandoc.Header level attr _))) -> [fmap (BlockElement . Pandoc.Header level attr . Pandoc.toList) (concatChildrenInlines childrenNodes)]
  RichTextNode (BlockNode (PandocBlock (Pandoc.CodeBlock attr _))) ->
    [ do
        inlines <- concatChildrenInlines childrenNodes
        case firstInline inlines of
          Nothing -> Right $ BlockElement $ Pandoc.CodeBlock attr T.empty
          Just (Pandoc.Str text) -> Right $ BlockElement $ Pandoc.CodeBlock attr text
          _ -> Left $ MalformedTreeError "Could not extract code block text"
    ]
  RichTextNode (BlockNode (PandocBlock (Pandoc.RawBlock format _))) ->
    [ do
        inlines <- concatChildrenInlines childrenNodes
        case firstInline inlines of
          Nothing -> Right $ BlockElement $ Pandoc.RawBlock format T.empty
          Just (Pandoc.Str text) -> Right $ BlockElement $ Pandoc.RawBlock format text
          _ -> Left $ MalformedTreeError "Could not extract raw block text"
    ]
  RichTextNode (BlockNode (ListItem _)) -> concat childrenNodes
  RichTextNode (BlockNode (PandocBlock (Pandoc.BulletList _))) -> [fmap (BlockElement . Pandoc.BulletList) (mapToChildBlocks childrenNodes)]
  RichTextNode (BlockNode (PandocBlock (Pandoc.OrderedList attrs _))) -> [fmap (BlockElement . Pandoc.OrderedList attrs) (mapToChildBlocks childrenNodes)]
  RichTextNode (BlockNode (PandocBlock (Pandoc.BlockQuote _))) -> [fmap (BlockElement . Pandoc.BlockQuote) (traverseAssertingChildIsBlock $ concat childrenNodes)]
  RichTextNode (BlockNode (PandocBlock (Pandoc.Figure attr _ _))) ->
    -- The Figure's own caption payload is empty in the tree; the real caption arrives as a child.
    [ do
        let (captionItems, blockItems) = partition isCaptionItem (concat childrenNodes)
        figCaption <- buildFigureCaption captionItems
        figBlocks <- traverseAssertingChildIsBlock blockItems
        Right $ BlockElement $ Pandoc.Figure attr figCaption figBlocks
    ]
    where
      isCaptionItem (Right (CaptionElement _)) = True
      isCaptionItem _ = False

      buildFigureCaption :: [Either MalformedTreeError PandocElement] -> Either MalformedTreeError Pandoc.Caption
      buildFigureCaption [] = Right Pandoc.emptyCaption
      buildFigureCaption [item] = item >>= assertCaption
      buildFigureCaption _ = Left $ MalformedTreeError "Figure has more than one caption"
  -- Caption blocks live as tree children; the payload slot is always empty here.
  -- Re-emit a `CaptionElement` so the parent Figure/Table folder can pick it out of its children.
  RichTextNode (BlockNode (Caption (Pandoc.Caption short _))) ->
    [ do
        childBlocks <- traverseAssertingChildIsBlock (concat childrenNodes)
        Right $ CaptionElement $ Pandoc.Caption short childBlocks
    ]
  -- FigureContent flows its children up to the Figure folder, symmetric with ListItem.
  RichTextNode (BlockNode (FigureContent _)) -> concat childrenNodes
  RichTextNode (BlockNode (PandocBlock (Pandoc.Div attr _))) -> [fmap (BlockElement . Pandoc.Div attr) (traverseAssertingChildIsBlock $ concat childrenNodes)]
  RichTextNode (BlockNode (PandocBlock (Pandoc.HorizontalRule))) -> [Right $ BlockElement $ Pandoc.HorizontalRule]
  -- Note content subtrees will be mapped to Pandoc notes when handling the note refs.
  RichTextNode (BlockNode (NoteContent _ _)) -> []
  RichTextNode (InlineNode (InlineContent inlineSpans)) -> (fmap . fmap) InlineElement $ inlineSpansToPandocInlines inlineSpans
  -- Kept whole in the tree, as leaves.
  RichTextNode (BlockNode (PandocBlock block@(Pandoc.Table {}))) -> [Right $ BlockElement block]
  RichTextNode (BlockNode (PandocBlock block@(Pandoc.LineBlock _))) -> [Right $ BlockElement block]
  RichTextNode (BlockNode (PandocBlock block@(Pandoc.DefinitionList _))) -> [Right $ BlockElement block]
  where
    concatChildrenInlines :: [[Either MalformedTreeError PandocElement]] -> Either MalformedTreeError Pandoc.Inlines
    concatChildrenInlines children = concatInlines $ map (>>= assertInlines) $ concat children
      where
        concatInlines :: [Either MalformedTreeError Pandoc.Inlines] -> Either MalformedTreeError Pandoc.Inlines
        concatInlines eitherInlines = fmap mconcat $ sequenceA eitherInlines

    inlineSpansToPandocInlines :: [InlineSpan] -> [Either MalformedTreeError Pandoc.Inlines]
    inlineSpansToPandocInlines = map inlineSpanToPandocInlines
      where
        inlineSpanToPandocInlines :: InlineSpan -> Either MalformedTreeError Pandoc.Inlines
        inlineSpanToPandocInlines (NoteRef noteId) = case M.lookup noteId noteContentsMap of
          Just noteContentsSubtree -> do
            noteContentBlockLists <- traverse treeToPandocBlocks noteContentsSubtree
            let noteContentBlocks = concat noteContentBlockLists
            Right $ Pandoc.singleton $ Pandoc.Note noteContentBlocks
          Nothing -> Left $ MalformedTreeError "Found orphan note ref"
        inlineSpanToPandocInlines (InlineText textSpan) = Right $ convertTextSpan textSpan
        inlineSpanToPandocInlines (InlineImage (DocTree.Common.Image attrs altInlines (url, title))) =
          Right $ Pandoc.imageWith attrs url title (Pandoc.fromList altInlines)
        inlineSpanToPandocInlines (PandocInline inline) = Right $ Pandoc.singleton inline

    mapToChildBlocks :: [[Either MalformedTreeError PandocElement]] -> Either MalformedTreeError [[Pandoc.Block]]
    mapToChildBlocks children = (traverse . traverse) (>>= assertBlock) children

    traverseAssertingChildIsBlock :: [Either MalformedTreeError PandocElement] -> Either MalformedTreeError [Pandoc.Block]
    traverseAssertingChildIsBlock children = traverse (>>= assertBlock) children

    firstInline :: Pandoc.Inlines -> Maybe Pandoc.Inline
    firstInline = firstValue

convertTextSpan :: TextSpan -> Pandoc.Inlines
convertTextSpan = convertMarksToInlines <*> convertTextToInlines

convertTextToInlines :: TextSpan -> Pandoc.Inlines
convertTextToInlines = Pandoc.str . value

convertMarksToInlines :: TextSpan -> Pandoc.Inlines -> Pandoc.Inlines
convertMarksToInlines textSpan inlines = foldr markToInlines inlines $ marks textSpan

markToInlines :: Mark -> Pandoc.Inlines -> Pandoc.Inlines
markToInlines mark = case mark of
  StrongMark -> Pandoc.strong
  EmphMark -> Pandoc.emph
  LinkMark (DocTree.Common.Link attrs (url, title)) -> Pandoc.linkWith attrs url title
  CodeMark -> Pandoc.code . concatStrInlines
    where
      concatStrInlines :: Pandoc.Inlines -> T.Text
      concatStrInlines inlines = T.concat [t | Pandoc.Str t <- Pandoc.toList inlines]

getBlocks :: [PandocElement] -> Either MalformedTreeError [Pandoc.Block]
getBlocks = traverse assertBlock

assertBlock :: PandocElement -> Either MalformedTreeError Pandoc.Block
assertBlock (BlockElement block) = Right block
assertBlock (InlineElement _) = Left $ MalformedTreeError "Found orphan inline node"
assertBlock (CaptionElement _) = Left $ MalformedTreeError "Found orphan caption node"

assertInlines :: PandocElement -> Either MalformedTreeError Pandoc.Inlines
assertInlines (BlockElement _) = Left $ MalformedTreeError "Found block node in inline node slot"
assertInlines (InlineElement inlines) = Right $ inlines
assertInlines (CaptionElement _) = Left $ MalformedTreeError "Found caption node in inline node slot"

assertCaption :: PandocElement -> Either MalformedTreeError Pandoc.Caption
assertCaption (CaptionElement c) = Right c
assertCaption (BlockElement _) = Left $ MalformedTreeError "Expected caption, found block"
assertCaption (InlineElement _) = Left $ MalformedTreeError "Expected caption, found inline"

type NoteContentsMap = M.Map NoteId ([Tree DocNode])

buildNoteContentsMap :: Tree DocNode -> NoteContentsMap
buildNoteContentsMap (Node node children) = case node of
  RichTextNode (BlockNode (NoteContent noteId _)) -> M.insert noteId children childMaps
  _ -> childMaps
  where
    childMaps = M.unions (map buildNoteContentsMap children)
