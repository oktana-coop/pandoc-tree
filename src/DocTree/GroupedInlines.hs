{-# LANGUAGE OverloadedStrings #-}

module DocTree.GroupedInlines (BlockNode (..), InlineSpan (..), InlineNode (..), DocNode (..), TreeNode (..), toTree, toPandoc) where

import Control.Monad.Except (throwError)
import Control.Monad.State (State, get, modify, runState)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tree (Tree (Node), foldTree, unfoldForestM, unfoldTreeM)
import DocTree.Common (BlockNode (..), InlineSpan (..), LinkMark (..), Mark (..), NoteId (..), TextSpan (..))
import Text.Pandoc (PandocError (PandocSyntaxMapError))
import Text.Pandoc.Builder as Pandoc
  ( Block (..),
    Blocks,
    Inlines,
    Pandoc,
    code,
    doc,
    emph,
    fromList,
    linkWith,
    singleton,
    str,
    strong,
    toList,
  )
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Inline (..), Pandoc (..))
import Utils.Sequence (firstValue)

data InlineNode = InlineContent [InlineSpan] deriving (Show, Eq)

instance Semigroup InlineNode where
  (<>) (InlineContent spans1) (InlineContent spans2) = InlineContent (spans1 <> spans2)

instance Monoid InlineNode where
  mempty = InlineContent []

data TreeNode = BlockNode BlockNode | InlineNode InlineNode deriving (Show, Eq)

data DocNode = Root | TreeNode TreeNode deriving (Show, Eq)

data NoteData = NoteData
  { noteCounter :: Int,
    -- Accumulated note content block nodes
    noteContents :: [Tree DocNode]
  }

type NotesState = State NoteData

toTree :: Pandoc.Pandoc -> Tree DocNode
toTree (Pandoc.Pandoc _ blocks) = Node Root forestWithNotes
  where
    blockNodes = map (BlockNode . PandocBlock) blocks
    (mainForest, notesState) = runState (unfoldForestM treeNodeUnfolder blockNodes) initialState
    initialState = NoteData 0 []
    noteTrees = noteContents notesState
    forestWithNotes = mainForest <> noteTrees

treeNodeUnfolder :: TreeNode -> NotesState (DocNode, [TreeNode])
treeNodeUnfolder (BlockNode blockNode) = blockTreeNodeUnfolder blockNode
treeNodeUnfolder (InlineNode inlineNode) = return $ inlineTreeNodeUnfolder inlineNode

blockTreeNodeUnfolder :: BlockNode -> NotesState (DocNode, [TreeNode])
blockTreeNodeUnfolder (PandocBlock block) = case block of
  Pandoc.Plain inlines -> do
    inlineNode <- buildInlineNode inlines
    return ((TreeNode . BlockNode . PandocBlock . Pandoc.Plain) [], [inlineNode])
  Pandoc.Para inlines -> do
    inlineNode <- buildInlineNode inlines
    return ((TreeNode . BlockNode . PandocBlock . Pandoc.Para) [], [inlineNode])
  Pandoc.Header level attrs inlines -> do
    inlineNode <- buildInlineNode inlines
    return (TreeNode . BlockNode $ PandocBlock $ Pandoc.Header level attrs [], [inlineNode])
  Pandoc.CodeBlock attrs text -> return (TreeNode . BlockNode $ PandocBlock $ Pandoc.CodeBlock attrs T.empty, [InlineNode $ InlineContent $ [InlineText $ TextSpan {value = text, marks = []}]])
  Pandoc.BulletList items -> return ((TreeNode . BlockNode . PandocBlock . Pandoc.BulletList) [], map (BlockNode . ListItem) items)
  Pandoc.OrderedList attrs items -> return (TreeNode $ BlockNode $ PandocBlock $ Pandoc.OrderedList attrs [], map (BlockNode . ListItem) items)
  Pandoc.BlockQuote children -> return ((TreeNode . BlockNode . PandocBlock . Pandoc.BlockQuote) [], map (BlockNode . PandocBlock) children)
  _ -> undefined
blockTreeNodeUnfolder (ListItem children) = return ((TreeNode . BlockNode . ListItem) [], map (BlockNode . PandocBlock) children)
blockTreeNodeUnfolder (NoteContent noteId children) = return (TreeNode $ BlockNode $ NoteContent noteId [], map (BlockNode . PandocBlock) children)

buildInlineNode :: [Pandoc.Inline] -> NotesState TreeNode
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
  Pandoc.Strong inlines -> do
    wrappedSpans <- pandocInlinesToSpans inlines
    return $ addMark StrongMark wrappedSpans
  Pandoc.Emph inlines -> do
    wrappedSpans <- pandocInlinesToSpans inlines
    return $ addMark EmphMark wrappedSpans
  Pandoc.Link attrs inlines target -> do
    wrappedSpans <- pandocInlinesToSpans inlines
    return $ addMark (LinkMark $ DocTree.Common.Link attrs target) wrappedSpans
  -- TODO: Handle code attrs
  Pandoc.Code _ s -> return [InlineText $ TextSpan s [CodeMark]]
  Pandoc.Note noteBlocks -> do
    -- Generate note ID and create note content
    notesState <- get
    let newNoteId = noteCounter notesState + 1
        noteIdText = T.pack $ show newNoteId
        noteId = NoteId noteIdText

    -- Convert note blocks to spans
    noteContentTree <- unfoldTreeM treeNodeUnfolder (BlockNode $ NoteContent noteId noteBlocks)

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
  -- TODO: Handle other inline elements
  _ -> return []

addMark :: Mark -> [InlineSpan] -> [InlineSpan]
addMark mark spans = fmap (addMarkToSpan mark) spans
  where
    addMarkToSpan :: Mark -> InlineSpan -> InlineSpan
    addMarkToSpan m (InlineText textSpan) = InlineText $ TextSpan T.empty [m] <> textSpan
    -- Leave non-text spans (like note refs) unchanged
    addMarkToSpan _ otherSpan = otherSpan

inlineTreeNodeUnfolder :: InlineNode -> (DocNode, [TreeNode])
inlineTreeNodeUnfolder inlineNode = (TreeNode $ InlineNode inlineNode, [])

data BlockOrInlines = BlockElement Pandoc.Block | InlineElement Pandoc.Inlines

toPandoc :: (PandocMonad m) => Tree DocNode -> m Pandoc.Pandoc
toPandoc = either throwError (pure . Pandoc.doc) . treeToPandocBlocks

treeToPandocBlocks :: Tree DocNode -> Either PandocError Pandoc.Blocks
treeToPandocBlocks tree = sequenceA (foldTree (treeNodeToPandocBlockOrInlines noteContentsMap) tree) >>= getBlockSeq
  where
    noteContentsMap = buildNoteContentsMap tree

treeNodeToPandocBlockOrInlines :: NoteContentsMap -> DocNode -> [[Either PandocError BlockOrInlines]] -> [Either PandocError BlockOrInlines]
treeNodeToPandocBlockOrInlines noteContentsMap node childrenNodes = case node of
  Root -> concat childrenNodes
  -- TODO: Consider just concatenating children in the case of `Plain`.
  TreeNode (BlockNode (PandocBlock (Pandoc.Plain _))) -> [fmap (BlockElement . Pandoc.Para . Pandoc.toList) (concatChildrenInlines childrenNodes)]
  TreeNode (BlockNode (PandocBlock (Pandoc.Para _))) -> [fmap (BlockElement . Pandoc.Para . Pandoc.toList) (concatChildrenInlines childrenNodes)]
  TreeNode (BlockNode (PandocBlock (Pandoc.Header level attr _))) -> [fmap (BlockElement . Pandoc.Header level attr . Pandoc.toList) (concatChildrenInlines childrenNodes)]
  TreeNode (BlockNode (PandocBlock (Pandoc.CodeBlock attr _))) ->
    [ do
        inlines <- concatChildrenInlines childrenNodes
        case firstInline inlines of
          Just (Str text) -> Right $ BlockElement $ Pandoc.CodeBlock attr text
          _ -> Left $ PandocSyntaxMapError "Error in mapping: Could not extract code block text"
    ]
  TreeNode (BlockNode (ListItem _)) -> concat childrenNodes
  TreeNode (BlockNode (PandocBlock (Pandoc.BulletList _))) -> [fmap (BlockElement . Pandoc.BulletList) (mapToChildBlocks childrenNodes)]
  TreeNode (BlockNode (PandocBlock (Pandoc.OrderedList attrs _))) -> [fmap (BlockElement . Pandoc.OrderedList attrs) (mapToChildBlocks childrenNodes)]
  TreeNode (BlockNode (PandocBlock (Pandoc.BlockQuote _))) -> [fmap (BlockElement . Pandoc.BlockQuote) (traverseAssertingChildIsBlock $ concat childrenNodes)]
  -- Note content subtrees will be mapped to Pandoc notes when handling the note refs.
  TreeNode (BlockNode (NoteContent _ _)) -> []
  TreeNode (InlineNode (InlineContent inlineSpans)) -> (fmap . fmap) InlineElement $ inlineSpansToPandocInlines inlineSpans
  -- TODO: Iteratively handle more blocks
  _ -> undefined
  where
    concatChildrenInlines :: [[Either PandocError BlockOrInlines]] -> Either PandocError Pandoc.Inlines
    concatChildrenInlines children = concatInlines $ map (>>= assertInlines) $ concat children
      where
        concatInlines :: [Either PandocError Pandoc.Inlines] -> Either PandocError Pandoc.Inlines
        concatInlines eitherInlines = fmap mconcat $ sequenceA eitherInlines

    inlineSpansToPandocInlines :: [InlineSpan] -> [Either PandocError Pandoc.Inlines]
    inlineSpansToPandocInlines = map inlineSpanToPandocInlines
      where
        inlineSpanToPandocInlines :: InlineSpan -> Either PandocError Pandoc.Inlines
        inlineSpanToPandocInlines (NoteRef noteId) = case M.lookup noteId noteContentsMap of
          Just noteContentsSubtree -> do
            noteContentBlockSequences <- traverse treeToPandocBlocks noteContentsSubtree
            let noteContentBlocks = concatMap toList noteContentBlockSequences
            Right $ singleton $ Pandoc.Note noteContentBlocks
          Nothing -> Left $ PandocSyntaxMapError "Error in mapping: Found orphan note ref"
        inlineSpanToPandocInlines (InlineText textSpan) = Right $ convertTextSpan textSpan

    mapToChildBlocks :: [[Either PandocError BlockOrInlines]] -> Either PandocError [[Pandoc.Block]]
    mapToChildBlocks children = (traverse . traverse) (>>= assertBlock) children

    traverseAssertingChildIsBlock :: [Either PandocError BlockOrInlines] -> Either PandocError [Pandoc.Block]
    traverseAssertingChildIsBlock children = traverse (>>= assertBlock) children

    firstInline :: Pandoc.Inlines -> Maybe Pandoc.Inline
    firstInline = firstValue

convertTextSpan :: TextSpan -> Pandoc.Inlines
convertTextSpan = convertMarksToInlines <*> convertTextToInlines

convertTextToInlines :: TextSpan -> Pandoc.Inlines
convertTextToInlines = Pandoc.str . value

convertMarksToInlines :: TextSpan -> Pandoc.Inlines -> Pandoc.Inlines
convertMarksToInlines textSpan inlines = foldl' (flip markToInlines) inlines $ marks textSpan

markToInlines :: Mark -> Pandoc.Inlines -> Pandoc.Inlines
markToInlines mark = case mark of
  StrongMark -> Pandoc.strong
  EmphMark -> Pandoc.emph
  LinkMark (DocTree.Common.Link attrs (url, title)) -> Pandoc.linkWith attrs url title
  CodeMark -> Pandoc.code . concatStrInlines
    where
      concatStrInlines :: Inlines -> T.Text
      concatStrInlines inlines = T.concat [t | Pandoc.Str t <- Pandoc.toList inlines]

getBlockSeq :: [BlockOrInlines] -> Either PandocError Pandoc.Blocks
getBlockSeq = fmap Pandoc.fromList . traverse assertBlock

assertBlock :: BlockOrInlines -> Either PandocError Pandoc.Block
assertBlock (BlockElement block) = Right block
assertBlock (InlineElement _) = Left $ PandocSyntaxMapError "Error in mapping: found orphan inline node"

assertInlines :: BlockOrInlines -> Either PandocError Pandoc.Inlines
assertInlines (BlockElement _) = Left $ PandocSyntaxMapError "Error in mapping: found block node in inline node slot"
assertInlines (InlineElement inlines) = Right $ inlines

type NoteContentsMap = M.Map NoteId ([Tree DocNode])

buildNoteContentsMap :: Tree DocNode -> NoteContentsMap
buildNoteContentsMap (Node node children) = case node of
  TreeNode (BlockNode (NoteContent noteId _)) -> M.insert noteId children childMaps
  _ -> childMaps
  where
    childMaps = M.unions (map buildNoteContentsMap children)
