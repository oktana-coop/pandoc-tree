# pandoc-tree

This library exposes data structures that leverage [Haskell's Tree type](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Tree.html) to model a rich text document tree, while leveraging [Pandoc models](https://hackage.haskell.org/package/pandoc-types) (e.g. Pandoc blocks) where possible.

## Tree Shapes

The library exposes two kinds of trees, differing only in how a block's **inline content** is laid out. Assuming the document:

```
# Title

This is **strong text with *emphasis***

More text here.
```

We can get the following trees:

### Grouped-Inlines Tree

Inline content is **one node** holding a _list_ of marked spans (grouped according to their marks). Mirrors how [ProseMirror handles inline content](https://prosemirror.net/docs/guide/#doc).

```
Root (Meta)
  ├── Header 1
  │   └── InlineNode  [ "Title" ]
  ├── Para
  │   └── InlineNode  [ "This is " | "strong text with " ⟨Strong⟩ | "emphasis" ⟨Strong,Emph⟩ ]
  └── Para
      └── InlineNode  [ "More text here." ]
```

### Leaf-Text-Spans Tree

Each inline span is its **own leaf node** under a wrapper inline node. A new leaf usually begins **wherever the mark set changes**:

```
Root (Meta)
  ├── Header 1
  │   └── InlineNode
  │       └── "Title"
  ├── Para
  │   └── InlineNode
  │       ├── "This is "
  │       ├── "strong text with "   {Strong}
  │       └── "emphasis"            {Strong, Emph}
  └── Para
      └── InlineNode
          └── "More text here."
```

Marks are only the default reason to start a new leaf. Because each span is its own node, the same "break a new leaf wherever a per-span property changes" rule generalizes to **any** per-span data a consumer wants to carry. When computing edits, for example, spans can be segmented by their marks _and_ by an edit operation (insert / delete / copy), so each leaf is a run that shares the same marks _and_ the same operation. This makes the leaf-text-spans tree a natural substrate for **diffing and other per-span annotations**, while the grouped-inlines tree stays focused on faithful representation and round-tripping.

## Testing

Navigate to the project root folder and run the following command to build and run the tests:

```
stack build --test
```
