# pandoc-tree

This library exposes data structures that leverage [Haskell's Tree type](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Tree.html) to model a rich text document tree, while leveraging [Pandoc models](https://hackage.haskell.org/package/pandoc-types) (e.g. Pandoc blocks) where possible.

Specifically, it exposes two kinds of trees:

1. One with grouped (according to their marks) inline text spans, similar to how [ProseMirror handles inline content](https://prosemirror.net/docs/guide/#doc).
2. One with text spans as leaf nodes (separate from the inline tree node).
