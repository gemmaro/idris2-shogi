||| Reference: [Haskell `containers` package][containers]
|||
||| [containers]: https://hackage.haskell.org/package/containers
module Shogi.Tree

%default total

public export
record Tree a where
  constructor MkTree
  rootLabel : a
  subForest : List (Tree a)

record Forest a where
  constructor MkForest
  value : List (Tree a)
