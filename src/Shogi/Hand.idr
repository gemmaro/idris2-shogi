module Shogi.Hand

import Data.Fin

%default total

public export
record Hand where
  constructor MkHand
  rook, bishop : Fin 3
  gold, silver, knight, lance : Fin 5
  pawn : Fin 19

export
empty : Hand
empty = MkHand 0 0 0 0 0 0 0
