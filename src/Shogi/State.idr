module Shogi.State

import Shogi.Board
import Shogi.Hand
import Shogi.Piece

%default total

public export
record State where
  constructor MkState
  board : Board
  blackHand, whiteHand : Hand

export
map : (Maybe Piece -> Maybe Piece) -> State -> State
map f (MkState board b w) = MkState (map f board) b w
