module Shogi.Deadend

import Shogi.Rank
import Shogi.Kind
import Shogi.Coordinate
import Shogi.Piece

%default total

export
legal : Rank -> Kind -> Bool
legal r Knight = toFin r >= 2
legal r Lance  = toFin r >= 1
legal r Pawn   = toFin r >= 1
legal _ _      = True
