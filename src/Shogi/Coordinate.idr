module Shogi.Coordinate

import Shogi.File
import Shogi.Kind
import Shogi.Piece
import Shogi.Rank

%default total

public export
record Coordinate where
  constructor MkCoordinate
  file : File
  rank : Rank

illegalRank : Coordinate -> Piece -> Bool
illegalRank c p = illegalRank c.rank p.kind
