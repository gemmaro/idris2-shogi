module Shogi.Piece

import Shogi.Color
import Shogi.Kind

%default total

public export
record Piece where
  constructor MkPiece
  color : Color
  kind  : Kind

export
isBlack : Piece -> Bool
isBlack (MkPiece Black _) = True
isBlack (MkPiece White _) = False

export
isWhite : Piece -> Bool
isWhite = not . isBlack

export
Eq Piece where
  (==) = (==) `on` (\p => (p.color, p.kind))
