module Shogi.Row

import Data.Vect
import Shogi.Piece

%default total

public export
data Row = MkRow (Vect 9 (Maybe Piece))

export
toVect : Row -> Vect 9 (Maybe Piece)
toVect (MkRow c) = c
