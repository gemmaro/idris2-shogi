module Shogi.Column

import Data.Fin
import Data.Vect

import Shogi.File
import Shogi.Piece

%default total

public export
data Column = MkColumn (Vect 9 (Maybe Piece))

export
toVect : Column -> Vect 9 (Maybe Piece)
toVect (MkColumn c) = c

export
at : File -> Column -> Maybe Piece
at f (MkColumn c) = index (toFin f) c

export
map : (Maybe Piece -> Maybe Piece) -> Column -> Column
map f (MkColumn xs) = MkColumn $ map f xs
