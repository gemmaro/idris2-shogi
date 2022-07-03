module Shogi.Board

import Data.Vect

import Shogi.Column
import Shogi.Piece
import Shogi.Row

%default total

public export
data Board = MkBoard (Vect 9 (Column))

export
fromRows : Vect 9 Row -> Board
fromRows = MkBoard . map MkColumn . reverse . transpose . map toVect

export
map : (Maybe Piece -> Maybe Piece) -> Board -> Board
map f (MkBoard xs) = MkBoard $ map (map f) xs
