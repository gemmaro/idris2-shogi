module Shogi.Pickup

import Data.Vect
import Data.Fin

import Shogi.Coordinate
import Shogi.Piece
import Shogi.Board
import Shogi.File
import Shogi.Rank
import Shogi.Column
import Shogi.State

%default total

public export
interface Pickup a where
  pickup : Coordinate -> a -> Maybe Piece

export
Pickup Board where
  pickup (MkCoordinate f r) (MkBoard xs) = index (toFin r) $ toVect $ index (toFin f) xs

export
Pickup State where
  pickup c s = pickup c s.board
