module Shogi.Territory

import Data.Fin

import Shogi.Color
import Shogi.Coordinate
import Shogi.Rank

%default total

public export
interface Territory a where
  isTerritory : Color -> a -> Bool

export
Territory Rank where
  isTerritory Black r = (toFin r) < 3
  isTerritory White r = (toFin r) > 5

export
Territory Coordinate where
  isTerritory color coordinate =
    isTerritory color coordinate.rank
