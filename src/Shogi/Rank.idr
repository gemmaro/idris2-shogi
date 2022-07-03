module Shogi.Rank

import Data.Fin

import Shogi.Color

%default total

public export
data Rank = MkRank (Fin 9)

export
Eq Rank where
  (MkRank r) == (MkRank r') = r == r'

export
Ord Rank where
  compare (MkRank r) (MkRank r') = compare r r'

export
toFin : Rank -> Fin 9
toFin (MkRank r) = r
