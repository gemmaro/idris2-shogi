module Shogi.Rank

import public Data.Fin

%default total

public export
data Rank = MkRank (Fin 9)

export
toFin : Rank -> Fin 9
toFin (MkRank r) = r

export
Eq Rank where
  (==) = (==) `on` toFin

export
Ord Rank where
  compare = compare `on` toFin
