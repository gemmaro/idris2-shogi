module Shogi.Color

import Data.Vect

%default total

public export
data Color = Black | White

export
variants : Vect 2 Color
variants = [Black, White]

export
toFin : Color -> Fin 2
toFin Black = 0
toFin White = 1

export
fromFin : Fin 2 -> Color
fromFin 0 = Black
fromFin 1 = White

export
Eq Color where
  Black == Black = True
  White == White = True
  _     == _     = False
