module Shogi.Relative

import Shogi.Color
import Shogi.Coordinate
import Shogi.Rank
import Shogi.Reverse

%default total

public export
interface Relative a where
  relative : Color -> a -> a

export
Relative Rank where
  relative Black = id
  relative White = reverse

export
Relative Coordinate where
  relative c = { rank $= relative c }
