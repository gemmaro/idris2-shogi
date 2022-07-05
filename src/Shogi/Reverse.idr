module Shogi.Reverse

import Data.Fin.Extra
import Data.Vect

import Shogi.Board
import Shogi.Color
import Shogi.Column
import Shogi.File
import Shogi.Piece
import Shogi.Rank
import Shogi.Row

%default total

public export
interface Reverse a where
  reverse : a -> a

export
Reverse Column where
  reverse = MkColumn . reverse . toVect

export
[ Vertical ] Reverse Board where
  reverse (MkBoard xs) = MkBoard $ map reverse $ xs

export
[ Horizontal ] Reverse Board where
  reverse (MkBoard xs) = MkBoard $ map MkColumn $ transpose $ map toVect xs

export
Reverse Board where
  reverse = reverse @{Vertical} . reverse @{Horizontal}

export
Reverse Color where
  reverse Black = White
  reverse White = Black

export
Reverse File where
  reverse (MkFile x) = MkFile $ invFin x

Reverse Piece where
  reverse p = { color := reverse p.color } p

export
Reverse Rank where
  reverse = MkRank . invFin . toFin

export
Reverse Row where
  reverse = MkRow . reverse . toVect
