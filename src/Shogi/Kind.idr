module Shogi.Kind

import Data.Either
import Data.Vect

import Shogi.Rank

%default total

public export
data Kind =
  King   |
  Rook   | Dragon         |
  Bishop | Horse          |
  Gold   |
  Silver | PromotedSilver |
  Knight | PromotedKnight |
  Lance  | PromotedLance  |
  Pawn   | PromotedPawn

export
kinds : Vect 14 Kind
kinds =
  [ King
  , Rook   , Dragon
  , Bishop , Horse
  , Gold
  , Silver , PromotedSilver
  , Knight , PromotedKnight
  , Lance  , PromotedLance
  , Pawn   , PromotedPawn
  ]

export
illegalRank : Rank -> Kind -> Bool
illegalRank r Knight = toFin r < 2
illegalRank r Lance  = toFin r < 1
illegalRank r Pawn   = toFin r < 1
illegalRank _ _      = False

export
Eq Kind where
  King           == King           = True
  Rook           == Rook           = True
  Dragon         == Dragon         = True
  Bishop         == Bishop         = True
  Horse          == Horse          = True
  Gold           == Gold           = True
  Silver         == Silver         = True
  PromotedSilver == PromotedSilver = True
  Knight         == Knight         = True
  PromotedKnight == PromotedKnight = True
  Lance          == Lance          = True
  PromotedLance  == PromotedLance  = True
  Pawn           == Pawn           = True
  PromotedPawn   == PromotedPawn   = True
  _              == _              = False
