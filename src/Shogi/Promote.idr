module Shogi.Promote

import Data.Either
import Shogi.Piece
import Shogi.Kind

%default total

public export
data PromoteError = Already | Constant

public export
interface Promote a where
  promote : a -> Either PromoteError a

  promotable : a -> Bool
  promotable = isRight . promote

  promoted : a -> Bool
  promoted x with (promote x)
    _ | (Left Already) = True
    _ | _              = False

export
Promote Kind where
  promote King   = Left Constant
  promote Gold   = Left Constant
  promote Rook   = Right Dragon
  promote Bishop = Right Horse
  promote Silver = Right PromotedSilver
  promote Knight = Right PromotedKnight
  promote Lance  = Right PromotedLance
  promote Pawn   = Right PromotedPawn
  promote _      = Left Already

export
holdable : Kind -> Bool
holdable Gold = True
holdable k    = promotable k

export
[ Revert ] Promote Kind where
  promote King           = Left Constant
  promote Gold           = Left Constant
  promote Dragon         = Right Rook
  promote Horse          = Right Bishop
  promote PromotedSilver = Right Silver
  promote PromotedKnight = Right Knight
  promote PromotedLance  = Right Lance
  promote PromotedPawn   = Right Pawn
  promote _              = Left Already

Promote Piece where
  promote p = Right $ { kind := !(promote p.kind) } p

namespace Revert
  [ Revert ] Promote Piece where
    promote p = Right $ { kind := !(promote @{Revert} p.kind) } p
