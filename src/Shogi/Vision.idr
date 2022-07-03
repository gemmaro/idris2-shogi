module Shogi.Vision

import Data.Vect
import Shogi.Kind

data Delta = Negative (Fin 8) | Zero | Positive (Fin 8)

record Vector where
  constructor MkVector
  file : Delta
  rank : Delta

forth : Vector
forth = MkVector Zero (Negative 0)

back : Vector
back = MkVector Zero (Positive 0)

left : Vector
left = MkVector (Positive 0) Zero

right : Vector
right = MkVector (Negative 0) Zero

forthRight : Vector
forthRight = MkVector (Negative 0) (Negative 0)

forthLeft : Vector
forthLeft = MkVector (Positive 0) (Negative 0)

backRight : Vector
backRight = MkVector (Negative 0) (Positive 0)

backLeft : Vector
backLeft = MkVector (Positive 0) (Positive 0)

record Vision where
  constructor MkVision
  hop : List Vector
  fly : List Vector

gold : Vision
gold = MkVision [forthRight, forth, forthLeft, right, left, back] []

pawn : Vision
pawn = MkVision [forth] []

lance : Vision
lance = MkVision [] [forth]

knight : Vision
knight = MkVision [MkVector (Negative 0) (Negative 1), MkVector (Positive 0) (Negative 1)] []

silver : Vision
silver = MkVision [forthRight, forth, forthLeft, backRight, backLeft] []

bishop : Vision
bishop = MkVision [] [forthRight, forthLeft, backRight, backLeft]

rook : Vision
rook = MkVision [] [forth, right, left, back]

king : Vision
king = MkVision [forthRight, forth, forthLeft, right, left, backRight, back, backLeft] []

horse : Vision
horse = MkVision [forthRight, forthLeft, backRight, backLeft] [forth, right, left, back]

dragon : Vision
dragon = MkVision [forth, right, left, back] [forthRight, forthLeft, backRight, backLeft]

vision : Kind -> Vision
vision King           = king
vision Rook           = rook
vision Dragon         = dragon
vision Bishop         = bishop
vision Horse          = horse
vision Gold           = gold
vision Silver         = silver
vision PromotedSilver = gold
vision Knight         = knight
vision PromotedKnight = gold
vision Lance          = lance
vision PromotedLance  = gold
vision Pawn           = pawn
vision PromotedPawn   = gold
