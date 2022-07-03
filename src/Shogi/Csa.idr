module Shogi.Csa

import Data.String.Parser

import Shogi.Color
import Shogi.Kind
import Shogi.Piece

%default total

black : Char
black = '+'

white : Char
white = '-'

export
color : Parser Color
color = (char black $> Black) <|> (char white $> White)

export
serializeColor : Color -> Char
serializeColor Black = black
serializeColor White = white

export
serializeKind : Kind -> String
serializeKind King           = "OU"
serializeKind Rook           = "HI"
serializeKind Dragon         = "RY"
serializeKind Bishop         = "KA"
serializeKind Horse          = "UM"
serializeKind Gold           = "KI"
serializeKind Silver         = "GI"
serializeKind PromotedSilver = "NG"
serializeKind Knight         = "KE"
serializeKind PromotedKnight = "NK"
serializeKind Lance          = "KY"
serializeKind PromotedLance  = "NY"
serializeKind Pawn           = "FU"
serializeKind PromotedPawn   = "TO"

export
kind : Parser Kind
kind =
  (string "OU" $> King)           <|>
  (string "HI" $> Rook)           <|>
  (string "RY" $> Dragon)         <|>
  (string "KA" $> Bishop)         <|>
  (string "UM" $> Horse)          <|>
  (string "KI" $> Gold)           <|>
  (string "GI" $> Silver)         <|>
  (string "NG" $> PromotedSilver) <|>
  (string "KE" $> Knight)         <|>
  (string "NK" $> PromotedKnight) <|>
  (string "KY" $> Lance)          <|>
  (string "NY" $> PromotedLance)  <|>
  (string "FU" $> Pawn)           <|>
  (string "TO" $> PromotedPawn)

export
serializePiece : Piece -> String
serializePiece p = cast (serializeColor p.color) ++ serializeKind p.kind

export
piece : Parser Piece
piece = do
  c <- color
  k <- kind
  pure $ MkPiece c k
