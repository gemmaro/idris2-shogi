module Shogi.Sfen

import Data.Fin
import Data.List
import Data.String.Parser
import Data.Vect
import Shogi.Board
import Shogi.Color
import Shogi.Coordinate
import Shogi.File
import Shogi.Kind
import Shogi.Piece
import Shogi.Promote
import Shogi.Rank
import Shogi.Row
import Shogi.State

%default total

-- Color

export
color : Parser Color
color =
  (char 'b' $> Black) <|>
  (char 'w' $> White)

serializeColor : Color -> Char
serializeColor Black = 'b'
serializeColor White = 'w'

-- Fin 9

fin9 : Parser (Fin 9)
fin9 =
  (char '1' $> 0) <|>
  (char '2' $> 1) <|>
  (char '3' $> 2) <|>
  (char '4' $> 3) <|>
  (char '5' $> 4) <|>
  (char '6' $> 5) <|>
  (char '7' $> 6) <|>
  (char '8' $> 7) <|>
  (char '9' $> 8)

serializeFin9 : Fin 9 -> Char
serializeFin9 0 = '1'
serializeFin9 1 = '2'
serializeFin9 2 = '3'
serializeFin9 3 = '4'
serializeFin9 4 = '5'
serializeFin9 5 = '6'
serializeFin9 6 = '7'
serializeFin9 7 = '8'
serializeFin9 8 = '9'

-- File

export
file : Parser File
file = do
  d <- fin9
  pure $ MkFile d

serializeFile : File -> Char
serializeFile (MkFile x) = serializeFin9 x

-- Rank

export
rank : Parser Rank
rank = do
  a <- (char 'a' $> 0) <|>
       (char 'b' $> 1) <|>
       (char 'c' $> 2) <|>
       (char 'd' $> 3) <|>
       (char 'e' $> 4) <|>
       (char 'f' $> 5) <|>
       (char 'g' $> 6) <|>
       (char 'h' $> 7) <|>
       (char 'i' $> 8)
  pure $ MkRank a

serializeRank : Rank -> Char
serializeRank = serializeFin9 . toFin

-- Coordinate

export
coordinate : Parser Coordinate
coordinate = do
  f <- file
  r <- rank
  pure $ MkCoordinate f r

serializeCoordinate : Coordinate -> String
serializeCoordinate c = pack $ [serializeFile c.file, serializeRank c.rank]

-- Kind

export
blackRawKind : Parser Kind
blackRawKind =
  (char 'K' $> King) <|>
  (char 'R' $> Rook) <|>
  (char 'B' $> Bishop) <|>
  (char 'G' $> Gold) <|>
  (char 'S' $> Silver) <|>
  (char 'N' $> Knight) <|>
  (char 'L' $> Lance) <|>
  (char 'P' $> Pawn)

export
whiteRawKind : Parser Kind
whiteRawKind =
  (char 'k' $> King) <|>
  (char 'r' $> Rook) <|>
  (char 'b' $> Bishop) <|>
  (char 'g' $> Gold) <|>
  (char 's' $> Silver) <|>
  (char 'n' $> Knight) <|>
  (char 'l' $> Lance) <|>
  (char 'p' $> Pawn)

export
promote : Parser Bool
promote =
  char '+' $> True <|>
  pure False

export
blackKind : Parser Kind
blackKind = do
  p <- promote
  k <- blackRawKind
  if p then do case promote k of
                 Right k => pure k
                 _ => empty
       else pure k

export
whiteKind : Parser Kind
whiteKind = do
  p <- promote
  k <- whiteRawKind
  if p then do case promote k of
                 Right k => pure k
                 _ => empty
       else pure k

serializeBlackKind : Kind -> String
serializeBlackKind King           = "K"
serializeBlackKind Rook           = "R"
serializeBlackKind Dragon         = "+R"
serializeBlackKind Bishop         = "B"
serializeBlackKind Horse          = "+B"
serializeBlackKind Gold           = "G"
serializeBlackKind Silver         = "S"
serializeBlackKind PromotedSilver = "+S"
serializeBlackKind Knight         = "N"
serializeBlackKind PromotedKnight = "+N"
serializeBlackKind Lance          = "L"
serializeBlackKind PromotedLance  = "+L"
serializeBlackKind Pawn           = "P"
serializeBlackKind PromotedPawn   = "+P"

serializeWhiteKind : Kind -> String
serializeWhiteKind King           = "k"
serializeWhiteKind Rook           = "r"
serializeWhiteKind Dragon         = "+r"
serializeWhiteKind Bishop         = "b"
serializeWhiteKind Horse          = "+b"
serializeWhiteKind Gold           = "g"
serializeWhiteKind Silver         = "s"
serializeWhiteKind PromotedSilver = "+s"
serializeWhiteKind Knight         = "n"
serializeWhiteKind PromotedKnight = "+n"
serializeWhiteKind Lance          = "l"
serializeWhiteKind PromotedLance  = "+l"
serializeWhiteKind Pawn           = "p"
serializeWhiteKind PromotedPawn   = "+p"

-- Move

export
record Migration where
  constructor MkMigration
  from, to : Coordinate
  promote : Bool

export
record Drop where
  constructor MkDrop
  kind : Kind
  at : Coordinate

export
data Move =
  MigrationMove Migration |
  DropMove Drop

serializeMove : Move -> String
serializeMove (MigrationMove m) with (m.promote)
  serializeMove (MigrationMove m) | False = serializeCoordinate m.from ++ serializeCoordinate m.to
  serializeMove (MigrationMove m) | True  = serializeCoordinate m.from ++ serializeCoordinate m.to ++ "+"
serializeMove (DropMove m) = serializeBlackKind m.kind ++ "*" ++ serializeCoordinate m.at

export
move : Parser Move
move =
  ( do f <- coordinate
       t <- coordinate
       p <- succeeds $ char '+'
       pure $ MigrationMove $ MkMigration f t p ) <|>
  ( do k <- blackRawKind
       ignore $ char '*'
       c <- coordinate
       pure $ DropMove $ MkDrop k c )

-- Piece

serializePiece : Piece -> String
serializePiece p with (p.color)
  serializePiece p | Black = serializeBlackKind p.kind
  serializePiece p | White = serializeWhiteKind p.kind

export
piece : Parser Piece
piece =
  (blackKind <&> MkPiece Black) <|>
  (whiteKind <&> MkPiece White)

-- Chunk

||| 左から右方向の部分的な升の並びです
data Chunk =
  PieceChunk Piece |
  SpacesChunk (Fin 9)

export
chunk : Parser Chunk
chunk = do
  (piece <&> PieceChunk) <|>
  (fin9 <&> SpacesChunk)

-- Row

fromChunks : List Chunk -> Maybe Row
fromChunks l = map MkRow $ fromChunks' [] l
  where
    fromChunks' : List (Maybe Piece) -> List Chunk -> Maybe (Vect 9 (Maybe Piece))
    fromChunks' r@[_, _, _, _, _, _, _, _, _] [] = Just $ fromList (reverse r)
    fromChunks' _ [] = Nothing
    fromChunks' r ((PieceChunk p) :: cs) = fromChunks' (r ++ [Just p]) cs
    fromChunks' r ((SpacesChunk n) :: cs) = fromChunks' (r ++ replicate (finToNat n + 1) Nothing) cs

-- TODO: This is not total except for executable form by Chez Scheme backend.
covering
export
row : Parser Row
row = do
  l <- some chunk
  case fromChunks l of
       Nothing => fail "Sum of chunks exceeds limit"
       Just r => pure r

-- Board

rowSeparator : Parser Char
rowSeparator = char '/'

export
covering
board : Parser Board
board = do
  r0 <- row
  ignore $ rowSeparator
  r1 <- row
  ignore $ rowSeparator
  r2 <- row
  ignore $ rowSeparator
  r3 <- row
  ignore $ rowSeparator
  r4 <- row
  ignore $ rowSeparator
  r5 <- row
  ignore $ rowSeparator
  r6 <- row
  ignore $ rowSeparator
  r7 <- row
  ignore $ rowSeparator
  r8 <- row
  pure $ fromRows [r0, r1, r2, r3, r4, r5, r6, r7, r8]

-- Hand

-- TODO

-- State (state and ply)

-- TODO
