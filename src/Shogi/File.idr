module Shogi.File

import Data.Fin

%default total

public export
data File = MkFile (Fin 9)

export
toFin : File -> Fin 9
toFin (MkFile x) = x
