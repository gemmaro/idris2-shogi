module Shogi.Setting

import Shogi.State
import Shogi.Color
import Shogi.Preset

%default total

data Setting = Custom State Color
             | Preset Preset
