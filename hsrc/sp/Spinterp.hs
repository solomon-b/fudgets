module Spinterp(interpSP) where
import SP

interpSP put get null' sp =
    case sp of
      PutSP x sp' -> put x (interpSP put get null' sp')
      GetSP xsp -> get (\x -> interpSP put get null' (xsp x))
      NullSP -> null'

