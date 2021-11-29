module Sizing where
import Geometry(pmax)

data Sizing
  = Static | Growing | Dynamic
--  | StaticDebug
  deriving (Eq,Show,Read)

newSize sizing curSize reqSize =
  case sizing of
    Static  -> curSize
--    StaticDebug  -> curSize
    Growing -> pmax curSize reqSize
    Dynamic -> reqSize
