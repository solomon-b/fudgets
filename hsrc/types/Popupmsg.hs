module Popupmsg where
import Geometry

data PopupMsg a = Popup Point a | Popdown  deriving (Eq, Ord)

