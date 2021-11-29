module LayoutHints(module LayoutHints,LayoutHint(..)) where
import Fudget
import LayoutRequest
import FRequest
--import Geometry(Point, Size(..),Rect)
import NullF(putMessageFu)
--import Command

serHint  = "ser" :: LayoutHint
parHint  = "par" :: LayoutHint
listHint = "list" :: LayoutHint
loopHint = "loop" :: LayoutHint

layoutHintF :: LayoutHint -> F a b -> F a b
layoutHintF hint = putMessageFu (Low (LCmd (LayoutHint hint)))
