module TryLayout(tryLayoutK) where

import FRequest
--import Message
import Cont
import LayoutRequest
--import Command
--import Fudget
--import Xtypes
--import Event

tryLayoutK lreq =
  cmdContK (layoutRequestCmd lreq) $ \r ->
  case r of
    LEvt (LayoutSize s) -> Just s
    _ -> Nothing
