module DynSpacerF(dynSpacerF,dynPlacerF) where
import Fudget
import LayoutRequest
import SerCompF(idLeftF)
import CompSP(postMapSP)
import Path(here)
import Message
--import Command
import FRequest

dynSpacerF = dynLayoutMsgF LayoutReplaceSpacer
dynPlacerF = dynLayoutMsgF LayoutReplacePlacer

dynLayoutMsgF lmsg fud = F (post `postMapSP` fudsp)
  where
    F fudsp = idLeftF fud
    post = message Low (either sendLmsg High)
    sendLmsg x = Low (here,LCmd (lmsg x))
