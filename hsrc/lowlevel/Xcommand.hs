module Xcommand(
  xcommand,xcommandK,xcommandF,
  xcommands,xcommandsK,xcommandsF) where
import FRequest
import FudgetIO
import NullF(F,K)

xcommandK = xcommand :: (XCommand -> K i o -> K i o)
xcommandF = xcommand :: (XCommand -> F i o -> F i o)

xcommandsK = xcommands :: ([XCommand] -> K i o -> K i o)
xcommandsF = xcommands :: ([XCommand] -> F i o -> F i o)

xcommand xcmd = putLow (XCmd xcmd)
xcommands xcmds = putLows (map XCmd xcmds)
