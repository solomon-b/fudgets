module Shells where

import FDefaults
import Fudget
import FRequest
import Xcommand
import DShellF
import EitherUtils
--import NullF
import CompOps
import Command
import FudgetIO
import Event
import Spops(concatMapSP)
import MapstateK

unmappedShellF cmds = unmappedShellF' standard cmds

unmappedShellF' pm cmds k =
    shellKF' (pm. setVisible False. setMargin 0)
	     (putLows cmds k)
   
unmappedSimpleShellF = unmappedSimpleShellF' standard

unmappedSimpleShellF' :: Customiser ShellF -> String -> F i o -> F i o
unmappedSimpleShellF' pm name f = 
   stripEither >^=< shellKF' params k f  >=^^< mapraiseSP where
     params = setVisible False . pm
     startcmds = [StoreName name]
     mapraiseSP = concatMapSP ( \msg -> [Right msg, Left True])
     k = xcommandsK startcmds mapWindowK

mapWindowK = mapstateK k1 False
  where
    k1 False  (High True)                  = (True,   [Low $ XCmd $ MapRaised])
    k1 True   (High False)               = (False,  [Low $ XCmd $ UnmapWindow])
    k1 _      (Low (XEvt (UnmapNotify _))) = (False,  [])
    k1 _      (Low (XEvt (MapNotify _)))   = (True,   [])
    k1 mapped _                            = (mapped, [])

-- Retained for backwards compatibility:
simpleShellF name wattrs = shellF' (setWinAttr wattrs) name
