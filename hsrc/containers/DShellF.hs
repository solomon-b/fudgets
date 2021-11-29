{-# LANGUAGE CPP #-}
module DShellF(ShellF,shellF, shellF', shellKF, shellKF',
       setDeleteWindowAction,
       getDeleteWindowActionMaybe', -- for use in titleShellF
       DeleteWindowAction(..),setDeleteQuit,
       HasClickToType(..),setInitPos,setFocusMgr,
       HasVisible(..)) where

import FDefaults
import Dlayout(sF)
import AutoLayout(autoLayoutF',nowait)
import QuitK
import Fudget
import EitherUtils
import CompOps
import Geometry(Point(..))
import Command
import Xcommand
import Xtypes
import Defaults(defaultSep,defaultPosition)
import CmdLineEnv(argFlag)
import FocusMgr(focusMgr)
--import Placer
--import Spacers
import Spacer(marginF)
--import LayoutRequest
import Sizing(Sizing(..))
import NullF
import ParK
--import Maptrace(ctrace) -- debugging
#include "defaults.h"

newtype ShellF = Pars [Pars]
data Pars
  = WinAttr [WindowAttributes]
  | DeleteWindowAction (Maybe DeleteWindowAction)
  | ClickToType Bool
  | FocusMgr Bool -- mainly for internal use
  | Visible Bool
  | Margin Int
  | Sizing Sizing
  | InitPos (Maybe Point)

data DeleteWindowAction = DeleteQuit | DeleteUnmap deriving (Eq,Show)

parameter(InitPos)
parameter(FocusMgr)

parameter_instance(WinAttr,ShellF)

parameter(DeleteWindowAction)
getDeleteWindowActionMaybe' pm =
  getDeleteWindowActionMaybe (pm (Pars []))

-- Backwards compatibility:
setDeleteQuit b = setDeleteWindowAction (if b then Just DeleteQuit else Nothing)

parameter_class(ClickToType,Bool)
parameter_instance(ClickToType,ShellF)


parameter_class(Visible,Bool)
parameter_instance(Visible,ShellF)

parameter_instance(Margin,ShellF)
parameter_instance(Sizing,ShellF)

shellF = shellF' standard
shellF' pmod s f = stripEither >^=< shellKF' pmod k f >=^< Right where
	k = xcommandK (StoreName s) nullK

shellKF = shellKF' standard

shellKF' :: (Customiser ShellF)->K a b -> F c d -> F (Either a c) (Either b d)
shellKF' pmod k f = genShellF siz clicktt focusmgr vis pos sep [] kernel f
 where
   ps = pmod (Pars [WinAttr [],DeleteWindowAction (Just DeleteQuit),
                    ClickToType ctt, FocusMgr defFocusMgr,
                    InitPos defaultPosition, -- hmm
		    Visible True,Margin defaultSep,Sizing Dynamic])
						-- !! Change default Sizing?
   d_action = getDeleteWindowAction ps
   clicktt = getClickToType ps
   focusmgr = getFocusMgr ps
   sep = getMargin ps
   vis = getVisible ps
   wa = getWinAttr ps
   siz = getSizing ps
   pos = getInitPos ps
   kernel = xcommandK (ChangeWindowAttributes wa) $
	    maybe (wmDeleteWindowK (const k)) -- see (*) in QuitK.hs
		  (\ a -> quitK (action a) `parK` k)
		  d_action

   action DeleteQuit = exitK
   action DeleteUnmap = unmapWindowK

genShellF sizing ctt focusmgr map pos sep cmds k f = 
       sF (not map) pos cmds k (filter (sepf f)) where
   filter = if focusmgr
            then focusMgr sizing ctt
            else autoLayoutF' nowait sizing -- usually sits inside a groupF in focusMgr
   sepf = if sep == 0 then id else marginF sep

ctt = argFlag "ctt" True
defFocusMgr = argFlag "focusmgr" True
