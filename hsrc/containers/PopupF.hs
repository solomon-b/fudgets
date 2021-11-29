module PopupF(popupShellF,popupShellF') where
import Command
import DShellF
import FDefaults
import Fudget
import FRequest
import Xcommand
import Geometry(Point(..), pP)
--import LayoutRequest(LayoutRequest)
import Loops(loopCompThroughRightF)
--import Message(Message(..))
--import Spops
import MapstateK
import Xtypes
--import NullF(putsK)
import CompSP
import Path(here)

popupShellF = popupShellF' standard

popupShellF' :: Customiser ShellF -> String -> Maybe Point -> F a b -> F a (a,b)
popupShellF' pm title optpos (F f) =
  let pos = case optpos of
              Just pos -> pos
              Nothing -> pP 300 300
      params = pm . setVisible False . setDeleteQuit False
  in loopCompThroughRightF (shellKF' params (popupK title pos) 
			   (F{-ff-} $ prepostMapSP pre post (idRightSP f)))

pre (Low m) = Left (Low m)
pre (High (Left a)) = Left (High a)
pre (High (Right a)) = Right a
post (Right a) = Low (here,a)
post (Left (Low m)) = Low m
post (Left (High a)) = High a

popupK title pos =
  let kf s@(mapped,trig) msg =
	  case msg of
	    High (Right trig') -> ((True,trig'), 
	      [High (Left (Left trig')),lowfromf (GrabEvents True)] ++
	       if not mapped then [Low $ XCmd MapRaised] else [])
	    High (Left x) -> ((False,trig),
			      (if mapped then unmapcmds else []) ++ 
			      [lowfromf UngrabEvents,High (Right (trig, x))])
	    Low _ -> (s, [])
      lowfromf = High . Left . Right . XCmd
      unmapcmds = [Low $ XCmd UnmapWindow,Low $ XCmd Flush]
      startcmds =
	  [StoreName title, SetNormalHints pos, moveWindow pos,
	   ChangeWindowAttributes [CWSaveUnder True]]
  in xcommandsK startcmds $
     mapstateK kf (False,error "premature output from fudget inside popupShellF")
