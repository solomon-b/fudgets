module PopupGroupF(popupGroupF,rootPopupF) where
import Command
import CompOps((>=^^<), (>^^=<))
--import Direction
import Dlayout(unmappedGroupF)
import Sizing(Sizing(..))
import Shells(unmappedShellF)
--import Event
import Fudget
import FRequest
import Geometry(psub,pP)
import LayoutRequest
import LoopLow
--import Message(Message(..))
import ParK
--import Path(Path(..))
import Popupmsg
--import Spops
import MapstateK
import SpEither(filterRightSP)
--import SerCompF(concatMapF)
import Spops
--import Xtypes

popupGroupF = popupF (unmappedGroupF Dynamic)
rootPopupF  = popupF unmappedShellF

popupF grF (offset, wattrs, k) f =
    let post (tag, cmd) =
            case cmd of
              LCmd req ->
	        case req of
--		  Layout size fh' fv -> [High (tag, LEvt $ LayoutPlace (Rect origin size))]
		  LayoutRequest (Layout {minsize=size}) ->
		    [High (tag, LEvt $ LayoutSize size)]
		    -- treated specially in windowKF.
		  _ -> []
              cmd' -> [Low (tag, cmd')]
        pre ev =
            case ev of
              High ev' -> [ev']
              Low (_, LEvt (LayoutPlace _)) -> [] -- shouldn't happen?
              Low tev -> [tev]
        distr (Popup p x) = [Right x, Left (Left (Just p))]
        distr Popdown = [Left (Left Nothing)]
        startcmds = [XCmd $ ChangeWindowAttributes wattrs]
    in  (filterRightSP >^^=<
         loopLow (concmapSP post)
                 (concmapSP pre)
                 (grF startcmds (compK (placeK offset) k) f)) >=^^<
        concatMapSP distr

placeK offset = mapstateK sizeT (False,pP 0 0)
  where
    sizeT s@(mapped,size) msg =
      case msg of
        High (Just p) ->
	  ((True,size),
	   [Low $ XCmd (moveWindow (psub p (offset size)))] ++
	   if not mapped then [Low $ XCmd MapRaised] else [])
	High Nothing ->
	  ((False,size),if mapped then [Low $ XCmd UnmapWindow] else [])
	Low (LEvt (LayoutSize size')) ->
	  ((mapped,size'), [])
	_ -> (s, [])

