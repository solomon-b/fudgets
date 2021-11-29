module PosPopupShellF(posPopupShellF) where
import Command
import Shells(unmappedShellF)
--import Event(Event(..))
import Fudget
import FRequest
import Geometry(origin, pP, psub)
--import LayoutRequest(LayoutRequest)
import Loops(loopCompThroughRightF)
--import Message(Message(..))
import NullF
--import Path(Path(..))
import QueryPointer
--import SP
--import Xtypes

posPopupShellF title wattrs f =
    loopCompThroughRightF (unmappedShellF startcmds popupK f)
  where
    startcmds =
        [XCmd $ StoreName title,
	 XCmd $ SetNormalHints origin,
	 XCmd $ ChangeWindowAttributes wattrs]

popupK = kf (error "premature output from fudget inside posPopupShellF")
  where
    pickPos p cont =
      case p of
        Just pos -> cont pos
	Nothing -> queryPointerK (\(_, r, _, _) -> cont (psub r (pP 5 5)))

    kf s@(mapped,trig) =
        getK $ \msg ->
        case msg of
          High (Right (trig', optpos)) ->
	    pickPos optpos $ \pos ->
            putsK ([Low $ XCmd $ moveWindow pos,
                    High (Left trig')] ++
                   if not mapped then [Low $ XCmd MapRaised] else []) $
            kf (True,trig')
          High (Left y) ->
	    putsK ((if mapped then [Low $ XCmd UnmapWindow] else []) ++
		   [High (Right (trig, y))]) $
            kf (False,trig)
          _ -> kf s
