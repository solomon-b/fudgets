module SizingF(sizingF) where
import LoopLow(loopThroughLowF)
import Spops(mapAccumlSP)
import LayoutRequest
import FRequest
--import Event
--imaport Command
import Geometry(Rect(..))
--import Fudget
import Sizing(newSize)
--import Maptrace(ctrace) -- debugging

sizingF = loopThroughLowF . sizingSP

sizingSP sizing = mapAccumlSP sizingT state0
  where
    --tr = if sizing==StaticDebug then ctrace "dsizing" else const id
    state0 = Nothing

    sizingT Nothing msg =
      case msg of
	Left (path,LCmd (LayoutRequest (Layout size' fh' fv' wa' ha' rps' wanted'))) ->
	  (Just (Nothing,size',fh',fv'{-,rps'-}),msg)
	_ -> (Nothing,msg)
    sizingT s@(Just state@(optpos,size,fh,fv{-,rps-})) msg =
      case msg of
	Left (path,LCmd (LayoutRequest r@(Layout size' fh' fv' wa' ha' rps' wanted'))) ->
	    case (state' == state,optpos) of
	      (True,Just pos) -> (s,Right (path,LEvt (LayoutPlace (Rect pos size))))
	      _ -> --tr (show (state,state'))
	            (Just state',msg')
	  where size'' = newSize sizing size size'
	        state' = (optpos,size'',fh',fv'{-,rps'-})
	        msg' = Left (path,layoutRequestCmd r{minsize=size''})
	Right (_,LEvt (LayoutPlace rect@(Rect pos' size'))) -> (Just (Just pos',size',fh,fv{-,rps-}),msg)
	_ -> (s,msg)

{-
sizingF prevents a fudget from outputting a layout request that doesn't change
anything. The sizing parameter also restricts what kind of resizing is allowed.

sizingF assumes that the argument fudget has only ONE layout box and will
confuse things if layout requests are received from several different paths.

sizingF is used in autoLayoutF and there is probably no reason to use it in
other places (i.e., use autoLayoutF' if there seems to be a need for sizingF).
-}
