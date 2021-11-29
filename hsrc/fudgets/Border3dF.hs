module Border3dF(border3dF) where
--import ButtonGroupF
import Color
import Command
import XDraw
import CompOps((>^=<))
import Defaults(bgColor, shadowColor, shineColor,new3d)
--import CmdLineEnv(argFlag)
import Dlayout(groupF)
import Event
import Fudget
--import FudgetIO
import FRequest
import Xcommand
import Gc
import Geometry(Point(..), origin, pP)
import GreyBgF(changeBg)
import LayoutRequest
--import Message(Message(..))
import NullF
import Spacer(marginF)
import EitherUtils(stripEither)
import Utils(swap)
import Xtypes
import GCtx(wCreateGCtx,GCtx(..))
import GCAttrs(gcFgA)

border3dF =
  if new3d
  then newBorder3dF
  else oldBorder3dF
 
newBorder3dF :: Bool -> Int -> F a b -> F (Either Bool a) b
newBorder3dF down edgew f =
    stripEither >^=< ((groupF startcmds kernel . marginF edgew) f)
  where
    startcmds =
      [XCmd $ ChangeWindowAttributes [CWEventMask [ExposureMask],
	                              CWBitGravity NorthWestGravity]]
				      -- bit gravity reduces flicker on resize

    wCreateGC gc0 gcas cont =
       wCreateGCtx (GC gc0 undefined) gcas $ \ (GC gc _) -> cont gc
    gcFg x = wCreateGC rootGC $ gcFgA x
    kernel =
      changeBg bgColor $
      gcFg shineColor $ \ whiteGC ->
      gcFg "black" $ \ blackGC ->
      gcFg [shadowColor,"black"] $ \ shadowGC ->
      let dRAW s pressed =
	    let lrc@(Point w h) = s-1
		ulc = 0; llc = pP 0 h; urc = pP w 0
		uli = 1; lli = llc+nw; lri = lrc-1; uri = urc-nw
		nw = pP 1 (-1)
		upper1 = [llc,ulc,urc]
		upper2 = [lli,uli,uri]
		lower1 = [llc,lrc,urc]
		lower2 = [lli,lri,uri]
	    in if pressed
	       then draw lower1 upper1 upper2
	       else draw upper1 lower1 lower2
	  drawlines gc ls = (gc,[DrawLines CoordModeOrigin ls])
	  draw wls bls dls =
	       [ClearWindow,
		DrawMany MyWindow [
		 drawlines shadowGC dls,
		 drawlines whiteGC wls,
		 drawlines blackGC bls]]
	  proc pressed size =
	      getK $ \bmsg ->
	      let same = proc pressed size
		  draw = dRAW size
		  redraw = draw pressed
	      in case bmsg of
		   Low (XEvt (Expose _ 0)) -> xcommandsK redraw same
		   Low (LEvt (LayoutSize newsize)) | newsize/=size ->
		     xcommandsK (dRAW newsize pressed) $ 
		     -- redraw needed here because of bit gravity
		     proc pressed newsize
		   High change | change/=pressed ->
		     xcommandsK (draw change) (proc change size)
		   _ -> same
	  proc0 pressed =
	      getK $ \msg ->
	      case msg of
		Low (LEvt (LayoutSize size)) -> proc pressed size
		High change -> proc0 change
		_ -> proc0 pressed
      in proc0 down

oldBorder3dF :: Bool -> Int -> F a b -> F (Either Bool a) b
oldBorder3dF down edgew f =
    stripEither >^=< ((groupF startcmds kernel . marginF edgew) f)
  where
    startcmds =
      [XCmd $ ChangeWindowAttributes [CWEventMask [ExposureMask],
	                              CWBitGravity NorthWestGravity]]
				      -- bitgravity reduces flicker on resize
    kernel =
      changeBg bgColor $
      allocNamedColorPixel defaultColormap shineColor $ \shine ->
      allocNamedColorPixel defaultColormap shadowColor $ \shadow ->
      wCreateGC rootGC [GCFunction GXcopy,
			GCForeground shadow] $ \shadowGC ->
      wCreateGC shadowGC [GCForeground shine] $ \shineGC ->
      let dRAW s pressed =
	    let lrc@(Point w h) = s
		e = edgew
		ulc = origin
		urc = pP w 0	-- pP (w - 1) 0
		llc = pP 0 h	-- pP 0 (h - 1)
		uli = pP e e
		lli = pP e (h - edgew)
		lri = pP (w - edgew) (h - edgew)
		uri = pP (w - edgew) e
		upper = [ulc, urc, uri, uli, lli, llc]
		lower = [llc, lrc, urc, uri, lri, lli]
		(upperGC, lowerGC) = (if pressed
				      then swap
				      else id) (shineGC, shadowGC)
	    in [ClearWindow,
		DrawMany MyWindow [
		  (lowerGC,[FillPolygon Nonconvex CoordModeOrigin lower]),
		  (upperGC,[FillPolygon Nonconvex CoordModeOrigin upper])]]
	  proc pressed size =
	      getK $ \bmsg ->
	      let same = proc pressed size
		  draw = dRAW size
		  redraw = draw pressed
	      in case bmsg of
		   Low (XEvt (Expose _ 0)) -> xcommandsK redraw same
		   Low (LEvt (LayoutSize newsize)) ->
		     if newsize==size
		     then same
		     else xcommandsK (dRAW newsize pressed) $ 
			  -- redraw needed here because of bit gravity
			  proc pressed newsize
		   High change -> xcommandsK (draw change) (proc change size)
		   _ -> same
	  proc0 pressed =
	      getK $ \msg ->
	      case msg of
		Low (LEvt (LayoutSize size)) -> proc pressed size
		High change -> proc0 change
		_ -> proc0 pressed
      in proc0 down
