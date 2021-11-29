module DragF(containerGroupF,hPotF,hPotF',vPotF,vPotF',PotRequest(..),PotState(..)) where
import Command
import CompOps((>=^<), (>^=<))
import CompFfun(prepostMapHigh)
import Cursor
import Dlayout(groupF,groupF')
import Sizing(Sizing(..))
import Event
--import Color
import Fudget
--import FudgetIO
import FRequest
import NullF
import Geometry
import GreyBgF
import Border3dF(border3dF)
import LayoutRequest(LayoutResponse(..))
import LayoutF(holeF')
import Spacers(hvMarginS)
import DynSpacerF(dynSpacerF)
import Spacer(noStretchF)
import Alignment
import Loops(loopCompThroughRightF,loopLeftF)
--import Message(Message(..))
import EitherUtils(stripEither)
import Data.Maybe(fromMaybe)
import CmdLineEnv(argReadKey)
import Xtypes
--import Maptrace(ctrace)

data PotRequest = ResizePot Int Int -- frame size, total size
                | MovePot   Int     -- new position
		| PotMkVisible Int Int (Maybe Alignment) -- pos, length, alignm
		| PotInput (ModState, KeySym) -- remote control

type PotState = (Int,Int,Int)  -- position, frame size, total size

knobS knob@(Rect kp ks) box@(Rect bp bs) =
  --ctrace "knobS" (knob,box) $
  hvMarginS (kp+margin) (bs+margin-kp-ks)

knobK box knob pabs =
    let cont knob' pabs' = knobK box knob' pabs'
        newbox box' knob' = knobK box' knob' pabs
        same = cont knob pabs
	output knob = putK (High (Right knob))
	repos knob box = putK (High (Left (knobS knob box)))
    in  getK $ \msg ->
        case msg of
          Low (XEvt (MotionNotify {rootPos=pabs',state=mods})) ->
	    let knob' = moverect knob (psub pabs' pabs)
	        knob'' = confine box knob'
		pabs'' = padd pabs' (rsub knob'' knob')
            in  repos knob'' box $
                (if Shift `elem` mods then id else output knob'') $
	        cont knob'' pabs''
          Low (XEvt (ButtonEvent {rootPos=pabs',type'=Pressed})) -> cont knob pabs'
          Low (XEvt (ButtonEvent {type'=Released})) -> output knob $ same
          High (newknob, box') ->
	    let knob' = confine box' newknob
                msgs = (if knob'/=knob || box'/=box
                        then repos knob' box'
			else id) .
		       (if knob'/=knob -- was: knob/=newknob
		        then output knob'
			else id)
            in msgs $
               newbox box' knob'
          _ -> same

-- New version, using new more general containterGroupF
-- containterGroupF should be replaced.
containerGroupF knob box cursorshape buttons modifiers fudget =
    loopLeftF $
    prepostMapHigh pre post $
    dynSpacerF $
    groupF initcmds
           (setFontCursor cursorshape (knobK box knob origin))
           fudget
  where attrs = [CWEventMask []]
        initcmds = map XCmd 
	           [ChangeWindowAttributes attrs,
		    GrabButton False buttons modifiers
                        [PointerMotionMask, ButtonReleaseMask]]
		    --MapRaised]
        pre = id
	post = either (either Left (Right. Left)) (Right. Right)

knobF cursor box knob =
    (stripEither >^=< containerGroupF knob box cursor (Button 1) [] vF ) >=^<
    Left
  where vF = raisedF (holeF' s)
	s = rectsize knob

staticBorder3dF down fud = border3dF down 2 fud >=^< Right
raisedF = staticBorder3dF False
loweredF = staticBorder3dF True

--topleft = diag 2
--margin = diag 6
topleft = diag 0
margin = diag (argReadKey "potmargin" 0)

absknobpos size (pos, frame, tot) =
    if tot == 0
    then (0, max 1 size)
    else ((pos * size + tot `div` 2) `quot` tot, max 1 (frame * size `quot` tot))

newkpos :: PotState -> (Int,Int) -> PotState
newkpos (_, frame, tot) (pos, size) = (pos * tot `quot` size, frame, tot)

knobup d (pos, frame, tot) = (0 `max` (pos - d), frame, tot)
knobdown d (pos, frame, tot) = ((tot - frame) `min` (pos + d), frame, tot)
pageup knob@(_, frame, _) = knobup frame knob
pagedown knob@(_, frame, _) = knobdown frame knob

stepup len knob = knobup (stepsize len knob) knob
stepdown len knob = knobdown (stepsize len knob) knob
stepsize size (_,_,tot) = (tot+size-1) `quot` size

knobhome (_, frame, tot) = (0, frame, tot)
knobend (_, frame, tot) = (tot - frame, frame, tot)

resizePot (pos,_,_) frame tot = (pos,frame,tot) -- !! should adjust pos if necessary
movePot (_,frame,tot) pos = (pos,frame,tot) -- !! should keep pos within boundaries

--and adjkpos (pos,frame,tot) frame' tot' = (min (tot'-frame') (pos*tot'/tot),frame',tot')

keyAction mods s len =
    case s of
      s | s == "space"  || s == "Next"  -> Just (shift knobend pagedown)
      s | s `elem` pageupKeys           -> Just (shift knobhome pageup)
      s | s == "Home"                   -> Just knobhome
      s | s == "End"                    -> Just knobend
      s | s == "Down"   || s == "Right" -> Just (stepdown len)
      s | s == "Up"     || s == "Left"  -> Just (stepup len)
      _ -> Nothing
  where
    shift = if Shift `elem` mods then const else const id
    pageupKeys = ["Delete","BackSpace","Prior"]

mkVisible (pos,frame,tot) first last optAlign =
  case optAlign of
    Just a -> Just (max 0 (min (tot-frame) pos'),frame,tot)
      where pos' = first+truncate (a*fromIntegral (last-first-frame))
    _ ->
      if first<pos || last-first>frame
      then Just (first,frame,tot)
      else if last>pos+frame
	   then Just (last-frame,frame,tot)
	   else Nothing

potF hori par ort vect shape grav acceptFocus optsize =
    loopCompThroughRightF potGroupF
  where
    potGroupF =
	noStretchF (not hori) hori $
	loweredF $ 
	groupF' Static startcmds 
	       (darkGreyBgK potK0)
	       (knobF shape box0 (knob length0 kpos0))
      where wattrs = [CWEventMask eventmask]
	    alwayseventmask = [ButtonPressMask,Button2MotionMask]
	    focuseventmask = [EnterWindowMask, LeaveWindowMask, KeyPressMask]
	    eventmask = alwayseventmask ++ 
	                if acceptFocus then focuseventmask else []
	    startcmds = [--XCmd $ LayoutMsg (Layout wsize (not hori) hori),
			 XCmd $ ChangeWindowAttributes wattrs]
    wsize    = fromMaybe (vect 50 11) optsize
    boxsize0 = psub wsize margin
    length0  = par boxsize0
    boxwidth = ort boxsize0
    knob length' kpos' =
	let (pos, size) = absknobpos length' kpos'
	in  Rect (padd topleft (vect pos 0)) (vect size boxwidth)
    box0 = Rect topleft boxsize0
    potK0 = potK1 --allocNamedColorPixel defaultColormap "white" potK1
    potK1 = potK kpos0 length0 where
      potK kpos len =
	  let cont kpos' = potK kpos' len
	      newlen kpos' len' = potK kpos' len'
	      same = cont kpos
	      report kpos' = --ctrace "report" kpos' $
	                     High (Right kpos')
	      changeknob len' kpos' =
		  High (Left (knob len' kpos',
			     Rect topleft (vect len' boxwidth)))
	      moveknob kpos' = --ctrace "moveknob" kpos' $
			       putsK [changeknob len kpos'{-,report kpos'-}] $
			       -- Rely on the knob to report the new position,
			       -- after it has been confined to the box. This
			       -- is a fix for button2Action that result in
			       -- positions outside the box...
			       same -- cont kpos'
	      keyInput mods key = maybe same act (keyAction mods key len)
		where act action = moveknob (action kpos)

	      button2Action p = newkpos kpos (par p, len)

	      buttonAction (Button 2) mods p = button2Action p
	      buttonAction b mods p =
		case (par p < par (rectpos (knob len kpos)),
		      Shift `elem` mods || Control `elem` mods) of
	          (True, False) -> pageup   kpos
	    	  (True, True ) -> knobhome kpos
		  (False,False) -> pagedown kpos
		  (False,True ) -> knobend  kpos

	  in getK $ \msg ->
	     case msg of
	       Low (XEvt (ButtonEvent {button=b,pos=p,state=mods,type'=Pressed})) ->
	         moveknob (buttonAction b mods p)
	       Low (XEvt (MotionNotify {pos=p,state=mods})) | Button2 `elem` mods ->
	         moveknob (button2Action p)
	       Low (LEvt (LayoutSize size')) ->
		 let len' = par size' - par margin
		 in if len'/=len
	            then putsK [changeknob len' kpos] (newlen kpos len')
		    else same
	       Low (XEvt (KeyEvent _ _ _ mods Pressed _ key _)) ->
	          keyInput mods key
	       Low (XEvt (FocusIn {detail=d})) | d /= NotifyInferior -> 
		  lightGreyBgK same
	       Low (XEvt (FocusOut {detail=d})) | d /= NotifyInferior -> 
		  darkGreyBgK same
	       High (Right (PotInput (mods,key))) -> keyInput mods key
	       High (Right (ResizePot frame tot)) ->
		 let kpos' = resizePot kpos frame tot
		 in putK (changeknob len kpos') (cont kpos')
	       High (Right (MovePot pos)) ->
		 let kpos' = movePot kpos pos
		 in putK (changeknob len kpos') (cont kpos')
	       High (Right (PotMkVisible pos size optAlign)) ->
		 case mkVisible kpos pos (pos+size) optAlign of
		   Just kpos' -> moveknob kpos'
		   Nothing    -> same
	       High (Left newknob) ->
                 --ctrace "newknob" newknob $	       
		 let kpos' = newkpos kpos (par (rsub newknob box0), len)
		 in  if kpos'==kpos -- False
		     then same
		     else putK (report kpos') (cont kpos')
	       _ -> same
    kpos0 = (0,1,1)

vPotF' = potF False ycoord xcoord (\x -> \y -> Point y x) 116 NorthEastGravity
hPotF' = potF True xcoord ycoord Point 108 SouthWestGravity

vPotF = vPotF' True Nothing
hPotF = hPotF' True Nothing
