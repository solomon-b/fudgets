module ScrollF(scrollShellF,
               scrollF,oldScrollF,
               vScrollF,oldVscrollF,
	       hScrollF,oldHscrollF,
	       grabScrollKeys) where
import Fudget
import EitherUtils
import CmdLineEnv(argFlag)
import Utils(remove)
import LayoutRequest
import Geometry
import Command
import FRequest
import Event
import Xtypes

import FreeGroupF
import Dlayout(groupF)
import DShellF(shellF)
import Spops
import NullF(nullF)
import Cont(waitForSP)
import SpEither(mapFilterSP)
import DragF(hPotF',vPotF',PotRequest(..))
import Placer(tableF,hBoxF,vBoxF)
import SerCompF(absF)
import Loops(loopThroughRightF)
import CompOps
--import Maptrace(ctrace)

scrollShellF name initlimits = shellF name . oldScrollF True initlimits

grabScrollKeys = argFlag "grabscrollkeys" False
 -- True is not good if there are two or more scrollFs in the same shell window.

-- Std versions with arbirarily chosen limits...
scrollF = oldScrollF grabScrollKeys (pP 50 30,pP 550 700)
vScrollF = oldVscrollF grabScrollKeys (pP 50 30,pP 550 700)
hScrollF = oldHscrollF grabScrollKeys (pP 50 10,pP 550 700)

scroll foc = (const,plainAdjLayout,tableF 2,vPotF' foc Nothing >+< hPotF' foc Nothing)
vscroll foc = (const,wAdjLayout,hBoxF,vPotF' foc Nothing >+< nullF)
hscroll foc = (const,hAdjLayout,vBoxF,nullF>+<hPotF' foc Nothing)

oldScrollF grabKeys  = gScrollF (scroll  (not grabKeys)) grabKeys
oldVscrollF grabKeys = gScrollF (vscroll (not grabKeys)) grabKeys
oldHscrollF grabKeys = gScrollF (hscroll (not grabKeys)) grabKeys

gScrollF (outCoupling,inCoupling,placer,scrollbarsF) grabKeys initlimits fud =
    loopThroughRightF (placer mainF) (absF (initSP ctrlSP))
  where
    mainF =
        post>^=<
	(groupF start visibleK (freeGroupF fud)>+<scrollbarsF)
	>=^<pre
      where
        post = swapRight.mapEither assocLeft id -- (KV+(T+a))+S -> ((KV+T)+S)+a
	pre = mapEither assocRight id.swapRight -- converse

        start = map XCmd $
	        transinit++
	        [ChangeWindowAttributes [CWBackPixmap parentRelative],
		 ConfigureWindow [CWBorderWidth 1]]

	transinit =
	    if grabKeys
	    then [TranslateEvent tobutton [KeyPressMask]]
	    else []

      --tobutton k@(KeyEvent t p1 p2 s Pressed _ ks _) | (s, ks) `elem` keys =
	tobutton e@(KeyEvent {state=s,type'=Pressed,keySym=ks})
	           | (s, ks) `elem` keys =
	    Just e
        -- Mouse Wheel support:
	tobutton e@(ButtonEvent {button=Button 4,type'=Pressed}) = Just e
	tobutton e@(ButtonEvent {button=Button 5,type'=Pressed}) = Just e
	tobutton _ = Nothing

        keys = map ((,) []) keys' ++ map ((,) [Shift]) keys'
	  where keys' = ["Prior","Next","Home","End"]

    -- visibleK reports the current visible size and grabbed keys
    visibleK = K{-kk-} $ mapFilterSP visible
      where
	visible (Low (LEvt (LayoutSize vissize))) = Just (High (Right vissize))
	visible (Low (XEvt (KeyEvent{state=mods,type'=Pressed,keySym=key}))) =
	    Just (High (Left (mods,key)))
	visible (Low (XEvt (ButtonEvent{button=Button 4,type'=Pressed,state=mods}))) =
	    Just (High (Left (mods,"Prior")))
	visible (Low (XEvt (ButtonEvent{button=Button 5,type'=Pressed,state=mods}))) =
	    Just (High (Left (mods,"Next")))
	visible (High vissize) =
	    Just (Low (layoutRequestCmd (plainLayout vissize False False)))
	visible _ = Nothing

    initSP cont =
        waitForSP initreq $ \ req ->
	let vissize = limit initlimits rtotsize
            rtotsize = minsize req
	    adj = inCoupling req
	in putSP (Left (Left vissize)) $
	   cont vissize rtotsize adj
      where initreq (Left (Right (LayoutRequest req))) = Just req
            initreq _ = Nothing

    -- ctrlSP :: SP ((KV+T)+S) ((V+T)+S)
    -- cltrSP implements the scrolling
    -- Input : KV = grabbed keys or visible size (from visibleK)
    --       : T = total size (from freeGroupF)
    --       : S = scroll bar positions
    -- Output: V = requested visible size (to visibleK)
    --         S = scroll bar adjustments on size changes
    --         T = position adjustments on scroll bar changes,
    --             notification of current visible size
    ctrlSP visible total adj =
        concatMapAccumlSP ctrlT (visible, total, pP 0 0,adj) -- limits??
      where
        ctrlT s@(visible, total, pos, adj) msg =
	    case msg of
	      Left (Left (Left key)) -> (s,potKeyInput key)
	      Left (Left (Right visible')) -> adjustVisible visible' adj
	      Left (Right req) ->
	        case req of
		  LayoutRequest req -> adjustVisible visible adj'
		     where adj' = inCoupling req
		  LayoutMakeVisible rect align ->
		    (s, mkvisible rect align)
		  --LayoutScrollStep step -> ...
		  _ -> (s, [])
	      Right (Left (y,_,_)) -> vmove pos (-y)
	      Right (Right (x,_,_)) -> hmove pos (-x)
	  where
	    potKeyInput key@(mods,k) =
	      if Shift `elem` mods
	      then [Right (Right (PotInput (remove Shift mods,k)))]
	      else [Right (Left  (PotInput key))]
	    adjustVisible visible' adj' =
		((visible', total', pos, adj'),
		 Left (Right (Left total')):
		 adjustPots visible' total')
	      where total' = adj' visible'
	    adjustPots (Point visw vish) size@(Point totw toth) =
		    [Right (Right (ResizePot visw totw)),
		     Right (Left (ResizePot vish toth))]
	    mkvisible r@(Rect (Point x y) (Point w h)) (halign,valign) =
	      --ctrace "mkvisible" r $
		    [Right (Right (PotMkVisible x w halign)),
		     Right (Left (PotMkVisible y h valign))]
	    vmove pos@(Point x _) y =
	        --ctrace "vmove" (pos,pos') $
	        ((visible,total,pos',adj),[Left (Right (Right pos'))])
	      where pos' = Point x y
	    hmove pos@(Point _ y) x =
	        --ctrace "hmove" (pos,pos')
	        ((visible,total,pos',adj),[Left (Right (Right pos'))])
	      where pos' = Point x y


limit (min', max') size = pmax min' (pmin max' size)


type SizeCoupling = Size    -> Size      -> Size
--                  my size    other size   my new size

stdCoupling = pmax
vCoupling (Point tw th) (Point vw vh) = Point vw (max th vh)
hCoupling (Point tw th) (Point vw vh) = Point (max tw vw) vh

plainAdjLayout (Layout {minsize=total'}) = stdCoupling total'
wAdjLayout (Layout {wAdj=wa}) = s (flip vCoupling) (wa . xcoord)
hAdjLayout (Layout {hAdj=ha}) = s (flip hCoupling) (ha . ycoord)

s f g x = f x (g x)

-- assocLeft :: a+(b+c) -> (a+b)+c
-- assocRight :: (a+b)+c -> a+(b+c)
assocLeft = either (Left. Left) (either (Left. Right) Right)
assocRight = either (either Left (Right. Left)) (Right. Right)

--swapRight :: (a+b)+c -> (a+c)+b
swapRight = either (either (Left. Left) Right) (Left. Right)
