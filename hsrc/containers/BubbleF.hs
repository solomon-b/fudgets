module BubbleF(bubbleF, bubblePopupF, bubbleRootPopupF) where
--import Alignment(Alignment(..))
import Command
import XDraw
import CompOps((>=^<), (>^=<))
import Dlayout(groupF)
import Event
import Fudget(F)
--import FudgetIO
import FRequest
import NullF()
import Gc
import Geometry(origin,pP,Point(..),psub,padd)
import LayoutRequest(LayoutResponse(..))
import Message(Message(..))
import PopupGroupF
--import Popupmsg
import Spacer(sepF)
import ShapeK
import MapstateK
import EitherUtils(stripEither)
import Xtypes

default(Int) -- mostly for Hugs

bubblePopupF f =
    popupGroupF (bubbleOffset, wattrs, bubbleShapeK) (bubbleF f)

bubbleRootPopupF f =
    rootPopupF (bubbleOffset, rwattrs, bubbleShapeK) (bubbleF f)

bubbleF :: (F a b) -> F a b
bubbleF f =
  let startcmds = [XCmd $ ChangeWindowAttributes wattrs]
  in stripEither >^=<
     groupF startcmds bubbleShapeK (sepF sep f) >=^< Right

wattrs = [CWEventMask [ExposureMask]]
rwattrs = CWOverrideRedirect True:wattrs

bubbleShapeK =
  wCreateGC rootGC [GCLineWidth 2] $
  shapeK fillBubble . bubbleK

bubbleK gc =
  let bubbleT state@size msg =
	case msg of
	  Low (LEvt (LayoutSize size')) -> (size', [])
	  Low (XEvt (Expose _ 0)) ->
	    (state, if size == origin
	            then []
		    else [Low $ wDraw gc (drawBubble (size-1))])
	  _ -> (state, [])
  in mapstateK bubbleT origin

drawBubble size = DrawLines CoordModeOrigin (bubblePoints size)
fillBubble size = [FillPolygon Convex CoordModeOrigin (bubblePoints size)]

c = 4
ah = 12
ax = 12
aw = 6
atx = 6
bubbleBorder = ah + c
sep = pP (2 * c) (2 * c + bubbleBorder)
bubbleOffset (Point _ h) = Point atx (h - c)

bubblePoints size =
    let Point w h = psub size (pP 0 (2 * bubbleBorder))
    in  map (padd (pP 0 bubbleBorder))
            [pP c 0,
             pP (w - c) 0,
             pP w c,
             pP w (h - c),
             pP (w - c) h,
             pP (ax + aw) h,
             pP atx (h + ah),
             pP ax h,
             pP c h,
             pP 0 (h - c),
             pP 0 c,
             pP c 0]

--drawlines (p1 : p2 : ps) = DrawLine (Line p1 p2) : drawlines (p2 : ps)
--drawlines _ = []

