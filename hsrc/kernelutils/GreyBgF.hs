module GreyBgF(changeBg, darkGreyBgK, lightGreyBgK, greyBgK, knobBgK, changeBackPixmap) where
import BgF(changeBackPixel)
import Color
import Command
import XDraw
--import Event(XEvent,BitmapReturn)
import Fudget
--import FudgetIO
import Xcommand
import Gc
import Geometry(Rect(..), lL, pP)
--import LayoutRequest(LayoutRequest)
--import Message(Message(..))
--import NullF
import Pixmap
import Cont(tryM)
import Xtypes
import GCAttrs(convColorK) -- + instances

--changeBackPixmap :: ColorName -> ColorName -> Size -> [DrawCommand] -> (K a b) -> K a b
changeBackPixmap fgcol bgcol size draw f =
    convColorK fgcol $ \fg ->
    convColorK bgcol $ \bg ->
    changeBackPixmapCol fg bg size draw f

changeBackPixmapCol fg bg size draw f =
    createPixmap size copyFromParent $ \pm ->
    wCreateGC rootGC [{-GCFunction GXcopy,-}
                      GCForeground fg, GCBackground bg,
		      GCGraphicsExposures False] $ \gc ->
    wCreateGC gc [GCForeground bg] $ \gcbg ->
    xcommandsK [DrawMany (Pixmap pm)
                  [(gcbg,[FillRectangle (Rect (pP 0 0) size)]),
		   (gc, draw)],
		ChangeWindowAttributes [CWBackPixmap pm],
		clearWindowExpose,
		FreePixmap pm]
    f

knobBgK cont =
    try2 "grey33" "black" $ \ (Color fg _) ->
    try2 "grey" "white" $ \ (Color bg  _) ->
    changeBackPixmapCol fg bg (pP 8 8)
                        [DrawLine (lL 0 0 2 2), DrawLine (lL 4 6 6 4)] cont

dithered50BgK fg bg =
  changeBackPixmapCol fg bg (pP 2 2) [DrawLine (lL 0 0 1 1)]

dithered25BgK fg bg =
  changeBackPixmapCol fg bg (pP 2 2) [DrawLine (lL 0 0 0 0)]

dithered75BgK fg bg = dithered25BgK bg fg

trySolidGreyBgK cname dithK cont =
  alloc3 cname $ \ (Color black b) (Color white w) (Color gray g) ->
  if g==b || g==w
  then dithK white black cont
  else xcommandsK [ChangeWindowAttributes [CWBackPixel gray],
                   clearWindowExpose] $
       cont

greyBgK = trySolidGreyBgK "grey" dithered50BgK
darkGreyBgK = trySolidGreyBgK "dark grey" dithered25BgK
lightGreyBgK = trySolidGreyBgK "light grey" dithered75BgK

changeBg :: ColorName -> (K a b) -> K a b
changeBg bg =
  case bg of
    "Nothing" -> id
    "grey" -> greyBgK
    _ -> changeBackPixel bg

alloc3 colorname cont =
    allocNamedColor defaultColormap "black" $ \black ->
    allocNamedColor defaultColormap "white" $ \white ->
    tryAllocNamedColor defaultColormap colorname $ \ ocolor ->
    let color = case ocolor of
	         Nothing -> black
		 Just c -> c in
    cont black white color
    
try2 cname1 cname2 cont =
  tryM (tryAllocNamedColor defaultColormap cname1)
       (allocNamedColor defaultColormap cname2 cont)
       cont
