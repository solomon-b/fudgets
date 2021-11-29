-- If you get a type error when compiling this with HBC, try removing -O.
module GcWarningF where
import Dlayout(windowF)
import Command(XCommand(ClearWindow,ChangeWindowAttributes,SetGCWarningHack))
import DrawInPixmap
import LayoutRequest
import Geometry(Rect(..))
import Xtypes
import FudgetIO
import FRequest(layoutRequestCmd)
import Xcommand
import NullF(nullK)
--import ResourceIds
import Pixmap(createPixmap)
import GCtx(GCtx(..),pmCreateGCtx,rootGCtx)
--import GCAttrs
import Defaults(bgColor)

-- Garbage Collection Warning Fudget

gcWarningF = windowF startcmds warnK
  where
    startcmds = [layoutRequestCmd (plainLayout size True True)]

    warnK =
	createPixmap size copyFromParent $ \ gcon ->
	createPixmap size copyFromParent $ \ gcoff ->
	fg gcon [bgColor,"white"] $ \ (GC bg _) ->
	fg gcon ["red","black"] $ \ (GC red _) ->
	putLow (pmFillRectangle gcon bg r) $
	putLow (pmFillRectangle gcoff bg r) $
	putLow (pmFillArc gcon red r 0 (360*64)) $
	xcommandK (SetGCWarningHack gcon gcoff) $
	xcommandK (ChangeWindowAttributes [CWBackPixmap gcoff]) $
	xcommandK ClearWindow $
	nullK
      where
	r = Rect 0 size
	fg pm colspec =
	  pmCreateGCtx pm rootGCtx
	    ([GCForeground colspec]::[GCAttributes [String] String])

    size = 10
