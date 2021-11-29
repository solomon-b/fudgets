module XDraw(module DrawTypes,module XDraw) where
import Command(XCommand(Draw,DrawMany,ClearArea,ClearWindow))
import FRequest
import Geometry(Rect(..),Point(..))
import DrawTypes

draw d gc dcmd = XCmd (Draw d gc dcmd)
drawMany d dcmds = XCmd (DrawMany d dcmds)

wDraw = draw MyWindow
wDrawMany = drawMany MyWindow

pmDraw = draw . Pixmap 
pmDrawMany = drawMany . Pixmap

clearArea r b = XCmd (ClearArea r b)
clearWindow = XCmd ClearWindow

fillCircle p r = FillArc (Rect p (Point r r)) 0 (64 * 360)
drawCircle p r = DrawArc (Rect p (Point r r)) 0 (64 * 360)
