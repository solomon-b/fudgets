module OnOffDispF where
import CompOps((>=^<),(>^^=<))
import NullF(F)
import Spops(nullSP)
--import Xtypes
import ResourceIds() -- synonym ColorName, for hbc
import Geometry(Point(..))
import MeasuredGraphics(emptyMG)
import GraphicsF(graphicsDispF',GfxCommand(..),setGfxEventMask)
import FDefaults
import GCAttrs
--import Drawing( ) -- instances
import Graphic( )
import Defaults(bgColor, fgColor)
import CmdLineEnv(argKey)
import Sizing(Sizing(..))

onOffDispF :: Bool -> F Bool nothing
onOffDispF start = nullSP >^^=< graphicsDispF' custom >=^< pre
  where
    custom = setInitDisp (emptyMG dsize) . setGfxEventMask [] .
	     setSizing Static . setBorderWidth 0 .
	     setBgColor (color start)
    color on = if on then onColor else offColor
    dsize = Point 7 7
    --pre :: Bool -> GfxCommand Bool -- resolving overloading...
    pre = ChangeGfxBg . colorSpec . color

offColor = argKey "toggleoff" bgColor
onColor  = argKey "toggleon" fgColor
