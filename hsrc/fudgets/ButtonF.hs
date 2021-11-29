module ButtonF(oldButtonF) where
import CompOps((>^^=<),(>=^<))
--import Defaults(argKey, bgColor, fgColor)
import GraphicsF(graphicsDispF',replaceAllGfx,setGfxEventMask)
import FDefaults
import Sizing(Sizing(..))
import Drawing() -- instance, for hbc
import Graphic()
--import GCAttrs
--import GCtx(wCreateGCtx,rootGCtx)
import DrawingUtils
--import FDefaults
import PushButtonF
--import Placer(spacerF)
import Spacers(marginS,compS)
import Alignment(aCenter)
import CondLayout(alignFixedS')
import SpEither(filterRightSP)
--import Xtypes
--import NullF()
--import FudgetIO()

-- All this just because of the !@?! monomorphism restriction
--import Fudget(F)
--import GraphicsF(GfxEvent)
--import MeasuredGraphics(DPath)
--import DrawingUtils --(Gfx)

oldButtonF halign margin fname bg fg keys lbl =
    filterRightSP >^^=< pushButtonF keys lblF
 where
   --lblF :: Graphic lbl => F lbl (GfxEvent DPath) -- the monorestr
   lblF = let --lblD :: Graphic lbl => lbl -> Drawing a Gfx -- the monorestr
	      lblD = spacedD spacer . g
	      custom x = setInitDisp (lblD lbl) .
	               setGfxEventMask [] . setSizing Static .
		       setBorderWidth 0 . setBgColorSpec bg .
		       setFont fname . setFgColor fg $ x
              spacer = marginS margin `compS` alignFixedS' halign aCenter
	  in graphicsDispF' custom >=^< replaceAllGfx . lblD
