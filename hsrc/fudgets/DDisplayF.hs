{-# LANGUAGE CPP #-}
module DDisplayF(--HasInitDisp(..),
		 setSpacer,
		 DisplayF,
                 displayF,displayF',--displayF'',
                 intDispF,intDispF',--intDispF'',
                 labelF,labelF' --,labelF''
		) where
import FDefaults
import GraphicsF(graphicsDispF',replaceAllGfx,setGfxEventMask)--GfxCommand
import Graphic
import Drawing() -- instances
import DrawingUtils(spacedD,g)--vboxD,blankD,hardAttribD,
import GCAttrs --(ColorSpec,colorSpec) -- + instances
--import GCtx(GCtx(..),wCreateGCtx,rootGCtx)
--import EitherUtils(mapEither)
--import FudgetIO
--import Fudget
import NullF(F)
--import Xtypes
import ResourceIds() -- synonym ColorName, for hbc
import Defaults(defaultFont,labelFont,paperColor,fgColor,bgColor)
import CmdLineEnv(argKeyList)
import CompOps((>=^^<),(>=^<),(>^^=<))
import CompSP(idLeftSP)
import Spops(nullSP)
import SpEither(filterRightSP)
import Alignment(aRight,aLeft,aCenter)
--import AlignF(noStretchF)
--import LoadFont(safeLoadQueryFont)
--import Font(string_box_size)
import Spacers(marginS,compS,hAlignS)--minSizeS,noStretchS,
import LayoutRequest(Spacer)
import Sizing(Sizing(..))
import CondLayout(alignFixedS')
--import Maybe(fromMaybe)

#include "defaults.h"

newtype DisplayF a = Pars [Pars a]

data Pars a
  = BorderWidth Int
  | FgColorSpec ColorSpec
  | BgColorSpec ColorSpec
  | FontSpec FontSpec
--  | Align Alignment
  | Spacer Spacer
  | Margin Int
  | InitDisp a
  | InitSize a
  | Sizing Sizing
  | Stretchable (Bool,Bool)
-- Don't forget to adjust instance Functor DisplayF above if you add stuff here!

type StringDisplayF = DisplayF String

parameter_instance1(BorderWidth,DisplayF)
parameter_instance1(FgColorSpec,DisplayF)
parameter_instance1(BgColorSpec,DisplayF)
parameter_instance1(FontSpec,DisplayF)
--parameter_instance1(Align,DisplayF)
parameter_instance1(Margin,DisplayF)

parameter_instance(InitDisp,DisplayF)
parameter(Spacer)
parameter_instance(InitSize,DisplayF)
parameter_instance1(Stretchable,DisplayF)
parameter_instance1(Sizing,DisplayF)

-- For backwards compatibility:
instance HasAlign (DisplayF a) where
  setAlign align (Pars ps) = Pars (Spacer (alignFixedS' align aCenter):ps)

labelDisplayF :: Graphic g => F g void -- because of monomorphism restriction
labelDisplayF = labelDisplayF' standard
labelDisplayF' pm = noPF $ labelDisplayF'' pm

labelDisplayF''
  :: Graphic g => Customiser (DisplayF g) -> PF (DisplayF g) g void
labelDisplayF'' pmod = 
    nullSP >^^=<
    graphicsDispF' custom >=^<
    pre >=^^<
    filterRightSP
  where
    custom =
     	maybe id (setInitDisp . draw) initDisp .
	maybe id (setInitSize . draw) initSize .
	setGfxEventMask [] . setSizing sizing .
	setBorderWidth borderWidth . setBgColorSpec bgColor .
	setFont font . setFgColorSpec fgColor .
	setStretchable stretch
    pre = replaceAllGfx . draw 
    draw = marginD . g
    marginD = spacedD (marginS margin `compS` spacer)

    margin = getMargin ps
    borderWidth = getBorderWidth ps
    bgColor = getBgColorSpec ps
    fgColor = getFgColorSpec ps
    font = getFontSpec ps
    spacer = getSpacer ps
    stretch = getStretchable ps
    sizing = getSizing ps
    initSize = getInitSizeMaybe ps
    initDisp = getInitDispMaybe ps
    ps = pmod (Pars [Margin 4,BorderWidth 0,
                     FgColorSpec dispfg, BgColorSpec dispbg,
		     FontSpec (fontSpec defaultFont),
		     Spacer (alignFixedS' aLeft aCenter),
		     Stretchable (False,False),
		     Sizing Dynamic{-,InitSize "",InitDisp ""-}])

displayF :: Graphic g => F g void -- because of monomorphism restriction
displayF = displayF' standard
displayF' custom = noPF $ displayF'' custom

displayF'' :: Graphic g => Customiser (DisplayF g) -> PF (DisplayF g) g void
displayF'' pmod = labelDisplayF'' pmod'
  where
    pmod' = pmod .
            --setInitSize "XXXXX" .
	    setBorderWidth 1 .
	    setStretchable (True,False) .
	    setSpacer (hAlignS aLeft) .
	    setSizing Growing


labelF lbl = labelF' standard lbl
labelF' pm = noPF . labelF'' pm

labelF'' :: Graphic g => Customiser (DisplayF g) -> g -> PF (DisplayF g) a b
labelF'' pmod lbl = labelDisplayF'' pmod' >=^^< idLeftSP nullSP
  where
   pmod' = pmod.
	   setInitDisp lbl.
           (setFgColor lblfg::(Customiser (DisplayF g))).
	   setBgColor lblbg.
	   setFont labelFont.
	   setMargin 0 .
	   setSizing Static

-- cu works around a deficiency in the type inference algorithm.
-- cu x y = id x y :: (Customiser (DisplayF a))

intDispF = intDispF' standard
intDispF' = noPF . intDispF''
intDispF'' :: Customiser (DisplayF Int) -> PF (DisplayF Int) Int a
intDispF'' pm = displayF'' (pm' 0) -- >=^< mapEither id show
  where pm' x = pm.setAlign aRight
                  .setInitDisp x
		  .setInitSize ((-maxBound) `asTypeOf` x)
--	 	  .forgetInitDisp -- should be built into setInitDisp
                  .(setSizing Static::(Customiser (DisplayF Int)))
		  .setStretchable (False,False)

dispbg = colorSpec (argKeyList "dispbg" [paperColor,"white"])
dispfg = colorSpec (argKeyList "dispfg" [fgColor,"black"])

lblbg = colorSpec (argKeyList "lblbg" [bgColor,"white"])
lblfg = colorSpec (argKeyList "lblfg" [fgColor,"black"])

{-
forgetInitDisp :: DisplayF a -> DisplayF b
forgetInitDisp (Pars ps) = Pars (forget ps)
  where
    forget ps =
      case ps of
	[] -> []
	BorderWidth i:ps -> BorderWidth i:forget ps
	FgColor c:ps -> FgColor c:forget ps
	BgColor c:ps -> BgColor c:forget ps
	Font f:ps -> Font f:forget ps
	Align a:ps -> Align a:forget ps
	Margin i:ps -> Margin i:forget ps
	InitDisp x:ps -> forget ps
	InitSize s:ps -> InitSize s:forget ps
	Sizing s:ps -> Sizing s:forget ps
	Stretchable bb:ps -> Stretchable bb:forget ps
    
-}

{-
instance Functor DisplayF where
  map f (Pars ps) = Pars (map (mapPars f) ps)
    where
      mapPars f p =
	case p of
	  BorderWidth i -> BorderWidth i
	  FgColor c -> FgColor c
	  BgColor c -> BgColor c
	  Font f -> Font f
	  Align a -> Align a
	  Margin i -> Margin i
	  InitDisp x -> InitDisp (f x)
	  InitSize s -> InitSize s
	  Sizing s -> Sizing s
	  Stretchable bb -> Stretchable bb
-}
