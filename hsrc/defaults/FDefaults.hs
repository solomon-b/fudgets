{-# LANGUAGE CPP #-}
module FDefaults(module FDefaults,module Alignment,fromMaybe) where

import Fudget
import CompOps
import Xtypes
import Alignment(Alignment(..))
--import Geometry(pmax)
import Data.Maybe(fromMaybe)
import Sizing(Sizing)
import GCAttrs --(ColorSpec,colorSpec)
#include "defaults.h"

type Customiser a = a ->  a
cust :: (a->a) -> Customiser a -- to obtain better type signatures
cust = id

type PF p a b = F (Either (Customiser p) a) b
type PK p a b = K (Either (Customiser p) a) b

getpar pp = fromMaybe (error "getpar:: missing default") . getparMaybe pp

getparMaybe pp [] = Nothing
getparMaybe pp (p:ps) =
     case pp p of
       Just a -> Just a
       Nothing -> getparMaybe pp ps

noPF :: PF p a b -> F a b
noPF f = f >=^< Right

standard :: Customiser a
standard = id

parameter_class(FontSpec,FontSpec)
setFont f = setFontSpec (fontSpec f)

--parameter_class(Title,String)
parameter_class(Keys,[(ModState,KeySym)])
parameter_class(WinAttr,[WindowAttributes])
parameter_class(BorderWidth,Int)

parameter_class(BgColorSpec,ColorSpec)
parameter_class(FgColorSpec,ColorSpec)
-- eta expanded because of the stupid monomorphism restriction
setBgColor c = setBgColorSpec . colorSpec $ c
setFgColor c = setFgColorSpec . colorSpec $ c
--getBgColor c = getBgColorSpec $ c
--getFgColor c = getFgColorSpec $ c

parameter_class(Margin,Int)
parameter_class(Align,Alignment)
parameter_class1(InitSize)
parameter_class1(InitDisp)
parameter_class(Stretchable,(Bool,Bool))
parameter_class(InitText,[String])
parameter_class(Sizing,Sizing)
