module ContDynF where

import Fudget
--import Xtypes
import Command
import FRequest
--import LayoutRequest(LayoutRequest)
--import Geometry
--import Message
import Spops(getSP,putSP,walkSP,pullSP)
import Path(here)
--import Direction
import Cont
--import Dynforkmerge
--import NullF(getMessageF,putMessageF)
--import LayoutDir

contDynF :: F a b -> Cont (F a d) b
contDynF (F sp) = fContWrap (contDynFSP sp)

contDynFSP :: FSP a b -> Cont (FSP a d) b
contDynFSP f c = cdf (pullSP f)
  where cdf (outf,f') = out f' outf 
        out f' [] = getSP $ \msg -> cdf (walkSP f' msg)
        out f' (x:xs) = case x of
	   High msg -> putSP (Low (here,XCmd DestroyWindow)) $ c msg
	   Low cmd -> putSP (Low cmd) $ out f' xs
