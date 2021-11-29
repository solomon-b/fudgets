{-# LANGUAGE CPP #-}
module Placers(linearP, verticalP, horizontalP, horizontalP', verticalP') where
import Geometry
import LayoutDir
import LayoutRequest
import Spacers(Distance(..))
import Data.List(mapAccumL)
import Utils(part)
import Defaults(defaultSep)
import Maptrace(ctrace) -- debugging
--import NonStdTrace(trace)
import IntMemo

#ifndef __HBC__
#define fromInt fromIntegral
#endif
 
horizontalP = horizontalP' defaultSep
verticalP = verticalP' defaultSep

horizontalP' = linearP Horizontal
verticalP' = linearP Vertical

linearP :: LayoutDir -> Distance -> Placer
linearP ld sep = P $ linearP' ld sep

linearP' ld sep [] = ctrace "linearP" ("linearP "++show ld++" []") $
		    (plainLayout 1 (ld==Horizontal) (ld==Vertical),\ r -> [])
linearP' ld sep requests =
    let minsizes = map minsize requests
        totis = sep * (max 0 (length requests - 1))
        h = max 0 (h'-sep)   -- totis + sum (map (xc ld) minsizes)
	(h',rpss) = mapAccumL adjust 0 requests
	  where adjust x (Layout {minsize=rsz,refpoints=rps}) =
		    (x+rw+sep,map adj1 rps)
		  where adj1 p = mkp ld (x+xc ld p) (yc ld p)
		        rw = xc ld rsz
        v = (maximum . (0:) . map (yc ld)) minsizes
        line2 gotr =
            let goth = (fromInt . xc ld . rectsize) gotr - fromInt totis
                gotv = (yc ld . rectsize) gotr
                startx = (fromInt . xc ld . rectpos) gotr
                starty = (yc ld . rectpos) gotr
#if 0
-- Old solution
		requests' = requests
#else
-- New, experimental solution:
		requests' = map req' requests
		  where
		    req' req = req {minsize=adj gotv}
		      where adj=orthogonal ld (wAdj req) (hAdj req)
#endif
                (fih, flh) = part (fixh ld) requests'
                fixedh' =
		  (fromInt . sum . map (xc ld . minsize)) fih
                floath = (fromInt . sum . map (xc ld . minsize)) flh
                fixedR = if floath > 0.0 then 1.0 else goth / fixedh'
                floatR =
                    if floath == 0.0 then 1.0 else (goth - fixedh') / floath
                rR' req = if fixh ld req then fixedR else floatR
                pl x req =
                    let width = (fromInt . xc ld . minsize) req * rR' req
                    in  (x + width + fromInt sep,
                         Rect (mkp ld (truncate x) starty)
                              (mkp ld (truncate width) gotv))
            in  snd (mapAccumL pl startx requests')
	(fh',fv') = vswap ld (allf and fixh,allf or fixv)
	rps' = concat rpss --concatMap refpoints requests
	allf conn fix = conn (map (fix ld) requests)
	req0 = refpLayout (mkp ld h v) fh' fv' rps'
	req =
	  case ld of
	    Horizontal -> req0 { hAdj=memoInt ha } 
	      where ha h = Point (totis+sum ws) h
		      where ws = map (xcoord . flip hAdj h) requests
	    Vertical -> req0 { wAdj=memoInt wa } 
	      where wa w = Point w (totis+sum hs)
		      where hs = map (ycoord . flip wAdj w) requests
    in (req,line2)

#ifdef __NHC__
fromInt = fromIntegral
#endif
