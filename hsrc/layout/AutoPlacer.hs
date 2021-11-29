{-# LANGUAGE CPP #-}
module AutoPlacer(autoP,autoP') where
import LayoutRequest
import Geometry
import Placers(horizontalP',verticalP')
import Spacers() -- synonym Distance, for hbc
import AlignP(idP)
--import LayoutDir
import CmdLineEnv(argFlag)
import Defaults(defaultSep)
import Data.Ratio
import Debug.Trace(trace)

-- should get hints somehow
autoP = autoP' defaultSep

autoP' :: Size -> Placer
autoP' (Point hsep vsep) = P $ \ requests ->
 case requests of
   [] -> trace "autoP []" $ (plainLayout 1 True True, \ r -> [])
   [r] -> unP idP requests
   _ -> p where
        h2@(h,_) = unP (horizontalP' hsep) requests
	v2@(v,_) = unP (verticalP' vsep) requests
	p = if goodness h2 requests<goodness v2 requests
            then h2 else v2

{-
--godness : measure how good a layout is.
-- 1st: a layout is better if it uses less screen space.
-- 2nd: a layout is better if it is more quadratic 
-- (rather than long and narrow)
goodness (Layout {minsize=Point x y}) = (x*y,x+y)
                                 -- (area, circumference of rect / 2)
-- This doesn't work, because of separation between fudgets...
-}

goodness =
  if argFlag "sg" False
  then \ p1 rs -> (1%1,simpleGoodness (fst p1))
  else newGoodness

simpleGoodness (Layout {minsize=Point w h}) = w+h

#if 1
-- normal version
newGoodness (Layout {minsize=s@(Point w h)},placer2) reqs =
    (wasted % (w*h),w+h)
  where
    wasted = sum (zipWith waste reqs (placer2 (Rect origin s)))
    waste (Layout {minsize=Point rw rh,fixedh=fh,fixedv=fv}) (Rect _ (Point aw ah)) =
      case (fh,fv) of
        (True,True) -> aw*ah-rw*rh
	(False,False) -> 0
	(True,False) -> (aw-rw)*ah
	(False,True) -> (ah-rh)*aw
#else
-- Röjemo's debug version
-- needs updating
newGoodness (Layout {minsize=s@(Point w h)},placer2) reqs =
     trace ("w = " ++ show w ++ "\n"
         ++ "h = " ++ show h ++ "\n"
         ++ "wlist = " ++ show wlist ++ "\n"
         ++ "wasted = " ++ show wasted ++ "\n"
         ++ "w*h = " ++ show (w*h) ++ "\n"
         ++ "(wasted % (w*h),w+h) = " ++ show     (wasted % (w*h),w+h) ++ "\n")
    (wasted % (w*h),w+h)
  where
    wlist =  (zipWith waste reqs (placer2 (Rect origin s)))
    wasted = sum wlist
    waste (Layout (Point rw rh) fh fv) (Rect _ (Point aw ah)) =
      case (fh,fv) of
        (True,True) ->    trace ("aw*ah-rw*rh = " ++ show aw ++ '*':show ah ++ '-':show rw ++ '*':show rh ++ "\n") $ aw*ah-rw*rh
        (False,False) ->  trace ("0\n")  0
        (True,False) ->   trace ("(aw-rw)*ah = (" ++ show aw ++ '-':show rw ++ ")*" ++ show ah ++ "\n") $ (aw-rw)*ah
        (False,True) ->   trace ("(ah-rh)*aw = (" ++ show ah ++ '-':show rh ++ ")*" ++ show aw ++ "\n") $ (ah-rh)*aw
#endif
