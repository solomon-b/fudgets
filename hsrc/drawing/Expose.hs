module Expose where

import Message
import Cont
--import Command
--import LayoutRequest
import Fudget
--import Xtypes
import Geometry(Rect,rmax)
import FRequest
import Event
--import Font

collectExposeK :: Bool -> Rect -> Int -> ([Rect] -> K a b) -> K a b
collectExposeK grX r aft c = collect [r] aft
   where collect rs aft = 
	   if aft == 0 then c rs
	   else waitForK (\r ->
			case r of
			 Low (XEvt (Expose r aft')) | not grX -> Just (r,aft')
			 Low (XEvt (GraphicsExpose r aft' _ _)) | grX -> Just (r,aft')
			 _ -> Nothing) $ \(r,aft')->
			collect (r:rs) aft'


maxExposeK grX r aft c = collectExposeK grX r aft $ \rs ->
		     c (foldl1 rmax rs)
