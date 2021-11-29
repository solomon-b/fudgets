module ShapeGroupMgr(shapeGroupMgr) where

--import Path
import LoopLow
import Command
--import Event
import Fudget
import FRequest
import Geometry
--import Message(Message(..))
--import NullF
--import Spacers
--import Alignment
import Spops
--import EitherUtils
import Data.Maybe(fromMaybe,mapMaybe)
import Utils
import Xtypes
import WindowF(kernelTag,getBWidth,adjustBorderWidth,border_width)
--import AuxTypes(Ordering(..)) -- HBC bug workaround 960405 TH.
--import Prelude hiding (Ordering)

doConfigure tag wins cws = lookup tag wins >>= \br ->
    let br' = upd br cws 
        upd br [] = br
	upd br@(bw,r@(Rect (Point x y) (Point w h))) (c:cs) = 
	   upd (case c of
		 CWX x' -> (bw,Rect (Point x' y) (Point w h))
		 CWY y' -> (bw,Rect (Point x y') (Point w h))
		 CWWidth w' -> (bw,Rect (Point x y) (Point w' h))
		 CWHeight h' -> (bw,Rect (Point x y) (Point w h'))
		 CWBorderWidth bw' -> (bw',r)
		 _ -> br) cs
    in if br  == br' then Nothing else Just $ replace (tag,br') wins

filterBorderwidth = mapMaybe (\c->case c of CWBorderWidth _ -> Nothing
					    _-> Just c)
shapeGroupMgr :: F a b -> F a b
shapeGroupMgr f  = loopThroughLowF (sg (border_width,[])) f where
   sg state@(bw,wins) =
      getSP $ \msg -> 
      let same = sg state
          pass = putSP msg
	  passame = pass same 
          reshape bw wins = shape ShapeBounding bw $
			    shape ShapeClip 0 $ sg (bw,wins) where
	     shape kind bw =
	       putSP (Left (kernelTag,
	                      XCmd $
	                      ShapeCombineRectangles 
			        kind
				origin
				(map (adj bw.snd) wins) ShapeSet Unsorted))
	  adj bw (lbw,Rect p s) = Rect (p `psub` (Point bw bw))
			               (adjustBorderWidth (lbw+bw) s)
      in case msg of
        Left (tag,cmd) -> 
            case cmd of
	      XReq (CreateSimpleWindow stag r) | tag == kernelTag ->
	         pass $ sg (bw,(stag,(border_width,r)):wins)
	      XCmd (ConfigureWindow cws) | tag == kernelTag ->
	         let bw' = fromMaybe bw (getBWidth cws) 
		     cws'= filterBorderwidth cws
		 in
		 putSP (Left (tag,XCmd (ConfigureWindow cws'))) $
		 if bw == bw' then same else reshape bw' wins
	      XCmd (ConfigureWindow cws) -> 
	         case doConfigure tag wins cws of
		    Nothing -> passame
		    Just wins' -> pass $ reshape bw wins'
	      XCmd DestroyWindow ->
		 pass $ sg (bw,filter ((/=tag).fst) wins)
	      _ -> passame
        _ -> passame

