module Main(main,control) where

import Fudgets
import Life
import Utils2
import Panel
import Generate

--import NonStdTrace

main = fudlogue $ shellF "Life" (hBoxF control)

control = loopLeftF ((Left >^=< life defaultcellsize s0) >==< absF (mapstateSP c s0)) >==< panel
    where c state @ (bounds @ (xm,ym), gen) msg = 
	      case msg of
	          Left fromMatrix ->
                     case fromMatrix of
			 MouseClick p -> ((bounds,newgen), [NewCell (p,newcell)])
			     where (newgen,newcell) = togglegen p gen

			 NewBounds (b @ (xm,ym)) -> redraw (b,[(x,y) | (x,y) <- gen, x < xm, y < ym])
		  Right fromPanel ->
		      case fromPanel of
			  Right newSize -> (state, [NewCellSize newSize])
			  Left tick -> redraw (generate state)
		  _ -> (state,[])
	  redraw ns = (ns,[NewGen ns])
	    
bounds0 = (20,20)

s0 = (bounds0,[])
