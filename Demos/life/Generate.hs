module Generate where

import Utils2

generate (bounds,g) = (bounds,newg)
    where (xm,ym) = bounds
	  wrap u m = (u+m) `mod` m
	  newg = (cond g . foldl insert [] . reverse) [(wrap u xm, wrap v ym) | (x,y) <- g, u <- [x-1..x+1], v <- [y-1..y+1]]
	  insert (pn @ (p1,n):l) p | p == p1 = (p1,n+1):l
				   | p > p1  = pn : insert l p
	  insert l p = (p,1) : l
	  cond l ((p,n):nbs) = if not lived && n == 3 || lived && (n == 3 || n == 4) then p : rest else rest
			       where (l',lived) = mapgen (\l->(l,True)) (flip const) (\l->(l,False)) p l
				     rest = cond l' nbs 
	  cond _ [] = []

	
