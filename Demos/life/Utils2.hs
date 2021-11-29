module Utils2 where

import HbcUtils(apFst)

type Life = Bool
type Pos = (Int, Int)
type Bounds = Pos
type Generation = (Bounds,[Pos])

data LifeCmds = NewCell (Pos, Life) | NewGen Generation | NewCellSize Int
data LifeEvts = NewBounds Bounds | MouseClick Pos

mapgen found more notfound p l = mg l
    where mg (p1:l) | (p == p1) = found l
	            | (p >  p1) = more p1 (mg l)
	  mg l = notfound l

togglegen p = mapgen (\l->(l,False)) (apFst . (:)) (\l->(p:l,True)) p
setgen t p = mapgen c (:) c p where c = if t then (p:) else id

