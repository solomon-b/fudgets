module Life{-(life)-} where
import AllFudgets
import Utils2

life :: Int -> Generation -> F LifeCmds LifeEvts
--life cellsize (bnds,gen) = simpleF "Life" lifeK (topoint s0 bnds) s0
life cellsize (bnds,gen) = simpleWindowF lifeK (topoint s0 bnds) False False s0
			      where s0 = (cellsize,bnds,gen)

pointPair (x,y) = Point x y
pairPoint (Point x y) = (x,y)

--fillCircle pos d = let s = Point d d in FillArc (Rect pos s) 0 (64 * 360)

lifeK draw clear s event = 
    case event of
       High (NewCell (pos,l))      -> (s1, pattern s1 pos l) where s1 = sGen (setgen l pos (gen s)) s
       High (NewGen (nb,ng))       -> (s1, redrawgen s1) where s1 = sGen ng (sBnds nb s)
       High (NewCellSize ncs)      -> (s1,[(High . NewBounds . topos s1 . topoint s . bnds) s]) where s1 = sCellsize ncs s
       Low (XEvt (Expose _ 0))     -> (s,redrawgen s)
       Low (XEvt (ButtonEvent _ p _ _ Pressed _)) -> (s,[(High . MouseClick . topos s) p])
       Low (LEvt (LayoutSize nsize)) -> (s, [(High . NewBounds . topos s) nsize])
       _ -> (s,[])

    where
	pattern s p l = map (Low . (if l then draw else clear)) [fillCircle (topoint s p) (truncate (cellsize s))]
	redrawgen s = Low (XCmd ClearWindow) : concat [pattern s p True | p <- gen s] ++ [Low $ XCmd Flush]

topoint s = scalePoint (cellsize s) . pointPair
topos s = pairPoint . scalePoint (1 / (cellsize s))

cellsize (cs,b,g) = (fromIntegral cs)
bnds     (cs,b,g) = b
gen      (cs,b,g) = g
xm = fst . bnds
ym = snd . bnds

sCellsize cs1 (cs,b,g) = (cs1,b,g)
sBnds     b1  (cs,b,g) = (cs,b1,g)
sGen      g1  (cs,b,g) = (cs,b,g1)
