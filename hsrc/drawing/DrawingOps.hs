module DrawingOps where
import Drawing(Drawing(..),DPath(..),up)
import Utils(number)

drawingPart drawing path =
  case maybeDrawingPart drawing path of
    Just part -> part
    Nothing -> error ("bad path in drawingPart "++show path)

maybeDrawingPart drawing path =
  case (path::DPath) of
    [] -> Just drawing
    p:ps -> part drawing
      where
        part0 d = if p==0
	          then maybeDrawingPart d ps
		  else Nothing
        part drawing =
	  case drawing of
	    AtomicD           _  -> Nothing
	    LabelD    _       d  -> part0 d
	    AttribD   gcattrs d  -> part0 d
	    SpacedD   spacer  d  -> part0 d
	    PlacedD   placer  d  -> part0 d
	    ComposedD _       ds ->
	      if 1<=p && p<=length ds
	      then maybeDrawingPart (ds !! (p-1)) ps
	      else Nothing

drawingAnnotPart = drawingAnnotPart' (const True)

drawingAnnotPart' p drawing path =
      case path of
        [] -> []
	_ -> case maybeDrawingPart drawing path of
	       Just (LabelD a _) | p a -> path
	       _ -> drawingAnnotPart' p drawing (up path)

isVisibleDrawingPart drawing path =
  case (path::DPath) of
    [] -> True
    p:ps -> visible drawing
      where
        visible0 d = p==0 && isVisibleDrawingPart d ps -- ??
        visible drawing =
	  case drawing of
	    AtomicD           _  -> True -- ??
	    LabelD    _       d  -> visible0 d
	    AttribD   gcattrs d  -> visible0 d
	    SpacedD   spacer  d  -> visible0 d
	    PlacedD   placer  d  -> visible0 d
	    ComposedD n       ds ->
	      1<=p && p<=n && isVisibleDrawingPart (ds !! (p-1)) ps

visibleAncestor drawing path =
  case path::DPath of
    [] -> path
    p:ps ->
      case drawing of
        AtomicD           _  -> path
	LabelD    _       d  -> skip d
	AttribD   gcattrs d  -> skip d
	SpacedD   spacer  d  -> skip d
	PlacedD   placer  d  -> skip d
	ComposedD n       ds ->
	  if 1<=p && p<=n
	  then p:visibleAncestor (ds!!(p-1)) ps
	  else []
      where skip d = 0:visibleAncestor d ps

replacePart drawing path new = updatePart drawing path (const new)

updatePart drawing path new =
  case (path::DPath) of
    [] -> new drawing
    p:ps  -> repl drawing
      where
        err = error "bad path in updatePart"
        repl0 d = if p==0 then updatePart d ps new else err
        repl drawing =
	  case drawing of
	    AtomicD           _  -> err
	    AttribD   gcattrs d  -> AttribD gcattrs (repl0 d)
	    LabelD    label   d  -> LabelD label (repl0 d)
	    SpacedD   spacer  d  -> SpacedD spacer (repl0 d)
	    PlacedD   placer  d  -> PlacedD placer (repl0 d)
	    ComposedD n       ds ->
	      let (pre,d:post) = splitAt (p-1) ds
              in ComposedD n (pre++updatePart d ps new:post)

mapLabelDrawing f = ma
  where
    ma d =
      case d of
	AtomicD           x  -> AtomicD x
	AttribD   gcattrs d  -> AttribD gcattrs (ma d)
	LabelD    label   d  -> LabelD (f label) (ma d)
	SpacedD   spacer  d  -> SpacedD spacer (ma d)
	PlacedD   placer  d  -> PlacedD placer (ma d)
	ComposedD n       ds -> ComposedD n (map ma ds)

mapLeafDrawing f = ma
  where
    ma d =
      case d of
	AtomicD           x  -> AtomicD (f x)
	AttribD   gcattrs d  -> AttribD gcattrs (ma d)
	LabelD    label   d  -> LabelD label (ma d)
	SpacedD   spacer  d  -> SpacedD spacer (ma d)
	PlacedD   placer  d  -> PlacedD placer (ma d)
	ComposedD n       ds -> ComposedD n (map ma ds)

{-
drawingArity drawing =
  case drawing of
    AtomicD     _  -> 0
    LabelD    _ d  -> drawingArity d
    AttribD   _ d  -> drawingArity d
    SpacedD   _ d  -> drawingArity d
    PlacedD   _ d  -> drawingArity d
    ComposedD _ ds -> length ds
-}

annotChildren = annotChildren' (const True)

annotChildren' :: (a -> Bool) -> (Drawing a d) -> [(DPath, Drawing a d)]   
annotChildren' p drawing =
    case drawing of
      LabelD _ d -> ac0 d
      d -> ac d
  where 
    ac0 d0 = [(0:p,d)| (p,d)<-ac d0]
    ac d =
      case d of
        AtomicD     _  -> []
        LabelD    a d' -> if p a then [([],d)] else ac0 d'
	AttribD   _ d  -> ac0 d
	SpacedD   _ d  -> ac0 d
	PlacedD   _ d  -> ac0 d
	ComposedD _ ds -> [(i:p,d) | (i,cs) <- number 1 (map ac ds), (p,d)<-cs]

{-
drawingAnnots :: Drawing a d -> [(DPath,a)]   
drawingAnnots drawing = da drawing
  where 
    da d =
      case d of
        AtomicD     _  -> []
        LabelD    a d' -> ([],a):da d'
	AttribD   _ d  -> da d
	SpacedD   _ d  -> da d
	PlacedD   _ d  -> da d
	ComposedD _ ds -> [(i:p,d) | (i,cs) <- number 1 (map da ds), (p,d)<-cs]
-}

drawingAnnots drawing = extractParts drawing sel
  where sel (LabelD a d) = Just a
	sel _ = Nothing

extractParts :: Drawing lbl leaf -> (Drawing lbl leaf -> Maybe a) -> [(DPath,a)]
extractParts drawing sel = extr drawing
  where
    extr0 d = [(0:p,d') | (p,d') <- extr d]
    extr d =
      (case sel d of
	 Just x -> (([],x):)
	 _ -> id) $
      case d of
        AtomicD     _  -> []
        LabelD    a d' -> extr0 d'
	AttribD   _ d  -> extr0 d
	SpacedD   _ d  -> extr0 d
	PlacedD   _ d  -> extr0 d
	ComposedD _ ds -> [(i:p,d) | (i,cs) <- number 1 (map extr ds), (p,d)<-cs]

deletePart drawing [] = drawing -- !! error report?
deletePart drawing path =
    updatePart drawing (init path) (deleteElem (last path))
  where
    deleteElem i = di
      where di d =
              case d of
		ComposedD n       ds ->
		  case splitAt (i-1) ds of
		    (ds1,_:ds2) -> ComposedD n' (ds1++ds2)
		    _ -> d -- !! error report?
		  where n' = if i<=n
		             then n-1
			     else n
		_                    -> d   -- !! error report?
{-
		AtomicD           x  -> d   -- !! error report?
		AttribD   gcattrs d  -> AttribD gcattrs (di d)
		LabelD    label   d  -> LabelD  label   (di d)
		SpacedD   spacer  d  -> SpacedD spacer  (di d)
		PlacedD   placer  d  -> PlacedD placer  (di d)
-}

groupParts pos0 len0 drawing =
  case drawing of
    ComposedD n ds -> ComposedD n1 (ds1++ComposedD n2 ds2:ds3)
      where
        (ds1,ds2a) = splitAt (pos0-1) ds
        (ds2,ds3) = splitAt len0 ds2a
        pos = length ds1
        len = length ds2
        -- keep the same parts visible
        (n1,n2) = if n<=pos then (n,0)
                  else if n<=pos+len
                       then (pos+1,n-pos)
                       else (n-len+1,len)

ungroupParts pos drawing =
  case drawing of
    ComposedD n1 ds ->
      case splitAt (pos-1) ds of
        (ds1,ComposedD n2 ds2:ds3) -> ComposedD n (ds1++ds2++ds3)
          where
            -- Can't preserve visibility when some of ds3 was visible
            -- but some of ds2 was hidden
            n = if n1<=pos then n1
                else if n1==pos+1
                     then pos+n2
                     else n1-1+length ds2 -- all of ds2 becomes visible!!
        _ -> drawing -- hmm!!
    _ -> drawing -- hmm!!
