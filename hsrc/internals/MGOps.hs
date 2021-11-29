module MGOps where
import MeasuredGraphics(MeasuredGraphics(..))
import Maptrace(ctrace)
import Utils(anth)

{-
mgPart drawing path =
  case path of
    [] -> drawing
    p:ps -> part drawing
      where
        part drawing =
	  case drawing of
	    LeafM _ _ _     -> error "bad path in mgPart"
	    SpacedM   _  d  -> part d
	    PlacedM   _  d  -> part d
	    ComposedM    ds -> mgPart (ds !! ((p::Int)-1)) ps
-}

{-    
replaceMGPart drawing path new =
  (if any (<1) path
  then ctrace "replaceMGPart" path
  else id) $ replaceMGPart' drawing path new
-}

replaceMGPart drawing path new = updateMGPart drawing path (const new)

-- Replacing a part without changing the structure
updateMGPart drawing path f =
  case path of
    [] -> f drawing
    p:ps  -> repl drawing
      where
        err = error ("bad path in replaceMGPart: "++show path)
	repl0 d = if p==0
	          then updateMGPart d ps f
		  else err
        repl drawing =
	  case drawing of
	    LeafM _ _            -> err
	    MarkM     gctx    d  -> MarkM gctx (repl0 d)
	    SpacedM   spacer  d  -> SpacedM spacer (repl0 d)
	    PlacedM   placer  d  -> PlacedM placer (repl0 d)
	    ComposedM         ds -> ComposedM ds'
              where ds' = anth p (\d->updateMGPart d ps f) ds

-- Changing the structure but not the appearance
groupMGParts pos len drawing =
  case drawing of
    ComposedM ds -> ComposedM (ds1++ComposedM ds2:ds3)
      where
        (ds1,ds2a) = splitAt (pos-1) ds
        (ds2,ds3) = splitAt len ds2a
    _ -> drawing

-- Changing the structure but not the appearance
ungroupMGParts pos drawing =
  case drawing of
    ComposedM ds ->
        case splitAt (pos-1) ds of
          (ds1,ComposedM ds2:ds3) -> ComposedM (ds1++ds2++ds3)
          _ -> drawing
    _ -> drawing

parentGctx gctx mg path =
  case path of
    [] -> gctx
    0:ps ->
      case mg of
        MarkM gctx' mg' -> parentGctx gctx' mg' ps
	SpacedM _   mg' -> parentGctx gctx mg' ps
	PlacedM _   mg' -> parentGctx gctx mg' ps
	_ -> ctrace "badpath" path gctx -- This is actually an error
    p:ps ->
      case mg of
	ComposedM   mgs ->
	  --{-
	  if p>length mgs
	  then ctrace "badpath" path gctx -- This is actually an error
	  else --}
	  parentGctx gctx (mgs!!(p-1)) ps
	_ -> ctrace "badpath" path gctx -- This is actually an error
{-
seqMG mg k =
  case mg of
    LeafM _ _ -> k
    SpacedM _ mg -> seqMG mg k
    PlacedM _ mg -> seqMG mg k
    MarkM _ mg -> seqMG mg k
    ComposedM mgs -> foldr seqMG k mgs


sizeMG mg =
  case mg of
    LeafM _ _ -> 1::Int
    SpacedM _ mg -> 1+sizeMG mg
    PlacedM _ mg -> 1+sizeMG mg
    MarkM _ mg -> 1+sizeMG mg
    ComposedM mgs -> 1+sum (map sizeMG mgs)
-}
