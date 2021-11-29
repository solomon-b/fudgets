module CompiledGraphics where
import Geometry(Rect(..))
import Utils(number)
import DrawTypes(DrawCommand)
import ResourceIds(GCId,rootGC)
import Rects(overlaps,boundingRect)
import Maptrace(ctrace)

{-
--Version 1:
data CompiledGraphics = CGraphics Rect GCtx [DrawCommand] [CompiledGraphics]
-}

{-
--Version 2:
data CompiledGraphics = CGraphics Rect [XCommand] [CompiledGraphics]
       -- The only XCommand used is Draw MyWindow some_GC some_DrawCommand
-}

{-
--Version 3:
data CompiledGraphics = CGraphics Rect Cursor [XCommand] [CompiledGraphics]
       -- The only XCommand used is Draw MyWindow some_GC some_DrawCommand
-}

{-
--Version 4:
data CompiledGraphics
  = CGraphics Rect Cursor [XCommand] [CompiledGraphics]
       -- The only XCommand used is Draw MyWindow some_GC some_DrawCommand
  | CGMark CompiledGraphics -- path preserving dummy nodes
-}

--Version 5:
data CompiledGraphics
  = CGraphics Rect Cursor [(GCId,[DrawCommand])] [CompiledGraphics]
  | CGMark CompiledGraphics -- path preserving dummy nodes
  deriving (Show)

type Cursor = Bool

cgLeaf r rcmds =
    --ctrace "gctrace" cmds $
    cg
  where
    cg = CGraphics r False  cmds []
    --cmds = (map (Draw MyWindow gc) (f r))
    cmds = rcmds r

cgMark = CGMark

cgCompose r cgs = CGraphics r False cmds cgs
  where
    cmds = if anyOverlap cgs
	   then ctrace "cgoverlap" (map cgrect cgs) $
		[(rootGC,[])] --trick coding to force all subtrees to be redrawn
	   else []

    -- This is O(n^2) in general, but the bounding rect makes it O(n) for
    -- linear placers. It may also help somewhat for tables...
    -- Sorting the rectangles can cut down the complexity too...
    -- Better to let the placers tell if they produce overlapping parts...
    anyOverlap [] = False
    anyOverlap (cg:cgs) = anyOverlaps' [r] r cgs
      where r = cgrect cg

    anyOverlaps' rs bounding [] = False
    anyOverlaps' rs bounding (cg:cgs) =
        r `overlaps` bounding && any (overlaps r) rs ||
	anyOverlaps' (r:rs) (boundingRect bounding r) cgs
      where r = cgrect cg

cgrect (CGMark cg) = cgrect cg
cgrect (CGraphics r _ _ _) = r
cgsize = rectsize.cgrect

addcursor (CGMark cg) = CGMark (addcursor cg) -- hmm!!
addcursor (CGraphics r _ cmds cgs) = CGraphics r True cmds cgs
removecursor (CGMark cg) = CGMark (removecursor cg) -- hmm!!
removecursor (CGraphics r _ cmds cgs) = CGraphics r False cmds cgs
hascursor (CGMark cg) = hascursor cg -- hmm!!
hascursor (CGraphics _ cur _ _) = cur

cgpart cg [] = cg
cgpart (CGMark cg) (0:ps) = cgpart cg ps
cgpart (CGraphics _ _ _ parts) (p:ps) =
  if p<1||p>length parts then error "bad path in CompiledGraphics.cgpart " else
  cgpart (parts !! ((p::Int)-1)) ps

cgreplace cg path new = cgupdate cg path (const new)

{-
cgupdate cg ps f =
  (if any (<1) ps
  then ctrace "cgupdate" ps
  else id) $ cgupdate' cg ps f
-}

cgupdate cg                            []     f = f cg
cgupdate (CGMark cg)                   (0:ps) f = CGMark (cgupdate cg ps f)
cgupdate (CGMark cg)                   ps     f =
   ctrace "badpath" ("(CGMark _) "++show ps) $
   CGMark (cgupdate cg ps f)
cgupdate cg                            (0:ps) f =
   ctrace "badpath" (cg,0:ps) $
   CGMark (cgupdate cg ps f)
cgupdate (CGraphics r cur dcmds parts) (p:ps) f =
    CGraphics r cur dcmds (pre++cgupdate cg' ps f:post)
  where pre = take (p-1) parts
        cg':post = drop ((p-1)::Int) parts

cgcursors :: CompiledGraphics -> [[Int]]
cgcursors (CGMark cg) = map (0:) (cgcursors cg)
cgcursors (CGraphics _ cur _ parts) =
    if cur
    then []:partcursors
    else partcursors
  where
    partcursors =
      concatMap (\(n,ps) -> map (n:) (cgcursors ps)) (number 1 parts)

cgGroup pos len (CGMark cg) = CGMark (cgGroup pos len cg) -- hmm!!
cgGroup pos len (CGraphics r cur dcmds parts) =
    CGraphics r cur dcmds (ds1++cgCompose r2 ds2:ds3)
  where
    (ds1,ds2a) = splitAt (pos-1) parts
    (ds2,ds3) = splitAt len ds2a
    r2 = foldr (boundingRect.cgrect) (Rect 0 0) ds2

cgUngroup pos (CGMark cg) = CGMark (cgUngroup pos cg) -- hmm!!
cgUngroup pos cg@(CGraphics r cur dcmds parts) =
    case splitAt (pos-1) parts of
      (ds1,d2:ds3) ->
        case unmark 0 d2 of
          (m,CGraphics r2 cur2 dcmds2 ds2) ->
            CGraphics r cur (dcmds++dcmds2) (ds1++map (mark m) ds2++ds3)
          _ -> cg -- hmm!!

      _ -> cg -- hmm!!
  where
    unmark n (CGMark cg) = unmark (n+1) cg
    unmark n cg = (n,cg)

    mark 0 cg = cg
    mark n cg = mark (n-1) (CGMark cg)
