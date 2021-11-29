module MeasuredGraphics(DPath,up,MeasuredGraphics(..),compileMG,emptyMG,emptyMG',measureString,measureImageString,measurePackedString) where
import Geometry
import LayoutRequest
--import Placers2(overlayP)
import AutoPlacer(autoP)
import XDraw
import ResourceIds(GCId)
import Font
import TextExtents(queryTextExtents16K)
import Rects(boundingRect)
import CompiledGraphics
import GCtx(GCtx(..))
import GCAttrs(FontData(..))
import PackedString(unpackPS)
import Utils(lunconcat)
--import CmdLineEnv(argFlag)
import Debug.Trace(trace)

tr x y = y

type DPath = [Int] -- path to a part of a drawing

up :: DPath -> DPath
up [] = []
up path = init path

data MeasuredGraphics
--  = LeafM LayoutRequest GCtx (Rect->[DrawCommand])
	    -- GCtx should be replaced by GCId !
  = LeafM LayoutRequest (Rect->[(GCId,[DrawCommand])])
  | SpacedM Spacer MeasuredGraphics
  | PlacedM Placer MeasuredGraphics
  | MarkM GCtx MeasuredGraphics -- path & Gctx preserving nodes
  | ComposedM [MeasuredGraphics]

emptyMG size = emptyMG' (plainLayout size False False)
emptyMG' layout = LeafM layout (const [])

compileMG f mg = (cg,req)
  where
    (reqs,cg) = tr "layoutMG" $ layoutMG mg places
    (req0,placer2) = unP autoP reqs
    req = mapLayoutSize f req0
    place = tr "Rect" $ Rect origin (minsize req)
    places = placer2 place

    layoutMG :: MeasuredGraphics -> [Rect] -> ([LayoutRequest],CompiledGraphics)
    layoutMG mg rs =
      case mg of
        MarkM _ mg' -> (lls,cgMark cg)
	  where (lls,cg) = layoutMG mg' rs
	LeafM ll rcmds -> (tr "LeafM"  [ll],cgLeaf r rcmds)
	  where [r] = rs -- bug if length rs/=1 !!
	SpacedM (S spacer1) mg' -> (tr "SpaceM" lls',cgMark cg)
	  where (lls,cg) = layoutMG mg' rs'
	        (lls',spacer2s) = unzip (map spacer1 lls)
	        rs' = map2' id spacer2s rs
	PlacedM (P placer1) mg -> (tr "PlacedM" [ll'],cgMark cg)
	  where (lls,cg) = layoutMG mg rs'
		(ll',placer2) = placer1 lls
		rs' = placer2 r
		[r] = tr ("#rs="++show (length rs)) rs
	ComposedM [] -> (tr "ComposedM []" [],cgCompose (Rect origin origin) [])
	ComposedM mgs -> (tr "ComposedM" lls',cgCompose r cgs)
	  where (llss,cgs) = unzip (map2' layoutMG mgs rss)
	        lls' = concat llss
		r = case rs of
		      [] -> trace ("ComposedM, rs=[], length mgs="++show (length mgs)) (rR 0 0 1 1)
		      _ -> foldr1 boundingRect rs -- !!
	        rss = lunconcat llss rs

measureString'' unpack draw s (GC gc fd) k =
     measure $ \ a d next size ->
     let p1 = Point 0 a
	 p2 = Point next a
	 drawit (Rect p (Point _ h)) = [(gc,[draw (p+(Point 0 (h-d))) s])]
	 size' = Point (xcoord p2) (ycoord size) -- paragraphP bug workaround
     in --trace (unwords ["measureString ",unpack s,"rect",show r]) $
        k (LeafM (refpLayout size' True True [p1,p2]) drawit)
  where
    us = unpack s
    measure k =
      case fd of
        FS fs -> k a d next size
	  where
	    Rect _ size = string_rect fs us
	    a = font_ascent fs
	    d = font_descent fs
	    next = next_pos fs us
	FID fs ->
          let fid = font_id fs
	  in  if null us
   	      then queryTextExtents16K fid " " $ \ a d cs ->
	           k a d 0 (Point 0 (a+d))
	      else queryTextExtents16K fid us $ \ a d cs ->
	           k a d (char_width cs) (Point (char_rbearing cs) (a+d))

measureString' draw8 draw16 s gctx@(GC _ fd) k =
    measureString'' id draw s gctx k
  where
    draw =
      case fd of
        FS fs | snd (font_range fs) <= '\xff' -> draw8
	_ -> draw16

measureString = measureString' DrawString DrawString16
measureImageString = measureString' DrawImageString DrawImageString16

{- old:
measureString =
  if argFlag "string16bit" False
  then measureString' id DrawString16
  else measureString' id DrawString
-}

measurePackedString = measureString'' unpackPS DrawStringPS


-- map2' is a lazier version of map2 (aka zipWith)
-- length (map2' f xs ys) = length xs,
-- map2' f xs ys = map2' f xs (ys++repeat undefined)
map2' f [] _ = []
map2' f (x:xs) ~(y:ys) = f x y:map2' f xs ys
