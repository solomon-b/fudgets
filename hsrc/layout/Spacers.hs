module Spacers where
import LayoutRequest
import Geometry
import Utils(mapPair)
import Alignment

---- Spacer types

type Distance = Int


---- Primitive Spacers

-- Fixed margins

hMarginS, vMarginS :: Distance -> Distance -> Spacer
hMarginS dLeft dRight = hvMarginS (pP dLeft 0) (pP dRight 0)
vMarginS dTop dBottom = hvMarginS (pP 0 dTop) (pP 0 dBottom)

hvMarginS :: Size -> Size -> Spacer
hvMarginS dUpperLeft dBottomRight = S $ \ req ->
  let growth = dUpperLeft + dBottomRight
  in (mapLayoutRefs (+dUpperLeft) $
        mapAdjLayoutSize (+growth) (+(-xcoord growth)) (+(-ycoord growth)) req,
      center' dUpperLeft growth)

center p (Rect r s) = Rect (r+p) (s-(p+p))
center' offset shrink (Rect r s) = Rect (r+offset) (s-shrink)

sepS :: Size -> Spacer
sepS s = hvMarginS s s

marginS :: Distance -> Spacer
marginS d = sepS (diag d)

-- Flexible margins

leftS = hAlignS aLeft
hCenterS = hAlignS aCenter
rightS = hAlignS aRight

vAlignS = flipS . hAlignS
topS = flipS leftS
vCenterS = flipS hCenterS
bottomS = flipS rightS

hvAlignS hpos vpos = hAlignS hpos `compS` vAlignS vpos
centerS = vCenterS `compS` hCenterS

hAlignS :: Alignment -> Spacer
hAlignS hpos = S $ \ (Layout size@(Point rw _) fh fv wa ha rps wanted) ->
  let
    wa' w = wa (min rw w)
    hAlignR (Rect p@(Point x y) s@(Point aw ah)) =
	Rect (pP (x+spaceLeft) y) (pP rw' ah)
      where
	space = aw-rw'
	spaceLeft = scale hpos space
	rw' = min rw aw
	rw = xcoord (ha ah)
  in (Layout size False{-fh-} fv wa' ha rps wanted,hAlignR)

marginHVAlignS sep halign valign = marginS sep `compS` hvAlignS halign valign

--- Spacer operations

spacerP :: Spacer -> Placer -> Placer
spacerP (S spacer) (P placer) = P $ \ reqs ->
  let   (req',placer2) = placer reqs
        (req'',spacer2) = spacer req'
  in (req'',placer2.spacer2)

--flipS :: Spacer -> Spacer
flipS = mapS flipS'
  where
    flipS' spacer = mapPair (flipReq,flipS2) . spacer . flipReq
    flipS2 spacer2 = flipRect.spacer2.flipRect

mapS f (S sp) = S (f sp)

--idS :: Spacer
idS = S $ \ req -> (req,id)

compS :: Spacer -> Spacer -> Spacer
compS (S spa) (S spb) = S $ \ req ->
  let   (req',spb2) = spb req
        (req'',spa2) = spa req'
  in (req'',spb2.spa2)


sizeS,maxSizeS,minSizeS :: Size -> Spacer
sizeS    = resizeS . const
maxSizeS = resizeS . pmin
minSizeS = resizeS . pmax

resizeS :: (Size->Size) -> Spacer
resizeS = layoutModifierS . mapLayoutSize
-- The above and below lines now mean the same
--resizeS f = layoutModifierS (mapAdjLayoutSize f id id)

noStretchS :: Bool -> Bool -> Spacer
noStretchS fh fv = layoutModifierS lf
  where lf req = req { fixedh=fh, fixedv=fv }
--noStretchS fh fv req = (mapLayout lf req ,id)
--  where lf size _ _ wa ha rps = Layout size fh fv wa ha rps

mapLayout f req =
  case req of
    Layout size fh fv wa ha rps wanted -> f size fh fv wa ha rps wanted

--layoutModifierS :: (LayoutRequest -> LayoutRequest) -> Spacer
layoutModifierS lf = S $ \ req -> (lf req,id)
