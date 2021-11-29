module CondLayout(ifSizeP,ifSizeS,stretchCaseS,alignFixedS,alignFixedS') where
import LayoutRequest
--import Geometry(Size)
import Spacers(idS,hAlignS,vAlignS,hvAlignS,compS,noStretchS)

--ifSizeP :: (Size->Size->Bool) -> Placer -> Placer -> Placer
--ifSizeS :: (Size->Size->Bool) -> Spacer -> Spacer -> Spacer
ifSizeP b (P p1) (P p2) = P $ ifSize b p1 p2
ifSizeS b (S s1) (S s2) = S $ ifSize b s1 s2

ifSize p primP spareP reqs =
    if p primSize spareSize
    then primP2
    else spareP2
  where
    primP2@(Layout {minsize=primSize},_) = primP reqs
    spareP2@(Layout {minsize=spareSize},_) = spareP reqs

stretchCaseS :: ((Bool,Bool)->Spacer) -> Spacer
stretchCaseS sS = S $ \ req -> unS (sS (fixedh req,fixedv req)) req

alignFixedS ha va =
  stretchCaseS $ \ fixedhv ->
  case fixedhv of
    (False,False) -> idS
    (True, False) -> hAlignS ha
    (False,True)  -> vAlignS va
    (True, True)  -> hvAlignS ha va

alignFixedS' ha va =
  stretchCaseS $ \ fhv ->
   uncurry noStretchS fhv `compS` alignFixedS ha va
