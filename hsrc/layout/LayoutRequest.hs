module LayoutRequest where
import Geometry(Point(..), Size(..),Rect(..){-,padd,psub,pmax,pP-})
--import EitherUtils(mapMaybe)
--import HO(apFst)
--import Maptrace(ctrace) -- debugging
import Alignment
import ShowFun()

data LayoutRequest
  = Layout { minsize :: Size,
	     fixedh, fixedv :: Bool,
	     wAdj, hAdj :: Int -> Size,
		-- If the available width is w
		-- then the size of this box should be wAdj w.
		-- Analogously for hAdj.
	     refpoints :: [Point], -- used by some placers
	     wantedPos :: Maybe (Point,Size,Alignment)
           }
  deriving (Show)

plainLayout s fh fv = refpLayout s fh fv []
refpLayout s fh fv rps = Layout s fh fv wa ha rps Nothing
  where
    wa w = {-ctrace "wa" (show (w::Int,s)) $-} s
    ha h = {-ctrace "ha" (show (h::Int,s)) $-} s
    --wa = if fh then const s else \ w -> pmax s (pP w 0)
    --ha = if fv then const s else \ h -> pmax s (pP 0 h)

data LayoutMessage
  = LayoutRequest LayoutRequest
  | LayoutMakeVisible Rect (Maybe Alignment,Maybe Alignment)
  | LayoutScrollStep Int
  | LayoutName String
  | LayoutPlacer Placer
  | LayoutSpacer Spacer
  | LayoutHint LayoutHint
  | LayoutDoNow
  | LayoutDestroy
  | LayoutReplaceSpacer Spacer  -- for use by dynSpacerF
  | LayoutReplacePlacer Placer  -- for use by dynPlacerF
  deriving (Show)

data LayoutResponse
  = LayoutPlace Rect
  | LayoutSize Size
  | LayoutPos Point -- Position in parent window. Occationally useful.
  deriving Show

layoutMakeVisible r = LayoutMakeVisible r (Nothing,Nothing)

newtype Placer = P Placer1 deriving (Show)

type Placer1 = ([LayoutRequest] -> Placer2)
type Placer2 = (LayoutRequest, Rect -> [Rect])
unP (P p) = p

newtype Spacer = S Spacer1 deriving (Show)
type Spacer1 = (LayoutRequest -> Spacer2)
type Spacer2 = (LayoutRequest, Rect -> Rect)
unS (S s) = s

type LayoutHint = String -- ??

--mapLayoutSize f req@(Layout {minsize=s}) = req{minsize=f s}
mapLayoutSize f = mapAdjLayoutSize f id id

mapAdjLayoutSize f wf hf req@(Layout {minsize=s,wAdj=wa,hAdj=ha}) =
  req{minsize=f s, wAdj=f.wa.wf, hAdj=f.ha.hf}

mapLayoutRefs f req@(Layout{refpoints=rps}) = req{refpoints=map f rps}

flipReq (Layout p fh fv wa ha rps wanted) =
  Layout (flipPoint p) fv fh
	 (flipPoint . ha) (flipPoint . wa)
	 (map flipPoint rps)
	 (fmap flipWanted wanted)

flipWanted (p,s,a) = (flipPoint p,flipPoint s,a)

flipRect (Rect p s) = Rect (flipPoint p) (flipPoint s)
flipPoint (Point x y) = Point y x
