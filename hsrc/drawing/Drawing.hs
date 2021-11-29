{-# LANGUAGE DeriveFunctor #-}
module Drawing(Drawing(..),labelD,placedD,atomicD,DPath(..),up,GCSpec) where
import Graphic
import MeasuredGraphics(MeasuredGraphics(..),DPath(..),up)
--import FudgetIO
import NullF() -- instances, for hbc
import GCtx(GCSpec(..),wCreateGCtx)
import Placers2(overlayP)
import LayoutRequest
--import EitherUtils(Cont(..))
import GCAttrs(ColorSpec,FontSpec)
import Xtypes(GCAttributes)
--import Geometry() -- Show instances

data Drawing lbl leaf
  = AtomicD   leaf
  | LabelD    lbl  (Drawing lbl leaf)
  | AttribD   GCSpec (Drawing lbl leaf)
  | SpacedD   Spacer (Drawing lbl leaf)
  | PlacedD   Placer (Drawing lbl leaf)
  | ComposedD Int    [Drawing lbl leaf]   -- ^ Int=how many visible components
  | CreateHardAttribD GCtx [GCAttributes ColorSpec FontSpec] (GCtx -> 
                      Drawing lbl leaf)
  deriving (Show,Functor)

labelD = LabelD
placedD = PlacedD
atomicD = AtomicD

instance Graphic leaf => Graphic (Drawing annot leaf) where
  measureGraphicK = drawK
  measureGraphicListK = drawListK overlayP  -- or autoP ??


drawK d gctx{-@(GC gc fs)-} k =
  case d of
    AtomicD x -> measureGraphicK x gctx k
    LabelD _ d -> drawK d gctx (k . MarkM gctx)
    AttribD gcspec d ->
      wCreateGCtx' gctx gcspec $ \ gctx' ->
      drawK d gctx' (k . MarkM gctx')
    SpacedD spacer d ->
      drawK d gctx $ \ g ->
      k (SpacedM spacer g)
    PlacedD placer d ->
      drawK d gctx $ \ g ->
      k (PlacedM placer g)
    ComposedD n ds ->
      -- take the n visible components, remaining parts are invisible.
      drawsK gctx (take n ds) $ \ gs ->
      k (ComposedM gs)
    CreateHardAttribD templ attrs d ->
      wCreateGCtx templ attrs $ \tx ->
      drawK (d tx) gctx k
{-
  where
    replaceFontK fs gcattrs k = font gcattrs (k fs) (\fid -> queryFont fid k)
    font [] kdef _ = kdef
    font (GCFont fid:gcattrs) _ kfont = kfont fid
    font (_:gcattrs) kdef kfont = font gcattrs kdef kfont
-}

drawListK placer ds gctx k =
  drawsK gctx ds $ \ gs ->
  k (PlacedM placer $ ComposedM gs)

drawsK gctx [] k = k []
drawsK gctx (d:ds) k =
  drawK d gctx $ \ g ->
  drawsK gctx ds $ \ gs ->
  k (g:gs)

wCreateGCtx' gctx gcspec k =
  case gcspec of
    HardGC gctx' -> k gctx'
    SoftGC gcattrs -> wCreateGCtx gctx gcattrs k
