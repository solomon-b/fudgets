module GCtx where
import GCAttrs
import Xtypes
import Gc(createGC)
import Command(Drawable(..))
--import Font(FontStruct)
--import FudgetIO
--import NullF(F,K)
--import Maptrace(ctrace)

data GCtx = GC GCId FontData
 --deriving (Show) -- don't want to see FontStruct
instance Show GCtx where
  showsPrec d (GC gc fs) = ("GC "++) . showsPrec 10 gc . ("<<FontStruct>>"++)

gctx2gc (GC gc _) = gc

rootGCtx = GC rootGC (error "GCtx.rootGCtx0")
--  if usefontstructs then rootGCtx2 else rootGCtx0

--rootGCtx0 = GC rootGC (FID (error "GCtx.rootGCtx0"))
--rootGCtx1 = ...
--rootGCtx2 = GC rootGC (FS (error "GCtx.rootGCtx2"))

data GCSpec -- move to module Drawing?
  = SoftGC [GCAttributes ColorSpec FontSpec]
  | HardGC GCtx
  deriving (Show)

createGCtx drawable gctx@(GC gc fd) gcattrs k =
  --ctrace "gctrace" gc $
  convGCSpecK fd gcattrs $ \ gcattrs' fd' ->
  createGC drawable gc gcattrs' $ \ gc' ->
  --ctrace "gctrace" gc' $
  k (GC gc' fd')

wCreateGCtx x = createGCtx MyWindow $ x
pmCreateGCtx x = createGCtx . Pixmap $ x
