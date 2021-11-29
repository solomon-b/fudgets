module Gc(pmCreateGC, wCreateGC, createGC, createGCF, wCreateGCF, pmCreateGCF) where
import Command
import Event
--import FudgetIO
import NullF(F{-,K-})
import Xrequest
import Xtypes
--import Maptrace(ctrace)

createGC drawable template attr =
    let cmd = CreateGC drawable template attr
        expected (GCCreated gc) = {-ctrace "gctrace" gc $-} Just gc
        expected _ = Nothing
    in xrequest cmd expected

--createGCK :: Drawable -> GCId -> GCAttributeList -> (GCId -> K a b) -> K a b
--createGCK = createGC

createGCF :: Drawable -> GCId -> GCAttributeList -> (GCId -> F a b) -> F a b
createGCF = createGC

wCreateGC x = createGC MyWindow x
pmCreateGC x = (createGC . Pixmap) x

wCreateGCF = createGCF MyWindow
pmCreateGCF = createGCF . Pixmap
