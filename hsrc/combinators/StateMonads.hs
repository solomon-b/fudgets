{-# LANGUAGE CPP #-}
module StateMonads where
import Control.Applicative
import Control.Monad(ap)
import Fudget(K) --,KEvent
import FudgetIO
import StreamProcIO
import EitherUtils(Cont(..))
import NullF(getK)

--------------------------------------------------------------------------------

-- | The continuation monad
#ifdef __HBC__
newtype Mk k r = Mk (Cont k r)
unMk (Mk mk) = mk
#else
newtype Mk k r = Mk {unMk::Cont k r}
#endif
-- | Continuation monad with unit result
type Mkc k = Mk k ()

instance Functor (Mk k) where
  fmap f (Mk m) = Mk (\k -> m (k.f))

instance Applicative (Mk k) where
  pure = return
  (<*>) = ap
  
instance Monad (Mk k) where
  return r =  Mk ($ r)
  Mk m1 >>= xm2 = Mk (m1 . flip (unMk . xm2))

--------------------------------------------------------------------------------

-- | Continuation monad with state (just an instance of the continuation monad)
type Ms k s r = Mk (s -> k) r
type Msc k s = Ms k s ()

loadMs  :: Ms k s s
storeMs :: s -> Msc k s
modMs   :: (s -> s) -> Msc k s
fieldMs :: (s -> f) -> Ms k s f

loadMs    = Mk (\ k s -> k s s)
storeMs s = Mk (\ k _ -> k () s)
modMs   f = Mk (\ k s -> k () (f s))
fieldMs r = Mk (\ k s -> k (r s) s)

nopMs :: Msc k s
nopMs = return ()

--------------------------------------------------------------------------------

toMkc :: (k -> k) -> Mkc k
toMkc k = Mk (\f -> k (f ()))

toMs :: Cont k r -> Ms k s r
toMs f = Mk (bmk f)
bmk f = (f .) . flip

toMsc :: (k -> k) -> Msc k r
toMsc k = Mk (\f -> k . f ())

--------------------------------------------------------------------------------
-- | Fudget Kernel Monad with State (just an instance...)
type Ks i o s ans = Ms (K i o) s ans
--type Ksc i o s = Ks i o s ()

{-
putsKs :: [KCommand a] -> Ksc b a c
putKs  :: KCommand a -> Ksc b a c
getKs  :: Ks a b c (KEvent a)
nullKs :: Ks i o s ()
loadKs :: Ks i o s s
storeKs :: s -> Ks i o s ()
-}
putHighsMs c = toMsc (puts c)
putHighMs  c = toMsc (put c)
putLowsMs  c = toMsc (putLows c)
putLowMs   c = toMsc (putLow c)
getKs        = toMs getK


-- Some synonyms, kept mostly for backwards compatibility
nullKs   =  nopMs
storeKs  = storeMs
loadKs   = loadMs
unitKs x = return x
bindKs m1 xm2 = m1>>=xm2
thenKs m1 m2 = m1>>m2
mapKs f = fmap f

-- Running a kernel monad

--stateMonadK :: s -> Ks i o s ans -> (ans -> K i o) -> K i o
stateMonadK s0 (Mk ks) k = ks (\ans state->k ans) s0

--stateK :: a -> (Ksc b c a) -> (K b c) -> K b c
stateK s (Mk ksc) k = ksc (const (const k)) s
