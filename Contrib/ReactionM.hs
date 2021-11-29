{-# LANGUAGE CPP #-}
module ReactionM where
import Data.Maybe(isJust)
import Control.Applicative
import Control.Monad(ap)
#if MIN_VERSION_base(4,12,0)
#if MIN_VERSION_base(4,13,0)
#else
import Control.Monad.Fail
#endif
#endif

-- | Writer & State & Exception monad
newtype ReactionM s o a = M (s -> [o] -> Maybe (s,[o],a))

instance Functor (ReactionM s o) where
  fmap f m = do x <- m; return (f x)

instance Applicative (ReactionM s o) where
  pure = return
  (<*>) = ap

instance Monad (ReactionM s o) where
  return x = M (\s o->Just (s,o,x))
  (M f1) >>= xm2 =
    M $ \ s0 o0 ->
      let r1 = f1 s0 o2
          Just (s1,o1,x1) = r1
          M f2 = xm2 x1
	  r2 = f2 s1 o0
	  Just (s2,o2,x2) = r2
      in if isJust r1 && isJust r2
         then Just (s2,o1,x2)
	 else Nothing
#if MIN_VERSION_base(4,12,0)
instance MonadFail (ReactionM s o) where
#endif
  fail _ = rfail

react (M f) s0 = case f s0 [] of Just (s,o,_) -> (s,o); _ -> (s0,[])
put o = M $ \ s os -> Just (s,o:os,())
set s = M $ \ _ os -> Just (s,os,())
get = M $ \ s os -> Just (s,os,s)
field f = f <$> get
update f = M $ \ s os -> Just (f s,os,())

rfail = M $ \ _ _ -> Nothing

lift m = maybe rfail return m
nop = return ()
nop :: Monad m => m ()
