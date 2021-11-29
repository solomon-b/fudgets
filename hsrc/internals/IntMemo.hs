{-# LANGUAGE CPP #-}
module IntMemo(memoInt) where
import Prelude hiding (lookup)
import CmdLineEnv(argFlag)

#ifndef __NHC__
#define STRICTARG(x) (x){-#STRICT#-}
#else
#define STRICTARG(x) (x)
#endif

data IntMemo a
  = Tip
  | Node a (IntMemo a) (IntMemo a)

lookup :: Int -> IntMemo a -> Maybe a
lookup n m =
  case m of
    Tip -> Nothing
    Node x l r ->
      case n of
	0 -> Just x
        _ -> lookup (n `div` 2) (STRICTARG(if n `mod` 2==0 then l else r))

memoInt :: (Int->a)->(Int->a)
memoInt f = if memoOn then g else f
  where
    g n = if n<0
          then case lookup (-n) negtable of Just x -> x
	  else case lookup n table of Just x -> x
    table = tabulate 0 1
    negtable = tabulate 0 (-1)
    tabulate n b = Node (f n) (tabulate n (STRICTARG(2*b)))
			      (tabulate (STRICTARG(n+b)) (STRICTARG(2*b)))

memoOn = argFlag "memoint" True
