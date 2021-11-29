module FudgetIO where
import Fudget
import EitherUtils(Cont(..))
import Message(stripLow,stripHigh)

{-
The purpose of the FudgetIO class is to allow the many IO operations that
can be performed from both fudgets and fudget kernels, e.g., createGC,
loadQueryFont and allocNamedColor, to use one overloaded name instead of
two separate names.
-}

class FudgetIO f where
  waitForMsg :: (KEvent hi -> Maybe ans) -> Cont (f hi ho) ans
  putMsg :: KCommand ho -> f hi ho -> f hi ho

  -- Less useful methods:
  --nullMsg :: f hi ho -- name ?!
  --getMsg :: (KEvent hi -> f hi ho) -> f hi ho

putMsgs msgs k = foldr putMsg k msgs
putHigh x = (putMsg . High) x
putLow x = (putMsg . Low) x
putLows lows k = foldr putLow k lows

getHigh x = waitForMsg stripHigh x
getLow x = waitForMsg stripLow x

cmdContMsg msg expected = putMsg msg . waitForMsg expected

cmdContLow cmd expected = cmdContMsg (Low cmd) expectLow
  where expectLow msg = stripLow msg >>= expected
