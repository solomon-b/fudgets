module NullF(module NullF, Cont(..),K,F,StreamProcIO(..),FudgetIO(..)) where
import Utils(pair)
import Fudget
import Message(aLow,stripHigh) --Message(..),
import Path(here)
import Spops
import Cont(dropSP,kContWrap,fContWrap,waitForK,waitForFu)
import StreamProcIO
import FudgetIO

--{- -}
instance StreamProcIO F where
  put = putF
  get = getF -- Discards low level input! Leave undefined?
  end = nullF

instance StreamProcIO K where
  put = putHigh
  get = getHigh
  end = nullK

instance FudgetIO F where
  waitForMsg = waitForFu
  putMsg = putMessageFu
  --nullMsg = nullF
  --getMsg = getMessageFu


instance FudgetIO K where
  putMsg = putK
  waitForMsg = waitForK
  --nullMsg = nullK
  --getMsg = getK

---}

----

nullK = K{-kk-} nullSP
nullF = F{-ff-} nullSP

--putK :: KCommand ho -> K hi ho -> K hi ho
putK o (K sp) = kk (putSP o sp)

putF = putMessageF . High

putsF = puts :: ([b] -> F a b -> F a b)
--putsF his f = foldr putF f his
putsK = putMsgs :: ([KCommand b] -> K a b -> K a b)
--putsK msgs k = foldr putK k msgs

putMessageF msg (F sp) = F{-ff-} (putSP msg sp)
putMessageFu = putMessageF . aLow (pair here)

putMessagesF hos (F sp) = F{-ff-} (putsSP hos sp)
putMessagesFu = putMsgs :: ([KCommand b] -> F a b -> F a b)
--putMessagesFu msgs f = foldr putMessageFu f msgs

--appendStartK :: [KCommand b] -> K a b -> K a b
appendStartK kcmds (K sp) = kk (appendStartSP kcmds sp)

--appendStartMessageF :: [FCommand b] -> F a b -> F a b
appendStartMessageF fcmds (F sp) = F{-ff-} (appendStartSP fcmds sp)

--appendStartF :: [b] -> F a b -> F a b
appendStartF = appendStartMessageF . map High

getK = kContWrap getSP -- :: (Cont (K a b) (KEvent a))

getMessageF = fContWrap getSP -- :: (Cont (F a b) (FEvent a))
getMessageFu = fContWrap (getSP . (. aLow snd)) :: (Cont (F a b) (KEvent a))

--getF :: Cont (F a b) a
getF = fContWrap (dropSP stripHigh)

--startupK :: ([KEvent a] -> K a b -> K a b)
startupK kevs (K sp) = kk (startupSP kevs sp)
--startupMessageF :: ([FEvent a] -> F a b -> F a b)
startupMessageF fevs (F sp) = F{-ff-} (startupSP fevs sp)

--startupMessageFu = startupMessageF . map (aLow (pair here)) -- error prone

--startupF :: [a] -> F a b -> F a b
startupF = startupMessageF . map High

--delayF :: (F a b -> F a b)
delayF (F sp) = F{-ff-} (delaySP sp)
