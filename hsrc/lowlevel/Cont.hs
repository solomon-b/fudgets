module Cont(Cont(..),conts, tryM, cmdContF, cmdContK, cmdContK', waitForSP, waitForK, waitForF, waitForFu, cmdContSP,tryGet,getLeftSP,getRightSP,fContWrap,kContWrap,dropSP,contMap) where
--import Direction
import Fudget
import Message(stripHigh, stripLow, aLow) --Message(..),
import Path(here)
--import SP
import Spops
import StreamProcIO
import EitherUtils(stripLeft,stripRight)

cmdContSP :: a -> (b -> Maybe c) -> Cont (SP b a) c
cmdContSP cmd expected process =
    putsSP [cmd] (waitForSP expected process)

waitForSP expected process =
    let contSP pending =
            getSP (\msg ->
                   case expected msg of
                     Just answer -> startupSP (reverse pending) (process answer)
                     Nothing -> contSP (msg : pending))
    in  contSP []

waitForK expected = kContWrap (waitForSP expected)

waitForF :: (a -> Maybe b) -> Cont (F a c) b
waitForF expected = fContWrap (waitForSP expectHigh)
  where expectHigh msg = stripHigh msg >>= expected

waitForFu :: (KEvent hi -> Maybe ans) -> Cont (F hi ho) ans
waitForFu expected = fContWrap (waitForSP expectk)
  where expectk = expected . aLow snd

getLeftSP = waitForSP stripLeft
getRightSP = waitForSP stripRight

waitForLow expected = waitForSP expectLow
  where expectLow msg = stripLow msg >>= expected

cmdContLow cmd expected = putSP (Low cmd) . waitForLow expected

{- old:
cmdContLow cmd exp' =
    cmdContSP (Low cmd)
              (\msg ->
               case msg of
                 Low ev -> exp' ev
                 _ -> Nothing)
-}

cmdContK :: FRequest -> (FResponse -> Maybe a) -> Cont (K b c) a
cmdContK xcmd expected = kContWrap (cmdContLow xcmd expected)

cmdContK' msg expected = kContWrap (cmdContSP msg expected)

cmdContF :: FRequest -> (FResponse -> Maybe a) -> Cont (F b c) a
cmdContF cmd exp' =
    fContWrap $
    cmdContLow (here, cmd)
               (\tev ->
                case tev of
                  (t, ev) | t == here -> exp' ev
                  _ -> Nothing)

conts :: (a -> Cont c b) -> [a] -> Cont c [b]
conts g sl c =
    let co al [] = c (reverse al)
        co al (s : sl') = g s (\a -> co (a : al) sl')
    in  co [] sl

tryM :: Cont c (Maybe a) -> c -> Cont c a
tryM e errc c = e $ \ov ->
                case ov of
		   Nothing -> errc
		   Just v -> c v

tryGet :: Cont c (Maybe a) -> (Cont c a) -> Cont c a
tryGet e errc c = tryM e (errc c) c

dropSP expected c = dropit where
    dropit =
      getSP $ \msg ->
      case expected msg of
	Just m -> c m
	Nothing -> dropit

contMap op = m
  where m = get $ \ x -> op x $ \ y -> put y $ m
-- or:  where m = get $ flip op $ flip put m


fContWrap :: Cont (FSP hi ho) a -> Cont (F hi ho) a
fContWrap waitsp = F{-ff-} . waitsp . fContSP
  where fContSP contF x = case contF x of F sp -> sp

kContWrap :: Cont (KSP hi ho) a -> Cont (K hi ho) a
kContWrap waitsp = K{-kk-} . waitsp . kContSP
  where kContSP contK x = case contK x of K sp -> sp
