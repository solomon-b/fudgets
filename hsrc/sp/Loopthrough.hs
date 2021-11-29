module Loopthrough(loopThroughRightSP) where
import SP
--import Spops
import Queue

loopThroughRightSP sp1 sp2 = ltrSP empty sp1 sp2

-- When sp1 and sp2 are unknown:
ltrSP q sp1 sp2 =
    case sp1 of
      PutSP (Right out) sp1' -> PutSP out (ltrSP q sp1' sp2)
      PutSP (Left loop') sp1' -> ltrSP (enter q loop') sp1' sp2
      GetSP xsp1 -> ltrSP1 q xsp1 sp2
      NullSP -> NullSP

-- When sp1 is waiting for input:
ltrSP1 q xsp1 sp2 =
    case sp2 of
      PutSP x sp2' -> ltrSP q (xsp1 (Left x)) sp2'
      GetSP xsp2 ->
	case qremove q of
	  Just (x,q') -> ltrSP1 q' xsp1 (xsp2 x)
	  Nothing -> GetSP (\x -> ltrSP2 (xsp1 (Right x)) xsp2)
      NullSP -> GetSP (lltrSP . xsp1 . Right)

-- When sp2 is waiting for input:
ltrSP2 sp1 xsp2 =
    case sp1 of
      PutSP (Right out) sp1' -> PutSP out (ltrSP2 sp1' xsp2)
      PutSP (Left loop') sp1' -> loopThroughRightSP sp1' (xsp2 loop')
      GetSP xsp1 -> GetSP (\x -> ltrSP2 (xsp1 (Right x)) xsp2)
      NullSP -> NullSP

-- When sp2 has terminated:
lltrSP sp1 =
    case sp1 of
      PutSP (Right out) sp1' -> PutSP out (lltrSP sp1')
      PutSP (Left loop') sp1' -> lltrSP sp1'
      GetSP xsp1 -> GetSP (lltrSP . xsp1 . Right)
      NullSP -> NullSP

{- old (inefficient queueing and too strict in sp2):

loopThroughRightSP sp1 sp2 =
    case sp1 of
      PutSP (Right out) sp1' -> PutSP out (loopThroughRightSP sp1' sp2)
      PutSP (Left loop') sp1' -> case sp2 of
                                  GetSP xsp2 -> loopThroughRightSP sp1'
                                                                   (xsp2 loop')
                                  NullSP -> lltrSP sp1'
                                  _ -> loopThroughRightSP sp1'
                                                          (feedSP' loop' [] sp2)
      GetSP xsp1 -> ltrSP1 xsp1 sp2
      NullSP -> NullSP

ltrSP1 xsp1 sp2 =
    case sp2 of
      PutSP x sp2' -> loopThroughRightSP (xsp1 (Left x)) sp2'
      GetSP xsp2 -> GetSP (\x -> ltrSP2 (xsp1 (Right x)) xsp2)
      NullSP -> GetSP (lltrSP . xsp1 . Right)

ltrSP2 sp1 xsp2 =
    case sp1 of
      PutSP (Right out) sp1' -> PutSP out (ltrSP2 sp1' xsp2)
      PutSP (Left loop') sp1' -> loopThroughRightSP sp1' (xsp2 loop')
      GetSP xsp1 -> GetSP (\x -> ltrSP2 (xsp1 (Right x)) xsp2)
      NullSP -> NullSP

lltrSP sp1 =
    case sp1 of
      PutSP (Right out) sp1' -> PutSP out (lltrSP sp1')
      PutSP (Left loop') sp1' -> lltrSP sp1'
      GetSP xsp1 -> GetSP (lltrSP . xsp1 . Right)
      NullSP -> NullSP

-- normal code
feedSP' = feedSP

-}
