module Loop(loopLeftSP,loopSP,loopOnlySP) where
import SP
import Queue

-- New versions with explicit queues for improved efficiency:
-- (Adding a message to the loop queue is O(1) instead of O(n), n=queue length.)

loopLeftSP sp = llSP empty sp
  where
    llSP q sp =
      case sp of
	PutSP (Right out) sp' -> PutSP out (llSP q sp')
	PutSP (Left loop') sp' -> llSP (enter q loop') sp'
	GetSP xsp ->
	  case qremove q of
	    Just (loop',q') -> llSP q' (xsp (Left loop'))
	    Nothing -> GetSP (loopLeftSP.xsp.Right)
	NullSP -> NullSP

loopSP sp = lSP empty sp
  where
    lSP q sp =
      case sp of
	PutSP x sp' -> PutSP x (lSP (enter q x) sp')
	GetSP xsp ->
	  case qremove q of
	    Just (x,q') -> lSP q' (xsp x)
	    Nothing -> GetSP (loopSP.xsp)
	NullSP -> NullSP

loopOnlySP sp = loSP empty sp
  where
    loSP q sp =
      case sp of
	PutSP x sp' -> loSP (enter q x) sp'
	GetSP xsp ->
	  case qremove q of
	    Just (x,q') -> loSP q' (xsp x)
	    Nothing -> GetSP (loopOnlySP.xsp)
	NullSP -> NullSP


{--- old:

loopLeftSP sp =
    case sp of
      PutSP (Right out) sp' -> PutSP out (loopLeftSP sp')
      PutSP (Left loop') sp' -> loopLeftSP (feed1SP (Left loop') sp')
      GetSP xsp -> GetSP (loopLeftSP.xsp.Right)
      NullSP -> NullSP

loopSP sp =
  case sp of
    PutSP x sp' -> PutSP x (loopSP (feed1SP x sp'))
    GetSP xsp -> GetSP (loopSP.xsp)
    NullSP -> NullSP

loopOnlySP sp =
  case sp of
    PutSP x sp' -> loopOnlySP (feed1SP x sp')
    GetSP xsp -> GetSP (loopOnlySP.xsp)
    NullSP -> NullSP

feed1SP x sp =
    case sp of
      PutSP y sp' -> PutSP y (feed1SP x sp')
      GetSP xsp' -> xsp' x
      NullSP -> NullSP
-}
