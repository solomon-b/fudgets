module LayoutDoNow where

import Fudget
--import Command
import FRequest
--import Event
--import Message
import Spops
--import SP(SP)
import Path(here)
import LayoutRequest
import IsRequest

layoutDoNow (F sp) = F{-ff-} (layoutDoNow' sp)

layoutDoNow' f = donow 0 (pullSP f) where
  donow pendingreqs (os,f) = 
      putsSP os $ 
      let n' = pendingreqs + newreqs
          newreqs = length (filter isReq os)
	  isReq (Low (_,c)) = isRequest c
	  isReq _ = False
	  nResp (Low (_,e)) | isResponse e = 1
	  nResp _ = 0
      in if n' == 0 then putSP (Low (here,LCmd LayoutDoNow)) f
	 else 
      getSP $ \msg -> donow (n' - nResp msg) (walkSP f msg)
