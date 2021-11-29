module Xrequest(xrequest,xrequestF, xrequestK,Cont(..)) where
--import Command(XRequest)
--import Event(XResponse)
import FRequest
import FudgetIO
import EitherUtils(Cont(..))
import NullF(F,K)
--import DialogueIO hiding (IOError)

xrequestK = xrequest :: (XRequest -> (XResponse -> Maybe a) -> Cont (K b c) a)
xrequestF = xrequest :: (XRequest -> (XResponse -> Maybe a) -> Cont (F b c) a)

xrequest xreq expected = cmdContLow (XReq xreq) expectXResp
  where expectXResp msg =
          case msg of
            XResp xr -> expected xr
            _ -> Nothing

{- old:
xrequestK = xrequest cmdContK

xrequestF = xrequest cmdContF

xrequest k cmd exp' =
    k (DoXRequest cmd)
      (\msg ->
       case msg of
         IOResponse (XResponse xr) -> exp' xr
         _ -> Nothing)

-}
