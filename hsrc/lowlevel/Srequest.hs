module Srequest(select,sIOsucc,sIOstr,sIOerr,sIO,Cont(..)) where
import FRequest
import FudgetIO
import EitherUtils(Cont(..))
--import NullF(F,K)
import DialogueIO hiding (IOError) -- Select
import ShowFailure

sIOsucc sreq cont =
  socketIO sreq sFail $ \ r ->
  case r of
    Left Success -> cont
    Left r -> error ("sIOsucc: Expected Success, but got "++show r)
    Right r -> error ("sIOsucc: Expected Success, but got "++show r)

sIO sreq = sIOerr sreq sFail

sIOerr sreq fcont rcont =
    socketIO sreq fcont $ \ r ->
    case r of
      Right sr -> rcont sr
      Left dr -> error ("Socket IO: expected a SocketResponse, but got "++show dr)

sIOstr sreq cont =
  socketIO sreq sFail $ \ r ->
  case r of
    Left (Str s) -> cont s
    Left r -> error ("sIOsucc: Expected Str, but got "++show r)
    Right r -> error ("sIOsucc: Expected Str, but got "++show r)
    
---

sFail f = error ("Socket IO error: "++showFailure f)

socketIO sreq econt scont =
    cmdContLow (SReq sreq) expect $ either econt scont
  where expect msg =
          case msg of
            SResp sr -> Just (Right (Right sr))
	    DResp (Failure f) -> Just (Left f)
	    DResp r -> Just (Right (Left r))
            _ -> Nothing

----

select ds = putLow . DReq . Select $ ds
 -- no response from Select
-- eta expanded because of the monomorphism restriction
