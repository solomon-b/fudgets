module HaskellIO{-(hIOSucc, hIOSuccK, hIOSuccF,
	         hIO, hIOK, hIOF,
	         hIOerr, hIOerrK, hIOerrF,
	         haskellIO, haskellIOK, haskellIOF)-} where
import Prelude hiding (IOError)
--import Command
--import Cont(cmdContF)
import FudgetIO
import NullF(F)
import FRequest
--import Event
--import Fudget
--import Message(Message(..))
import ShowFailure
--import Sockets
--import Xtypes
import DialogueIO

hIOSucc req = hIO req . const

hIOerr req fcont scont =
  haskellIO req $ \ resp ->
  case resp of
    Failure f -> fcont f
    _ -> scont resp

haskellIO request = cmdContLow (DReq request) expected
  where
    expected (DResp response) = Just response
--  expected (SResp sresp) = Just (SocketResponse sresp) -- for backward compat
    expected _ = Nothing

hIO req = hIOerr req (\e -> error ("IOError: " ++ showFailure e))

------

haskellIOF :: Request -> (Response -> F a b) -> F a b
haskellIOF = haskellIO

hIOerrF :: Request -> (IOError -> F a b) -> (Response -> F a b) -> F a b
hIOerrF = hIOerr

hIOF :: Request -> (Response -> F a b) -> F a b
hIOF = hIO

hIOSuccF :: Request -> (F a b) -> F a b
hIOSuccF = hIOSucc

{---

haskellIOK :: Request -> (Response -> K a b) -> K a b
haskellIOK = haskellIO

hIOerrK :: Request -> (IOError -> K a b) -> (Response -> K a b) -> K a b
hIOerrK = hIOerr

hIOK :: Request -> (Response -> K a b) -> K a b
hIOK = hIO

hIOSuccK :: Request -> (K a b) -> K a b
hIOSuccK = hIOSucc

---}
