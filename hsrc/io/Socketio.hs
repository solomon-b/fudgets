module Socketio where
import Fudget
import FudgetIO
import FRequest
import Srequest
--import Message(Message(..))
import NullF
import Sockets
import Utils(loop)
--import Xtypes
import DialogueIO hiding (IOError)

transmitterF' s =
  loop $ \l -> getF $ \str ->
  if null str
  then putHigh () nullF
  else sIOsucc (WriteSocket s str) l

receiverF' s =
    select [SocketDe s] $
    loop $ \l ->
          getMessageFu $ \e ->
           case e of
             Low (DResp (AsyncInput (_, SocketRead str))) ->
	       if null str
	       then closeDown
	       else putF str l
             High _ -> closeDown
  where
    closeDown =
      select [] $
      putHigh "" $
      sIOsucc (CloseSocket s) $
      nullF
