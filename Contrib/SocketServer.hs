module SocketServer(ClientMsg(..),SocketMsg(..),mapSocketMsg,socketServerF) where
import AllFudgets
import DialogueIO hiding (IOError)

data ClientMsg a = ClientMsg a | ClientEOS | ClientNew deriving (Show)
data SocketMsg a = SocketMsg a | SocketEOS deriving (Show)

mapSocketMsg f (SocketMsg a) = SocketMsg (f a)
mapSocketMsg f SocketEOS = SocketEOS

instance Functor SocketMsg where fmap = mapSocketMsg

socketServerF port f = 
    loopThroughRightF (concatMapF router) (listenerF >+< dynListF)
  where
    router = either (either fromListener fromDynList) fromOutside
      where
	fromListener (i,f) = [todyn (i,DynCreate f), out (i,ClientNew)]

	fromDynList (i,m) =
	  case m of
	    SocketMsg m' -> [out (i,ClientMsg m')]
	    SocketEOS    -> [out (i,ClientEOS), todyn (i,DynDestroy)]

	fromOutside (i,m) = [todyn (i,DynMsg m)]

        todyn = Left . Right
	out = Right

    listenerF =
        openLSocketF port $ \lsocket ->
	select [LSocketDe lsocket] $
	accepter 0
      where
	accepter i = 
	  getMessageFu $ \e ->
	  case e of
	    Low (DResp (AsyncInput (_,SocketAccepted socket peer))) ->
		  putF (i,f socket peer) $
		  accepter (i+1)
	    _ -> accepter i
