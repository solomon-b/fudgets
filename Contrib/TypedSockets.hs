-- | Type-safe network sockets, as described in
-- [Client/Server Applications with Fudgets](http://www.altocumulus.org/Fudgets/ftp/client-server.pdf).
module TypedSockets(TPort,tPort,tSocketServerF,TServerAddress,tServerAddress,tTransceiverF,ClientMsg(..),SocketMsg(..)) where

import Fudgets
import SocketServer
import Debug.Trace
--import DialogueIO hiding (IOError)

newtype TPort a b = TPort Port

tPort :: (Show a, Read a,Show b,Read b) => Port -> TPort a b
tPort p = TPort p

newtype TSocket t r = TSocket Socket
newtype TLSocket t r = TLSocket LSocket
data TServerAddress c s = TServerAddress Host (TPort c s)

tServerAddress host port = TServerAddress host port

tSocketServerF ::
  (Read c, Show s) => 
  TPort c s -> (Peer -> F s (SocketMsg c) -> F a (SocketMsg b)) ->
  F (Int, a) (Int, ClientMsg b)
tSocketServerF (TPort p) f = socketServerF p (\ s p -> f p (textTransceiver s))

--texttransceiver :: (Show t,Read r) => Socket -> F t (SocketMsg r)
textTransceiver s = postSP >^^=< transceiverF s >=^< pre
  where
    pre s = shows s "\n"

    postSP = prepostMapSP eos stripEither $
             idLeftSP (mapFilterSP reader -==- inputLinesSP)

    eos "" = Left SocketEOS
    eos s = Right s

    reader s = case reads s of 
      [(a,"")] -> Just (SocketMsg a)
      _ -> trace ("No parse from socket: "++s) Nothing

tTransceiverF :: (Show c, Read s) => TServerAddress c s -> F c (SocketMsg s)
tTransceiverF (TServerAddress host (TPort port)) = 
   openSocketF host port textTransceiver
