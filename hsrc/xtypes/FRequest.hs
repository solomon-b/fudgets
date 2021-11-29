-- | Fudget low-level message types
module FRequest(
  module FRequest,
  XCommand,XEvent,XRequest,XResponse,
  SocketRequest,SocketResponse,
  LayoutMessage,LayoutResponse
  --AsyncInput(..)
) where
import Command(XCommand,XRequest)
import Event(XEvent,XResponse)
import Sockets(SocketRequest,SocketResponse{-,AsyncInput-})
import DialogueIO(Request,Response)
import LayoutRequest(LayoutMessage(..),LayoutResponse)

data FRequest
  = XCmd XCommand
  | LCmd LayoutMessage
  -- asynchronous above, synchronous below, but see ../internal/IsRequest.hs
  | XReq XRequest
  | SReq SocketRequest
  | DReq Request
  deriving Show

data FResponse
  = XEvt  XEvent
  | LEvt  LayoutResponse
--  | SEvt  AsyncInput -- still represented as DResp (AsyncInput ...)
  -- asynchronous above, synchronous below, but see ../internal/IsRequest.hs
  | XResp XResponse
  | SResp SocketResponse
  | DResp Response
  deriving Show

layoutRequestCmd = LCmd . LayoutRequest
