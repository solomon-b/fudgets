{-# LANGUAGE CPP #-}
module AsyncTransmitter(asyncTransmitterF,asyncTransmitterF',closerF) where
import Sockets
import Srequest
import FRequest
import Message(message)
import NullF(getK,nullK,nullF)
import Fudget()
import FudgetIO
import CompOps((>==<))
import IoF(ioF)
import DialogueIO hiding (IOError)
import qualified PackedString as PS
import Queue(QUEUE,empty,enter,qremove)

{- asyncTransmitterF has two states:

   - Idle state: we have nothing to send and don't want to be notified when
                 the socket becomes writable.

   - Blocked state: we have something to send and want to be notified when
                   the socket becomes writable.

   idleK handles the idle state. Transistion to the idleState is done
   with goIdleK. idleK should only be called when in the idle state.

   blockedK handles the blocked state. Transition to the blocked state is done
   with goBlockedK. blockedK should only be called when in the blocked state.

   writeK is called in blocked mode when the socket become writable. If there
   is something left in the buffer to write, writeK writes a chunk to the
   socket and continues in blocked mode, otherwise writeK switches to
   idle mode.
-}

asyncTransmitterF socket = closerF socket >==< asyncTransmitterF' socket

asyncTransmitterF' socket =
    -- Start in idle mode
    ioF idleK
  where
    -- To be called while in idle mode only:
    idleK = getMsg (const idleK) high
      where
	high "" = closeK
	high str = goBlockedK (buf1 str)

    -- To swich from blocked to idle mode:
    goIdleK = select [] $ idleK

    -- To switch from idle mode to blocked mode:
    goBlockedK buf =
      select [OutputSocketDe socket] $
      blockedK buf initblocksize

    -- To be called while in blocked mode only:
    blockedK buf n = getMsg low high
      where
        low (DResp (AsyncInput (_,SocketWritable))) = writeK buf n
	high str = blockedK (putbuf buf str) n

    -- To be called while in blocked mode, when socket becomes writable:
    writeK buf n =
      case getbuf buf n of
        Empty -> goIdleK
	EoS   -> closeK
	More ps buf' -> sIOerr (WriteSocketPS socket ps) errK okK
	  where
	    okK (Wrote n')  = blockedK buf' n
	    errK _          = blockedK buf' n -- Ignore errors?!

    closeK =
      select [] $
      --sIOsucc (CloseSocket socket) $
      -- Can't close the socket until the receiver (if any) has deselected it!
      putHigh () $
      nullK

closerF socket =
  getHigh $ \ _ ->
  sIOsucc (CloseSocket socket) $
  nullF

getMsg l h = getK $ message l h

data Buffer = Buf String (QUEUE String)
data GetBuf = More PS.PackedString Buffer | Empty | EoS

--buf0 = Buf "" empty
buf1 str = Buf str empty -- pre: str/=""

putbuf (Buf s q) str = Buf s (enter q str) -- str=="" to indicate eos

getbuf (Buf s q) n = getbuf' s q
  where
    getbuf' "" q =
      case qremove q of
        Nothing -> Empty
	Just (s,q') ->
	  if null s
	  then EoS
	  else getbuf'' s q'

    getbuf' s q = getbuf'' s q

    getbuf'' s q =
      case splitAt n s of
        (s1,s2) -> More (PS.packString s1) (Buf s2 q)


initblocksize = 512 :: Int

#if defined(__GLASGOW_HASKELL__) || defined(__PFE__)
nullPS = PS.nullPS
#else
nullPS = PS.null
#endif
