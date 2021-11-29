module OpenSocket(
  openSocketF,openSocketErrF,
  openLSocketF,openLSocketErrF,
  openFileAsSocketF,openFileAsSocketErrF
  ) where
import Srequest
import Sockets

-- All eta expanstion are due to the monomorphism restriction

openSocketF h = openSocketF' sIO h
openLSocketF p = openLSocketF' sIO p
openFileAsSocketF h = openFileAsSocketF' sIO h

openSocketErrF h p e = openSocketF' (sIOerr' e) h p
openLSocketErrF p e = openLSocketF' (sIOerr' e) p
openFileAsSocketErrF n m e = openFileAsSocketF' (sIOerr' e) n m

openSocketF' sio host port cont =
  sio (OpenSocket host port) $ \ (Socket socket) -> cont socket

openLSocketF' sio port cont =
  sio (OpenLSocket port) $ \(LSocket lsocket) -> cont lsocket

openFileAsSocketF' sio name mode cont =
  sio (OpenFileAsSocket name mode) $ \ (Socket socket) -> cont socket

sIOerr' e r = sIOerr r e
