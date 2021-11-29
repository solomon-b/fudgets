module Process where
--import Fudget
--import Xtypes
import Srequest
import NullF()
--import FudgetIO
import Sockets
import Transceivers
import CompOps
--import DialogueIO hiding (IOError)

subProcessF cmd =
  sIO (StartProcess cmd True True True) $
     \ (ProcessSockets (Just sin) (Just sout) (Just serr)) ->
     (receiverF sout>+<receiverF serr)>==<transmitterF sin
