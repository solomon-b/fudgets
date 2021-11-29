module QuitK where
import Command
import Event
import Fudget
--import FudgetIO
import FRequest
import Xcommand
import HaskellIO(haskellIO)
import InternAtom
--import Message(Message(..))
import NullF
import Spops(nullSP)
import CompFfun(postProcessHighK,preProcessHighK)
--import Sockets
import Xtypes
import DialogueIO hiding (IOError)

quitK action =
   nullSP `postProcessHighK`
   wmK (Just action)
   `preProcessHighK` nullSP

-- (*) wmK always enables the WM_DELETE_WINDOW protocol, since some window
-- managers provide a close button that destroy the window if the protocol
-- is disabled. Sigh.

wmK optAction =
    wmDeleteWindowK $ \dw -> -- (*)
    case optAction of
      Just action ->
        -- wmDeleteWindowK $ \dw -> -- (*)
    	internAtomK "WM_PROTOCOLS" False $ \ pr ->
    	wmK' (lowHandler action pr dw)
      Nothing -> wmK' (const id)
  where
    lowHandler action pr dw event =
      case event of
	XEvt (ClientMessage a (Long (p : _))) | a == pr && Atom (fromIntegral p) == dw ->
	  action 
        _ -> id

    wmK' lowHandler = loop
      where
	loop =
	  getK $ \msg ->
	  case msg of
	    Low event -> lowHandler event loop
	    High (Left title) -> xcommandK (StoreName title) loop
	    High (Right True) -> xcommandK MapRaised loop
	    High (Right False) -> unmapWindowK loop
	    _ -> loop

-- Some handlers:
exitK cont = haskellIO (Exit 0) (const nullK)
unmapWindowK = xcommandK UnmapWindow
reportK = putK (High ())

wmDeleteWindowK cont =
    internAtomK "WM_DELETE_WINDOW" False $ \dw ->
    xcommandK (SetWMProtocols [dw]) $
    cont dw
