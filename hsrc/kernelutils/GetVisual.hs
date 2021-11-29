module GetVisual where
import Command(XRequest(..))
import Event(XResponse(..))
import Xrequest(xrequest)

defaultVisual cont = xrequest DefaultVisual gotit cont
  where gotit (GotVisual v) = Just v
        gotit _ = Nothing

