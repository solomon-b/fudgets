module DialogueIO(Request(..), Response(..), IOError(..)
       , Dialogue(..), SigAct(..) , dialogueToIO
       --, module _LibDialogue
	) where
import Prelude hiding (IOError)
import P_IO_data
import DoRequest
import Control.Concurrent.Chan

-- | Included just to illustrate that it is possible to convert a Dialogue
-- IO function to a monadic IO function. The implementation relies on
-- 'getChanContents' to construct the lazy list of responses needed by
-- the dialogue IO function.
dialogueToIO :: Dialogue -> IO ()
dialogueToIO f =
  do st <- initXCall
     respchan <- newChan
     reqs <- f <$> getChanContents respchan
     let doReq req = writeChan respchan =<< doRequest st req
     mapM_ doReq reqs
