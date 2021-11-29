module DialogueSpIO where
import System.IO(hFlush,stdout)
import SP
import DoRequest(initXCall,doRequest,getAsyncInput)
import DialogueIO(Request(XCommand,GetAsyncInput))
import Queue
import CmdLineEnv(argFlag)

--dialogueSpIO :: SP FResponse FRequest -> IO ()
{-
--Old, simple implementation:
dialogueSpIO sp =
    case sp of
      PutSP req sp' ->
        do resp <- doRequest req
	   dialogueSpIO (startupSP [resp] sp')
      GetSP xsp ->
        do resp <- doRequest GetAsyncInput
	   dialogueSpIO (xsp resp)
      NullSP -> return ()
-}

dialogueSpIO = if argFlag "dialogue-to-stdio" False
               then stdioDialogueSpIO
               else normalDialogueSpIO

normalDialogueSpIO = dialogueSpIO' initXCall doRequest getAsyncInput

stdioDialogueSpIO = dialogueSpIO' initXCall doRequest getAsyncInput
  where
    initXCall = return ()
    doRequest _ req = do print req
                         hFlush stdout
                         readLn
    getAsyncInput state = doRequest state GetAsyncInput

-- More efficient queueing of responses
dialogueSpIO' initXCall doRequest getAsyncInput sp = do
  iostate <- initXCall
  let doIO sp respq =
	case sp of
	  PutSP req sp' ->
	    do resp <- doRequest iostate req
	       case req of
		 -- The response to an XCommand is always Success
		 -- and is not propagated to the originating fudget.
	         XCommand {} -> doIO sp' respq
		 _ -> doIO sp' (enter respq resp)
	  GetSP xsp ->
	    case qremove respq of
	      Just (resp,respq') -> doIO (xsp resp) respq'
	      Nothing -> do resp <- getAsyncInput iostate
			    doIO (xsp resp) respq
	  NullSP -> return ()
  doIO sp empty
