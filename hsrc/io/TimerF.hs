module TimerF(timerF,Tick(..)) where
import Fudget
import FudgetIO
import FRequest
import Srequest(sIO,sIOsucc,select)
--import Message(Message(..))
import NullF
import Sockets
--import Xtypes
import DialogueIO hiding (IOError)

data Tick = Tick deriving (Show,Eq)

--timerF :: F (Maybe (Int,Int)) Tick
timerF =  f Nothing
  where
    f oTno =
      getMessageFu $ \ msg ->
      let same = f oTno
	  settimer int first = case oTno of
	    Just _ -> same
	    Nothing ->
	      sIO (CreateTimer int first) $ \ (Timer tno) ->
	      -- error handling?!
	      select [TimerDe tno] $
	      f (Just tno)
	  removetimer = case oTno of
		Just tno -> sIOsucc (DestroyTimer tno) $
			    select [] $
			    f Nothing
		Nothing -> same
      in case msg of
	   High (Just (interval, first)) -> settimer interval first
	   High Nothing -> removetimer
	   Low (DResp (AsyncInput (_, TimerAlarm))) ->
	     putHigh Tick $
	     same
