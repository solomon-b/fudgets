module LoopLow(loopLow,loopThroughLowSP,loopThroughLowF) where
--import Command(Command(..))
import CompSP
--import Event(Event(..))
import Fudget
import Loop
--import Message(Message(..))
--import Path(Path(..))
--import SP
import Loopthrough(loopThroughRightSP)

loopLow :: (SP TCommand (FCommand a)) -> (SP (FEvent a) TEvent) -> (F b c) -> F b c
loopLow ch eh (F f) =
    let prep (Left msg) = Low (High msg)
        prep (Right (High msg)) = High msg
        prep (Right (Low msg)) = Low (Low msg)
        post (High msg) = Right (High msg)
        post (Low (High msg)) = Left msg
        post (Low (Low msg)) = Right (Low msg)
    in F{-ff-} $
	loopLeftSP
	  (prepostMapSP prep post	
		        (idHighSP ch `serCompSP` f `serCompSP` idHighSP eh))

loopThroughLowSP :: SP (Either c e) (Either c e) -> 
		    SP (Message e a) (Message c b) ->
 	            SP (Message e a) (Message c b)

loopThroughLowSP ctrl f =
    loopThroughRightSP (prepostMapSP prep post (idHighSP ctrl)) f
  where
    prep msg = case msg of
		 Right (High a) -> High (Left a)
		 Left (High a) -> High (Right a)
		 Right (Low e) -> Low (Right e)
		 Left (Low c) -> Low (Left c)

    post msg = case msg of
		 High (Left msg) -> Left (High msg)
		 High (Right msg) -> Right (High msg)
		 Low (Left c) -> Right (Low c)
		 Low (Right e) -> Left (Low e)

loopThroughLowF
  :: SP (Either TCommand TEvent) (Either TCommand TEvent) -> F i o -> F i o
loopThroughLowF ctrlSP (F fudSP) = F $ loopThroughLowSP ctrlSP fudSP
