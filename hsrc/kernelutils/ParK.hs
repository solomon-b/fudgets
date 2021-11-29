module ParK(parK,compK) where
--import Command(Command(..))
import CompSP
--import Event(Event(..))
import Fudget
--import Message(Message(..))
--import Path(Path(..))
--import SP
import Spops
import IsRequest
import CompFfun
import SpEither


parK :: K a b -> K a b -> K a b
parK (K l) (K r) = K{-kk-} (parKSP l r)

parKSP :: KSP a b -> KSP a b -> KSP a b
parKSP l r = pkp [] (l `compEitherSP` r) where
   pkp routeq k = pk routeq (pullSP k)
   pk routeq (os,k) = pos os where
      pos os =
        case os of
         [] ->
	   getSP $ \msg ->
	   case msg of
	    Low e | isResponse e ->
	      case routeq of
	        [] -> pos [] -- response without request?
		r:routeq -> feed routeq [r msg]
            _ -> feedr [Left msg, Right msg]
           where feed routeq l = pkp routeq (startupSP l k)
		 feedr = feed routeq
	 o:os -> case o of
		   Right (High a) -> out (High a)
		   Left (High a) -> out (High a)
		   Right (Low c) -> lowout c Right
		   Left (Low c) -> lowout c Left
           where
             lowout c route =
	       putSP (Low c) $ 
	       if isRequest c
	       then pk (routeq++[route]) (os,k)
	       else pos os
             out m = putSP m $ pos os

compK :: K a b -> K c d -> K (Either a c) (Either b d)
compK (K l) (K r) = K{-kk-} (compKSP l r)

compKSP :: KSP a b -> KSP c d -> KSP (Either a c) (Either b d)
compKSP l r = parKSP (Left `postMapHigh'` l `preProcessHigh'` filterLeftSP)
		     (Right `postMapHigh'` r `preProcessHigh'` filterRightSP)
