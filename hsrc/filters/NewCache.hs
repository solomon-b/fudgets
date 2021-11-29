module NewCache(allcacheF) where
import Command
--import Event
import FRequest
--import Fudget
--import Loopthrough
import Spops
import LoopLow
import Cont
import IsRequest
--import DialogueIO hiding (IOError)
import qualified Data.Map as OM

--import Maptrace(ctrace) -- debug
--import NonStdTrace(trace)

-- A new implementation of the X resource cache.
-- TODO: reference counters and the ability to free unused resources.

allcacheable xreq =
  case xreq of
    LoadFont fn -> True
    QueryFont f -> True
    LoadQueryFont s -> True
    ListFonts pat max -> True
    ListFontsWithInfo pat max -> True
    CreateGC d t as -> True
    -- FreeGC gcid 
    AllocNamedColor cm cn -> True
    AllocColor cm rgb -> True
    -- FreeColors cm [pixel] (Pixel 0)
    CreateFontCursor shape -> True
    ReadBitmapFile name -> True
    CreateBitmapFromData bdata -> True -- hmm, big request, small result...
    _ -> False

allcacheF = cacheF allcacheable

cacheF cacheable fud = loopThroughLowF (cc OM.empty) fud where
  cc table = same where
     same = getSP cachehandle
     cachehandle msg = case msg of 
       Left tc@(tag,c) ->
	   case c of
	     XReq xreq ->
	       if cacheable xreq
	       then case OM.lookup xreq table of
		      Just r -> putSP (Right (tag,r)) $
				cc table
		      Nothing -> waitresp $ \r ->
				 --ctrace "trcache" ("alloc",c,d,r) $
				 cc (OM.insert xreq r table)
	       else waitresp $ \r -> same
	     _ -> if isRequest c
		  then waitresp $ \r -> same
		  else psame
         where waitresp c = cmdContSP (Left tc)
		 (\msg->case msg of Right te@(_,e) | isResponse e -> Just te
				    _ -> Nothing) $ \tr@(_,r) ->
		 putSP (Right tr) $ c r
       Right _ -> psame
      where
       pass = putSP msg
       psame = pass same
