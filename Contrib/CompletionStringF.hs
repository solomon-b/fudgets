module CompletionStringF
    (stdcc,completeFromList,
     completionStringF,completionStringF',completionStringF'')
  where
import Fudgets
import Data.List(isPrefixOf) 
import HbcUtils(mapSnd)

completionStringF = completionStringF' stdcc standard

stdcc = argReadKey "stdcc" ' ' -- standard completion char

completionStringF' cc cust = completionStringF'' cc cust >=^< mapEither id Right

completionStringF'' cc cust =  -- cc = completion character
    loopThroughRightF (absF completeSP0) (stringF'' cust)
  where
    completeSP0 = completeSP (const []) ([],[]) ""

    completeSP listfun updownlist current =
        getSP $ either fromStringF fromOutside
      where
        list = listfun current

        same = completeSP listfun updownlist current
	newList listfun' = completeSP listfun' ([],[]) current
	newString' p s = putSP (toOutput (InputChange s)) $
			 completeSP listfun p s
	newString = newString' ([],[])


        toStringF'' = Left
        toString = toStringF'' . Right
	toCustomiser = toStringF'' . Left
	toOutput = Right . Right
	toCompletionList = Right . Left

	fromOutside = either newList inputToStringF''
	inputToStringF'' msg = putSP (toStringF'' msg) same

        fromStringF msg = 
	  case msg of
	    InputDone "Up"   _ -> goto above
	    InputDone "Down" _ -> goto below
	    InputDone "Tab"  _ -> doCompletion
	    InputDone _ _ -> putSP (toOutput msg) same
	    InputChange s ->
	      if s==current++[cc]
	      then doCompletion
	      else if fromupdownlist s
	           then same
		   else newString s -- erase completion list?

        fromupdownlist s =
 	    case updownlist of
	      (_,(_,s'):_) -> s==s'
	      _ -> False

        goto (_,[]) = same
        goto l@(_,item@(_,s):_) =
	    putSP (toString s) $
	    putSP (toCompletionList [item]) $
	    putSP (toOutput (InputChange s)) $
	    newString' l current

	above = case updownlist of
		  ([],[]) -> case reverse updownlist' of
			       x:xs -> (xs,[x])
			       _ -> ([],[])
		  (x:xs,ys) -> (xs,x:ys)
		  _ -> updownlist

        below = case updownlist of
		  ([],[]) -> ([],updownlist')
		  (xs,x1:x2:ys) -> (x1:xs,x2:ys)
		  _ -> updownlist

        updownlist' = mapSnd (current++) list

        doCompletion =
	    putSP (toCompletionList list) $
	    putNewString (current++commonPrefix (map snd list))

        putNewString new =
	  putSP (toString new) $
	  putSP (toCustomiser (setCursorPos (length new))) $
	  newString new

commonPrefix ((c:s):ss) =
  case filter ((/=[c]).take 1) ss of
    [] -> c:commonPrefix (s:map tail ss)
    _ -> []
commonPrefix _ = []

pos y xs =
  case [ix|ix@(i,x)<-number 0 xs,y `isPrefixOf` x] of
    []      -> (0,False)
    (i,x):_ -> (i,x==y)

completeFromList list current =
    [(current,compl)|(pre,compl)<-splits,pre==current]
  where
    splits = map (splitAt n) list
    n = length current
