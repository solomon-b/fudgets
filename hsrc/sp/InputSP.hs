module InputSP where
import InputMsg
import Spops
import CompSP(serCompSP)
import SpEither(mapFilterSP)
import Utils(replace,setFst,setSnd)

-- New version: works with abstract InputMsg.
inputPairSP = mapFilterSP lift `serCompSP` ipSP (Nothing,Nothing)
  where
    ipSP optvalues = getSP $ either (change setFst) (change setSnd)
      where
	change setOne inputmsg =
	    putSP (mapInp (const optvalues') inputmsg) $
	    ipSP optvalues'
	  where
	    optvalues' = setOne optvalues (Just $ stripInputMsg inputmsg)

    lift = liftMaybeInputMsg . mapInp liftMaybePair

    liftMaybePair (Just x,Just y) = Just (x,y)
    liftMaybePair _               = Nothing

    liftMaybeInputMsg m = fmap im (stripInputMsg m)
      where im x = mapInp (const x) m

{- -- old version:
inputPairSP = ipSP Nothing Nothing
  where
    ipSP optx opty =
        getSP $ \msg ->
          case msg of
	    Left (InputChange x) -> changeL InputChange x
	    Left (InputDone k x) -> changeL (InputDone k) x
	    Right (InputChange y) -> changeR InputChange y
	    Right (InputDone k y) -> changeR (InputDone k) y
      where
        changeL f x =
            case opty of
	      Just y -> putsSP [f (x,y)] cont
	      Nothing -> cont
	  where cont = ipSP (Just x) opty
        changeR f y =
            case optx of
	      Just x -> putsSP [f (x,y)] cont
	      Nothing -> cont
	  where cont = ipSP optx (Just y)
-}

inputListSP tags = ilSP [(tag,Nothing)|tag<-tags]
  where
    ilSP acc =
        getSP $ \(t,msg) ->
          case msg of
	    InputChange x -> change t InputChange x
	    InputDone k x -> change t (InputDone k) x
      where
        change t f x = putsSP [f [(t,x)|(t,Just x)<-acc']] (ilSP acc')
	  where acc' = replace (t,Just x) acc


stripInputSP = mapFilterSP notLeave
  where 
    notLeave (InputChange s) = Just s
    notLeave (InputDone k s) = if k == inputLeaveKey
                               then Nothing
			       else Just s

inputDoneSP = mapFilterSP inputDone
inputLeaveDoneSP = mapFilterSP inputLeaveDone
