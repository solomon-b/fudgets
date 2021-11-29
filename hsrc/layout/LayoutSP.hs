module LayoutSP(layoutMgrF,dynLayoutMgrF) where
--import Command
import Data.List(sortBy)
--import Direction
--import Event
import FRequest
import Fudget
import CompOps((>=^<))
import Geometry(Rect,rR)
import LayoutRequest
import Path(here,showPath{-,Path(..),subPath-})
import Spops
--import EitherUtils(mapMaybe)
--import Utils(number, replace)
import HbcUtils(apFst)
--import Xtypes
--import NonStdTrace(trace)
--import CmdLineEnv(argKey)
import LayoutF(LayoutDirection(..))
import Maptrace(ctrace)

default (Int)

mytrace x = ctrace "layoutftrace" x

layoutMgrF :: Int -> LayoutDirection -> Placer -> F (Path, LayoutMessage) (Path, Rect)
layoutMgrF fudgetCnt dir lter1 = dynLayoutMgrF fudgetCnt dir lter1 >=^< Left

dynLayoutMgrF :: Int -> LayoutDirection -> Placer -> F (Either (Path, LayoutMessage) (Int,Bool)) (Path, Rect)
dynLayoutMgrF fudgetCnt0 dir (P lter1) = F{-ff-} $ getNLimits fudgetCnt0 []
  where
   sortTags = sortBy (order dir)
     where
       order Forward = ofst compare
       order Backward = ofst (flip compare)
       ofst r (x,_) (y,_) = r x y
   getNLimits 0 l = doLter1 Nothing $ sortTags l
   getNLimits n l =
    let same = getNLimits n l in
    getSP $ \msg -> case msg of
      High (Left (path,lmsg)) ->
        case lmsg of
	 LayoutRequest lr -> getNLimits (n-1) ((path,lr):l)
	 _ -> putSP (Low (path,LCmd lmsg)) same -- !!!
      High (Right (dyn,created)) ->
        if created
	then getNLimits (n+1) l
	else mytrace "fudget destroyed during getNLimits in layoutMgrF" $
	     --let l' =
	     -- if {-already received layout request from the destoyed fudget-}
	     -- then {-remove it from l-}
	     -- else l
	     -- in getNLimits (n-1) l'
	     same
      Low _ -> mytrace "unexpected event in getNLimits in layoutMgrF" $
               same
   doLter1 oplace slims =
     let (req,lter2) = lter1 (map snd slims)
     in mytrace ("req is"++show req) $ 
        putSP (Low (here,layoutRequestCmd req)) $
	mytrace ("enter loop with "++show (length slims)) $
	loop lter2 slims oplace
   loop lter2 slims oplace =
    let same = loop lter2 slims oplace
    in
    getSP $ \msg -> case msg of
      High (Left (path,LayoutRequest lr)) ->
	 case upd slims path lr of
	   Nothing -> case (oplace >>= \place ->
			    flip lookup (zip (map fst slims) (snd place)) path) of
			Nothing -> same
			Just r -> putSP (High (path,r)) same
	   Just slims' -> mytrace ("reenter: "++show (length slims')) $
	                  doLter1 (fmap (apFst (const $ rR 0 0 0 0)) oplace) slims'
         where upd slims path lr =
	         try path Nothing $
		 mytrace ("lF: trying subPath"++
		          show(path,slims,longesteq path (map fst slims)::Path)) $
		 try (longesteq path (map fst slims)) (Just path) Nothing
                 where try path orepl fail = u slims []
			  where u [] _ = fail
				u (pl@(path',lr'):rest) l = 
				  let nslims p = Just (reverse l ++ ((p,lr):rest)) in
				  if path'==path then case orepl of
				       Nothing -> {-if lr == lr' then Nothing else -} nslims path
				       Just repl -> nslims repl

				  else u rest (pl:l)
      High (Left (path,lr)) -> putSP (Low (path,LCmd lr)) same -- !!!
      High (Right (dyn,created)) ->
        if created
	then getNLimits 1 slims
	else mytrace "fudget destroyed in loop in layoutMgrF" $
	     -- remove it!
	     same
      Low (path,LEvt (LayoutPlace r)) -> mytrace ("Layoutplace "++showPath path++","++show r) $
	case oplace of 
	  Just (r',_) | r == r' -> mytrace ("lF: same rect "++show r) same
	  _ -> let rects = lter2 r
		   slims' = slims {-map (\((path,Layout s v h),Rect p s') ->
				   mytrace (show (showPath path,s,s'))$(path,Layout s' v h)) $ zip slims rects-}
		   paths = map fst slims
		   crects = zip paths rects {- case oplace of
		    Nothing -> zip paths rects
		    Just (_,orects) -> 
			   [(path,r) | 
			    (path,r,r') <- zip3 paths rects orects, r /= r']-}
	       in --mytrace (show$slims'==slims') $
		  putsSP [mytrace ("putsSP "++show (showPath path,r))$High pr | pr@(path,r) <- crects] $
		  loop (snd $ lter1 (map snd slims')) slims' (Just (r,rects))
      Low _ -> mytrace "unexpected event in loop in layoutMgrF" $
               same

--{-
--This is a fix for a problem with dynF, I (th) suppose. I think you can fix it in dynF instead.
begineqlen x = eq 0 x where
  eq n (x:xs) (y:ys) | x == y = eq (n+1) xs ys
  eq n _ _ = n

longesteq p1 (p:ps) = le (p1,begineqlen p1 p) ps where
  le (pm,l) [] = pm
  le (pm,l) (p:ps) = let len = begineqlen p1 p
                         pl1 = if len > l then (p,len) else (pm,l)
		     in le pl1 ps
-- -}
