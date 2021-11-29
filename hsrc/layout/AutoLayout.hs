{-# LANGUAGE CPP #-}
module AutoLayout(autoLayoutF,autoLayoutF',nowait) where
--import Prelude hiding (IO)
import LayoutRequest(LayoutMessage(..),LayoutResponse(..),LayoutRequest(minsize),LayoutHint,Spacer,Placer(..),Placer2,unS)
import LayoutDoNow
import PathTree hiding (pos)
import Geometry(Rect)
import Fudget
--import Spops
--import FudgetIO
import NullF(getK,putK,putsK) --,F,K
import Loops(loopThroughRightF)
import UserLayoutF
--import Xtypes
--import Event
--import Command
import FRequest
--import Path
import Direction
--import Placers
--import LayoutDir(LayoutDir)
--import CompOps
import IoF(ioF)
import CmdLineEnv(argFlag)
--import EitherUtils()
import Data.Maybe(isJust)
import HbcUtils(apFst,apSnd)
import Spacers(idS,compS,spacerP)
import AutoPlacer(autoP)
import SizingF
#ifdef __NHC__
import qualified Sizing
#else
import qualified Sizing(Sizing(..))
#endif
--import ContinuationIO(stderr)

-- debugging:
import StdIoUtil(echoStderrK)
--import NonStdTrace(trace)
--import Maptrace(ctrace)
--import SpyF

debugK :: String -> K hi ho -> K hi ho
debugK =
    if dbg
    then \ msg -> echoStderrK ("AutoLayout: "++msg)
    else const id
  where
    dbg = argFlag "ad" False

type LayoutTree = PathTree LayoutInfo

mapLT lf nf = mapPathTree (mapLayoutInfo lf nf)

top0 = Node (NodeInfo (Just "top",NoPlacerInfo)) Tip Tip

data LayoutInfo
  = NodeInfo NodeInfo
  | LeafInfo LeafInfo -- only in leaves
  deriving (Show)

mapLayoutInfo lf nf n = case n of
     NodeInfo n -> NodeInfo (nf n)
     LeafInfo l -> LeafInfo (lf l)

type LeafInfo = (LayoutRequest,Maybe Rect)
  -- (Layout s fh fv,Nothing) : received layout req, layout not computed
  -- (Layout s fh fv,Just rect) : rect is current placement.

type NodeInfo = ((Maybe LayoutHint), PlacerInfo)

data PlacerInfo =
   NoPlacerInfo |
   JustSpacer Spacer |
   SpacerPlacer Spacer Placer (Maybe Placer2) Spacer 
  deriving (Show)

data PlacementState
  = Placed (Rect->Rect)
  | Waiting 
  deriving (Show)

autoLayoutF = autoLayoutF' nowait Sizing.Dynamic

nowait = argFlag "nowait" False

autoLayoutF' :: Bool -> Sizing.Sizing -> F a b -> F a b
autoLayoutF' nowait sizing fud =
    loopThroughRightF 
      (userLayoutF (layoutDoNow fud)) 
      ({- spyF -} (sizingF sizing (ioF (autoLayoutMgrK0 state0 top0))))
	-- Note that the sizingF filter is not wrapped around fud and hence
	-- does not have to examine all commands and events!
  where
    state0 = if nowait then Placed id else Waiting

autoLayoutMgrK0 pstate ltree =
  debugK "autoLayoutMgrK" $
  autoLayoutMgrK pstate ltree

autoLayoutMgrK pstate ltree =
    --echoK (show (pstate,ltree)) $
    getK $ \ msg ->
    case msg of
      High (path,layoutmsg) ->
        case layoutmsg of
	  LayoutDoNow ->
	    debugK "LayoutDoNow" $
	    debugK (show ltree) $
	    newPlace ltree
	  LayoutRequest req ->
	       debugK (show path ++ " Layout "++show (minsize req)) $
	       debugK (show ltree') $
	       changePlacement ltree'
	    where ltree' = updateLeaf path req ltree''
	          ltree'' = if newBox ltree path
		            then forgetPlaces ltree
			    else ltree
	  -- LayoutHint & LayoutPlacer are only sent during initialisation.
	  -- They will be received before any child Layout requests.
	  LayoutHint hint ->
	    debugK (show path ++ " LayoutHint "++ show hint) $
	    updnode (insertHint hint)
	  LayoutPlacer placer ->
	    debugK (show path ++ " LayoutPlacer ...") $
	    updnode (insertPlacer placer)
	  LayoutSpacer spacer ->
	    debugK (show path ++ " LayoutSpacer ...") $
	    updnode (insertSpacer spacer)
	  -- LayoutReplaceSpacer is sent by dynSpacerF.
	  LayoutReplaceSpacer spacer ->
	    debugK (show path ++ " LayoutReplaceSpacer ...") $
	    replnode (replaceSpacer spacer)
	  LayoutReplacePlacer placer ->
	    debugK (show path ++ " LayoutReplacePlacer ...") $
	    replnode (replacePlacer placer)
	  LayoutDestroy -> 
	    debugK (show (path,ltree) ++ " LayoutDestroy") $
	    -- should check if the subtree contains anything but hints.
	    if newBox ltree path then debugK ("not in tree") same else
	    changePlacement (forgetPlaces (pruneLTree path ltree))
	      -- !! forgetPlaces should be called when the structure changes,
	      -- but not when an existing fudget requests a new size...
	  LayoutMakeVisible _ _ -> putK (Low (LCmd layoutmsg)) $ same
	  LayoutScrollStep  _ -> putK (Low (LCmd layoutmsg)) $ same
	  _ -> same -- !!! handle other layout requests?!
        where updnode  u = newTree (updateLNode path u ltree)
	      replnode u = changePlacement (forgetPlaces (updateLNode path u ltree))
      Low (LEvt (LayoutPlace rect)) ->
          debugK ("splitting 1 Place into "++show (length msgs)) $
          putsK (map High msgs) $
	  newTree ltree'
	where (ltree',msgs) = doLayout (s2 rect) ltree
	      s2 = case pstate of
			Placed s2 -> s2
			_ -> id
      Low _ -> debugK "Ignored low level msg" same
  where
    same = autoLayoutMgrK pstate ltree
    newTree t' = newState pstate t'
    newState p' t' = autoLayoutMgrK p' t'
    changePlacement ltree' =
      case pstate of
        Placed _ -> newPlace ltree'
	Waiting -> newTree ltree'
    newPlace ltree =
      let ltree' = chooseLayout ltree
      in case collectReqs ltree' of
           ([],_) -> debugK "newPlace without any requests in ltree" same
	   ((req,s2):_,ltree2) ->
	     putK (Low (layoutRequestCmd req)) $
	     newState (Placed s2) ltree2

updateLNode path i t = updateNode id emptyNode t path $
     \(NodeInfo ni) -> NodeInfo (i ni)

insertHint hint (_,pi) = (case pi of
	   SpacerPlacer _ _ _ _ -> Nothing
	   _ -> Just hint,pi)

insertPlacer placer (hint,pi) = (Nothing,case pi of
   NoPlacerInfo -> SpacerPlacer idS placer Nothing idS
   JustSpacer s -> SpacerPlacer s placer Nothing idS
   SpacerPlacer s1 p _ s2 -> SpacerPlacer (s1 `compS` s2) (p `compP` placer)
			           Nothing idS)
     where compP :: Placer -> Placer -> Placer
	   compP (P p1) (P p2) = P $ \ reqs ->
	       let (req1,p1r) = p1 [req2]
	           (req2,p2r) = p2 reqs
	       in (req1,p2r.head.p1r)

insertSpacer spacer (hint,pi) = (hint,case pi of
   NoPlacerInfo -> JustSpacer spacer
   JustSpacer s -> JustSpacer (s `compS` spacer)
   SpacerPlacer s1 p p2 s2 -> SpacerPlacer s1 p p2 (s2 `compS` spacer))

replaceSpacer spacer (hint,pi) = (hint,pi')
  where
    pi' = case pi of
            NoPlacerInfo -> JustSpacer spacer
	    JustSpacer s -> JustSpacer spacer
	    SpacerPlacer s1 p p2 s2 -> SpacerPlacer spacer p p2 s2 -- hmm

replacePlacer placer (hint,pi) = (hint,pi')
  where
    pi' = case pi of
            NoPlacerInfo -> SpacerPlacer idS placer Nothing idS
	    JustSpacer s -> SpacerPlacer s placer Nothing idS
	    SpacerPlacer s1 p p2 s2 -> SpacerPlacer s1 placer Nothing s2

updateLeaf path l t =
  updateNode invalid emptyNode t path (const (LeafInfo (l,Nothing)))

pruneLTree path t = pruneTree invalid emptyNode t path

forgetPlaces = mapLT (apSnd (const Nothing)) id
 
newBox x = subTree (const False) True x

invalid (NodeInfo i) = NodeInfo (invalid' i)
  where
    invalid' (hi,SpacerPlacer s p p2 s2) = (hi,SpacerPlacer s p Nothing s2)
    invalid' ni = ni

emptyNode = NodeInfo (Nothing,NoPlacerInfo)

hasPlacer (Nothing,SpacerPlacer _ _ _ _) = True
hasPlacer _ = False

-- strip hints below placer, insert autoP where there are hints left
chooseLayout = snd . attrMapLT lf nf False where
  lf strip _ i = (strip,(),i)
  nf strip _ n = (strip',(),n') where
     strip' = case n of
	    (Nothing,SpacerPlacer _ _ _ _) -> True
	    _ -> strip --
     n' = if strip then n else choosePlacer n

choosePlacer i = case i of
   (hi@(Just _),pi) -> (hi,case pi of
     NoPlacerInfo -> SpacerPlacer idS autoP Nothing idS
     JustSpacer s -> SpacerPlacer s autoP Nothing idS
     p -> p)
   i -> i

attrMapLT lf nf = attrMapPathTree f where
   f i s a = case a of
     LeafInfo li -> a3 LeafInfo $ lf i s li
     NodeInfo ni -> a3 NodeInfo $ nf i s ni
   a3 c (i,s,b) = (i,s,c b)

collectReqs = apFst (flip compose []) . attrMapLT lf nf idS where
  lf s _ i@(req,oplace) = (s,reqf,i) where 
       reqf = (unS s lr:)
       lr = case oplace of
{- -- You can use static sizing of shell windows instead of this:
	      Just (Rect _ currentsize) ->
		-- use current size, not originally requested size
		--ctrace "spacer" ("current",i) $
		mapLayoutSize (const currentsize) req
-}
	      _ -> --ctrace "spacer" ("nocurrent",i)
	           req
  nf s reqfs n@(hi,pi) = case pi of
       NoPlacerInfo -> (s,reqf,n)
       JustSpacer s1 ->
	 --ctrace "spacer" (fst ((s `compS` s1) (Layout origin False False))) $
	 (s `compS` s1,reqf,n)
       SpacerPlacer s1 p orp2 s2 ->
	  --ctrace "spacer" (n,fst (compp $ [Layout origin False False])) $ 
	             (inherS,syntreq,n') where
		   rp2@(req2,p2) = compp `spacer2P` reqfl
		   reqfl = reqf []
		   compp = if hashint then p else sl `spacerP` p
		   inherS = if hashint then sl `compS` s2 else s2
		   syntreq = if null reqfl then id else (unS idS req2:)
		   sl = s `compS` s1
		   hashint = isJust hi
		   orp2' = if null reqfl then Nothing else Just rp2
		   n' = (hi,SpacerPlacer s1 p orp2' s2)
--	  Just (req,_) -> ctrace "spacer" (n,req) (s2,(idS req:),n)
    where reqf = compose reqfs

  compose = foldr (.) id

doLayout rect tree = runIO (doLayoutIO tree []) [rect]

spacer2P (P p) reqfs = (req,s2f.p2) where
   (reqs,s2s) = unzip reqfs
   s2f rs = [s2 r | (s2,r) <- zip s2s rs]
   (req,p2) = p reqs

doLayoutIO t path =
  case t of
    Tip -> returnIO t
    Node (LeafInfo (l,maybeOldRect)) _ _ ->
      getIO `bindIO` \ r ->
      putIO (reverse path,r) `thenIO` 
         -- check if r is different from old rect?
      returnIO (Node (LeafInfo (l,Just r)) Tip Tip)
    Dynamic dt -> returnIO Dynamic `ap` dynDoLayoutIO dt path 0 1
    Node ni@(NodeInfo (_,pi)) lt rt ->
	case pi of
	  SpacerPlacer s1 p orp2  s2 ->
	    case orp2 of
	      Just (req,placer2) ->
		   getIO `bindIO` \ r ->
		   ungetIO (placer2 r) `thenIO`
		   doBranches
	      Nothing -> returnIO t -- no requests in this tree
	  _ -> doBranches
      where
        doBranches =
	  returnIO (Node ni) `ap` doLayoutIO lt (L:path)
	                      `ap` doLayoutIO rt (R:path)


dynDoLayoutIO dt path n i =
  case dt of
    DynTip -> returnIO dt
    DynNode t lt rt ->
      returnIO DynNode `ap`
      doLayoutIO t (Dno (unpos n):path) `ap`
      dynDoLayoutIO lt path n (2*i) `ap`
      dynDoLayoutIO rt path (n+i) (2*i)

--

type IO' a i o = i -> o -> (a,(i,o))

runIO io i =
  let (a,(_,o)) = io i []
  in (a,o)

returnIO a i o = (a,(i,o))
putIO o1 is os = ((),(is,o1:os))
getIO (i:is) os  = (i, (is,os))
--getIO (i:is) os  = (Just i, (is,os))
--getIO []     os  = (Nothing,(is,os))
ungetIO is' is os = ((),(is'++is,os))

bindIO io1 xio2 i0 o0  =
  let (x,(i1,o2)) = io1 i0 o1
      (y,(i2,o1)) = xio2 x i1 o0
  in (y,(i2,o2))

thenIO f1 f2 = f1 `bindIO` const f2

fIO `ap` xIO = fIO `bindIO` \ f ->
               xIO `bindIO` \ x ->
	       returnIO (f x)
