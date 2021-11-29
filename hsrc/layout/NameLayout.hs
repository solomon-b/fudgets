module NameLayout(LName(..),placeNL,spaceNL,modNL,marginNL,sepNL,hvAlignNL,marginHVAlignNL,nullNL,hBoxNL,hBoxNL',vBoxNL, vBoxNL',leafNL, NameLayout, nameF,
 listNF, nameLayoutF) where

--import NonStdTrace(trace)
import LayoutRequest
import NullF
import Spops
--import Command
--import Event
import FRequest
--import Xtypes
import EitherUtils(plookup)
import Data.Maybe(fromJust)
import Fudget
import Path
import Geometry
import Placers
import Spacers
--import Message
import ListF
import Loopthrough
import Cont
--import LayoutDir
import AlignP
--import Alignment
import Utils
import Maptrace
import AutoLayout
import ParF

type LName = String
newtype NameLayout = NL (MLNode LName) -- abstract

-- The layout structure datatype
type MLNode a = (Maybe LayoutRequest, LNode a)
data LNode a = 
      LNode Int Placer (Maybe (Rect -> [Rect])) [MLNode a]
    | LLeaf (LLeaf a) deriving Show

data LLeaf a = Name a | Req LayoutRequest deriving Show

type NPath = [Int]

-----------------------------------------------------------------------------------
-- Exported functions

placeNL :: Placer -> [NameLayout] -> NameLayout
placeNL lter ns = let dns = map deNL ns in
   buildnl $ LNode (length (filter (nothing.fst) dns)) lter Nothing dns

spaceNL :: Spacer -> NameLayout -> NameLayout
spaceNL = modNL . spacerP

modNL :: (Placer -> Placer) -> NameLayout -> NameLayout
modNL ltermod (NL (req,n)) = NL $ case n of
   LNode i lter f ls -> (req,LNode i (ltermod lter) f ls)
   LLeaf l -> 
      let lter = ltermod idP
          P lter' = lter in 
      case l of
	 Req r -> leafReq $ fst $ lter' $ [r]
	 _ -> (Nothing,LNode 1 lter Nothing [(req,n)])

marginNL = spaceNL . marginS
sepNL = spaceNL . sepS

hvAlignNL = spaceNL `oo` hvAlignS
marginHVAlignNL sep ha va = spaceNL (marginHVAlignS sep ha va)

hBoxNL = placeNL $ horizontalP
hBoxNL' d = placeNL $ horizontalP' d
vBoxNL = placeNL $ verticalP
vBoxNL' d = placeNL $ verticalP' d
leafNL name = buildnl $ LLeaf $ Name name
nullNL = NL $ leafReq $ plainLayout (Point 1 1) False False

nameF :: LName -> F a b -> F a b
nameF n = putMessageFu (Low (LCmd (LayoutName n))) . autoLayoutF

-- local

nothing Nothing = True
nothing _ = False

buildnl :: LNode LName -> NameLayout
buildnl x = NL (Nothing,x)
deNL (NL x) = x

leafReq :: LayoutRequest -> MLNode a
leafReq req = (Just req,LLeaf $ Req $ req)

listNF :: (Eq a, Show a) => [(a, F b c)] -> F (a, b) (a, c)  
listNF fs = listF [(t, nameF (show t) f) | (t, f) <- fs]

-- The main layout function
nameLayoutF :: NameLayout -> F a b -> F a b
nameLayoutF (NL ltree) (F fsp) =
    let layoutSP =
            getAllPNames (countLNames ltree) [] $ \pnames ->
            let (pathTable, ltree') = rebuildTree pnames [] ltree
	    in lSP pathTable ltree'
        lSP pt ltree = 
            let same = lSP pt ltree in
            getSP $ \msg ->
	    case msg of
	      -- A message from the fudget
	      Left (Low (path, LCmd (LayoutRequest lr))) ->
		  ctrace "nameLayoutF" lr $
		  let ltree' = updateTree path (pathlookup pt path) ltree lr
		  in case ltree' of 
			(Just lreq, _) -> --trace (show ltree') $
			    putSP (Right (Low ([], layoutRequestCmd lreq))) $
			    lSP pt ltree'
			_ -> lSP pt ltree'
	      Left x -> putSP (Right x) $ same
	      -- A message to the fudget
	      Right (Low (path, LEvt (LayoutPlace r))) ->
		  putsSP (map (Left. Low) $ traverseTree r ltree) $ same
	      Right x -> putSP (Left x) $ same
    in parF nullF $ F{-ff-} $ loopThroughRightSP layoutSP fsp
--  fix for autolayout

-----------------------------------------------------------------------------------
-- Local functions

-- Counts the number of named leafs in a layout structure
countLNames :: MLNode a -> Int
countLNames (_, LLeaf (Name _)) = 1
countLNames (_, LLeaf _) = 0
countLNames (_, LNode _ _ _ ns) = sum (map countLNames ns)

-- Traverses the layout structure, returning a mapping from leaf names to paths
getAllPNames :: Int -> [(LName, Path)] -> 
	     Cont (SP (Either (FCommand a) b) c) [(LName,Path)]
getAllPNames 0 pnames c = c pnames
getAllPNames n pnames c =
    waitForSP layoutName $ \pname ->
    getAllPNames (n-1) (pname:pnames) c
    where layoutName (Left (Low (path, LCmd (LayoutName name)))) = 
              Just(name, path)
          layoutName _ = Nothing

-- Rebuilds the layout structure. 
-- Returns also a mapping from ordinary paths to number paths.
rebuildTree :: [(LName, Path)] -> NPath -> MLNode LName ->
               ([(Path, NPath)], MLNode Path)
rebuildTree pnames np (_, LLeaf (Name name)) = 
    case lookup name pnames of
        Nothing -> error ("Couldn't find name "++ show name ++ 
			  " in (name, path) table.")
	Just path -> ([(path, np)], (Nothing, LLeaf $ Name path))
rebuildTree pnames np (_, (LLeaf (Req r))) = ([],(Just r, (LLeaf (Req r))))
rebuildTree pnames np (_, LNode c lter Nothing ns) =
    (concat ts, (Nothing, LNode c lter Nothing ns'))
    where (ts, ns') = unzip (zipWith (rebuildTree pnames) 
                                     (map ((np++) . (:[])) [1..]) ns)


-- Inserts layout requests in the layout structure.
-- Trigged by some fudget emitting a layout request
updateTree :: Path -> 
              Maybe NPath -> 
	      MLNode Path -> 
	      LayoutRequest -> 
              MLNode Path
updateTree path Nothing _ lr = 
    error ("Hmmm. Couldn't find path " ++ show path ++
    "in updateTree\nSomeone has probably forgotten to name a fudget.")
updateTree path (Just npath) lo lr = snd $ upd npath lo lr
    where upd _ (mlr, LLeaf (Name p)) lr = 
                 (nothing mlr, (Just lr, LLeaf (Name path)))
          upd (n:np) (mlr, LNode c lter@(P lter') mr ns) lr =
	      let (before, this:after) = splitAt (n-1) ns
	          (ready, child) = upd np this lr
	          c' = if ready then max (c-1) 0 else c
		  ns' = before ++ [child] ++ after
	      in if c' == 0 then
	             let (lreq, rectf) = lter' (map (fromJust . fst) ns')
		     in (nothing mlr, 
		         (Just lreq, LNode c' lter (Just rectf) ns'))
		 else
		     (False, (Nothing, LNode c' lter mr ns'))
          upd _ othernode _ = (False,othernode)

-- We have got a rectangle. Emit commands to all subfudgets saying how large
-- they should be. 
traverseTree :: Rect -> MLNode Path -> [TEvent]
traverseTree r (_, LLeaf (Name path)) = [(path, LEvt $ LayoutPlace r)]
traverseTree r (_, LLeaf _) = []
traverseTree r (_, LNode _ _ (Just rectf) ns) =
    concat (zipWith traverseTree (rectf r) ns) 

pathlookup table p = plookup (flip subPath p) table
