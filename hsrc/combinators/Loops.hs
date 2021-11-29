module Loops(loopF, loopThroughRightF, loopCompThroughRightF, loopCompThroughLeftF, loopRightF, loopLeftF, loopOnlyF) where
import Maptrace(ctrace) -- debugging -- syntax error if you put this is last in the import list. TH 960428
--import Command(Command(..))
import CompFfun(prepostMapHigh)
--import CompSP(prepostMapSP)
import CompOps
--import Event(Event(..))
import Fudget
import Loop
--import Message(Message(..))
--import Path(Path(..))
--import SP
import SpEither(toBothSP,mapFilterSP)
import EitherUtils(stripEither, swapEither)
import LayoutHints
import Route
import Direction
import Loopthrough
import CompSP


loopLeftF :: (F (Either a b) (Either a c)) -> F b c
loopLeftF (F sp) =
    let post (Low x) = Right (Low x)
        post (High (Right x)) = Right (High x)
        post (High (Left x)) = Left x
        pre (Right (Low x)) = Low x
        pre (Right (High x)) = High (Right x)
        pre (Left xs) = High (Left xs)
    in {-layoutHintF loopHint-} (F{-ff-} $ loopLeftSP (prepostMapSP pre post sp))

loopRightF :: (F (Either a b) (Either c b)) -> F a c
loopRightF f = loopLeftF (prepostMapHigh swapEither swapEither f)

loopThroughRightF :: F (Either a b) (Either c d) -> F c a -> F b d
loopThroughRightF (F m) (F s) = 
   --loopThroughRightSP (prepostMapSP pre post (idRightSP m)) s where
   layoutHintF loopHint $
   F{-ff-} $
   loopThroughRightSP
      (post `postMapSP` (idRightSP m) `serCompSP` mapFilterSP pre) s where

   post (Left (Low c)) = Right (compTurnLeft c)
   post (Right (Left c)) = Right (compTurnRight c)
   post (Right (Right e)) = Left (Low e)
   post (Left (High (Left m))) = Left (High m)
   post (Left (High (Right m))) = Right (High m)

   pre (Right (Low (p,e))) =
     case p of
       L:p -> Just $ Left (Low (p,e))
       R:p -> Just $ Right (Right (p,e))
       _   -> ctrace "drop" (p,e) $ Nothing --error "Dno in loopThroughRightF"
   pre (Left (Low c)) = Just $ Right (Left c)
   pre (Right (High m)) = Just $ Left (High (Right m))
   pre (Left (High m)) = Just $ Left (High (Left m))

loopCompThroughRightF :: (F (Either (Either a b) c) (Either (Either c d) a)) -> F b d
loopCompThroughRightF w =
    let post (Left (Left x)) = Left (Right x)
        post (Left (Right x)) = Right x
        post (Right x) = Left (Left (Left x))
        pre (Left x) = x
        pre (Right x) = Left (Right x)
    in  loopLeftF (prepostMapHigh pre post w)


loopCompThroughLeftF :: (F (Either a (Either b c)) (Either b (Either a d))) -> F c d
loopCompThroughLeftF f =
    loopCompThroughRightF (prepostMapHigh swapEither swapEither f)

loopOnlyF :: F a a -> F a b
loopOnlyF f = loopLeftF (prepostMapHigh stripEither Left f)

loopF :: F a a -> F a a
loopF f = loopLeftF (toBothSP>^^=<f>=^<stripEither)
