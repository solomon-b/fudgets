module CompF(compF) where
import Fudget
--import Message(Message(..))
import Route
import SP
import Direction
import LayoutHints

--import Maptrace(ctrace) -- debugging

compF (F f1) (F f2) = layoutHintF parHint (F{-ff-} (compF' f1 f2))

compF' :: FSP a b -> FSP c d -> FSP (Either a c) (Either b d)
compF' f1 f2 =
    case f1 of
      PutSP (High x) f1' -> PutSP (High (Left x)) (compF' f1' f2)
      PutSP (Low tcmd) f1' -> PutSP (compTurnLeft tcmd) (compF' f1' f2)
      GetSP xf1 -> compF1 xf1 f2
      NullSP -> restF2 f2

compF1 :: (FEvent a -> FSP a b) -> FSP c d -> FSP (Either a c) (Either b d)
compF1 xf1 f2 =
    case f2 of
      PutSP (High y) f2' -> PutSP (High (Right y)) (compF1 xf1 f2')
      PutSP (Low tcmd) f2' -> PutSP (compTurnRight tcmd) (compF1 xf1 f2')
      GetSP xf2 -> GetSP (compF12 xf1 xf2)
      NullSP -> GetSP (restF1x xf1)

compF2 :: FSP a b -> (FEvent c -> FSP c d) -> FSP (Either a c) (Either b d)
compF2 f1 xf2 =
    case f1 of
      PutSP (High x) f1' -> PutSP (High (Left x)) (compF2 f1' xf2)
      PutSP (Low tcmd) f1' -> PutSP (compTurnLeft tcmd) (compF2 f1' xf2)
      GetSP xf1 -> GetSP (compF12 xf1 xf2)
      NullSP -> GetSP (restF2x xf2)

compF12 :: (FEvent a -> FSP a b) -> (FEvent c -> FSP c d) -> (FEvent (Either a c)) -> FSP (Either a c) (Either b d)
compF12 xf1 xf2 msg =
    case msg of
      High (Left x) -> compF2 (xf1 (High x)) xf2
      High (Right y) -> compF1 xf1 (xf2 (High y))
      Low (tag, ev) -> case tag of
                         L : tag' -> compF2 (xf1 (Low (tag', ev))) xf2
                         R : tag' -> compF1 xf1 (xf2 (Low (tag', ev)))
                         _ -> {-ctrace "drop" (tag,ev) $-} GetSP (compF12 xf1 xf2)

restF2 f2 =
    case f2 of
      PutSP (High y) f2' -> PutSP (High (Right y)) (restF2 f2')
      PutSP (Low tcmd) f2' -> PutSP (compTurnRight tcmd) (restF2 f2')
      GetSP xf2 -> GetSP (restF2x xf2)
      NullSP -> NullSP

restF2x xf2 msg =
    case msg of
      High (Right y) -> restF2 (xf2 (High y))
      Low (R : tag, ev) -> restF2 (xf2 (Low (tag, ev)))
      _ -> GetSP (restF2x xf2)

restF1 f1 =
    case f1 of
      PutSP (High y) f1' -> PutSP (High (Left y)) (restF1 f1')
      PutSP (Low tcmd) f1' -> PutSP (compTurnLeft tcmd) (restF1 f1')
      GetSP xf1 -> GetSP (restF1x xf1)
      NullSP -> NullSP

restF1x xf1 msg =
    case msg of
      High (Left x) -> restF1 (xf1 (High x))
      Low (L : tag, ev) -> restF1 (xf1 (Low (tag, ev)))
      _ -> GetSP (restF1x xf1)

