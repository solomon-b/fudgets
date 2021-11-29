module SerCompF(stubF, bypassF, throughF, toBothF,
                idF, concatMapF, mapF, mapstateF, absF, idLeftF,
                idRightF, serCompLeftToRightF, serCompRightToLeftF,
		serCompF) where
--import Command(Command(..))
import CompF
import CompFfun(postMapHigh)
import CompSP
--import Direction
--import Event(Event(..))
import Fudget
import Loop
--import Message(Message(..))
import NullF
--import Path(Path(..))
import Route
import Spops
import EitherUtils(stripEither)
import LayoutHints

serCompF (F f1) (F f2) = layoutHintF serHint (F{-ff-} $ serCompF' f1 f2)

serCompF' :: FSP a b -> FSP c a -> FSP c b
serCompF' f1 f2 =
    let post (Left (High x)) = High x
        post (Left (Low tcmd)) = compTurnLeft tcmd
        post (Right tcmd) = compTurnRight tcmd
        mid (Left ltev) = Left ltev
        mid (Right (Low tcmd)) = Right tcmd
        mid (Right (High x)) = Left (High x)
        pre (High x) = [Right (High x)]
        pre (Low ([], _)) = []
        pre (Low tev) = compPath tev [] (:[])
    in  serCompSP (postMapSP post
                             (serCompSP (idRightSP f1)
                                        (postMapSP mid (idLeftSP f2))))
                  (concmapSP pre)

serCompRightToLeftF :: (F (Either a b) (Either c a)) -> F b c
serCompRightToLeftF (F sp) =
    let post (Low x) = Right (Low x)
        post (High (Left x)) = Right (High x)
        post (High (Right x)) = Left x
        pre (Right (Low x)) = Low x
        pre (Right (High x)) = High (Right x)
        pre (Left xs) = High (Left xs)
    in F{-ff-} $ loopLeftSP (prepostMapSP pre post sp)

serCompLeftToRightF :: (F (Either a b) (Either b c)) -> F a c
serCompLeftToRightF (F sp) =
    let post (Low x) = Right (Low x)
        post (High (Right x)) = Right (High x)
        post (High (Left x)) = Left x
        pre (Right (Low x)) = Low x
        pre (Right (High x)) = High (Left x)
        pre (Left xs) = High (Right xs)
    in F{-ff-} $ loopLeftSP (prepostMapSP pre post sp)

idRightF :: (F a b) -> F (Either a c) (Either b c)
--and idRightF w = w:+:idF
idRightF w = compF w idF

idLeftF w = compF idF w

absF :: (SP a b) -> F a b
absF sp =
    let pre (High x) = [x]
        pre (Low y) = []
    in F{-ff-} $ serCompSP (postMapSP High sp) (concmapSP pre)

concatMapF = absF . concatMapSP
mapF = absF . mapSP
mapstateF f x = absF (mapstateSP f x)

idF = mapF id

toBothF = concatMapF (\x -> [Left x, Right x])

throughF w = serCompF (idRightF w) toBothF

--and throughF w = idRightF w:==:toBothF
bypassF :: (F a a) -> F a a
bypassF f = postMapHigh stripEither (throughF f)

stubF :: F a b -> F c d
stubF f = serCompF (serCompF nullF f) nullF

