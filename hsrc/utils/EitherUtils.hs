module EitherUtils where
import Data.Maybe
import Data.List(find)

type Cont c a = (a -> c) -> c

plookup p = fmap snd . find (p.fst)

-- mapfilter = Maybe.mapMaybe
-- stripMaybe = Maybe.fromJust
-- stripMaybeDef = Maybe.fromMaybe
-- isM = Maybe.isJust

--mapMaybe :: (a->b) -> Maybe a -> Maybe b
--mapMaybe = fmap

stripLeft (Left a) = Just a
stripLeft _ = Nothing

stripRight (Right b) = Just b
stripRight _ = Nothing

stripEither (Left a) = a
stripEither (Right b) = b

filterLeft = mapMaybe stripLeft

filterRight = mapMaybe stripRight

isLeft (Left _) = True
isLeft _ = False

isRight (Right _) = True
isRight _ = False

mapEither fl fr (Left l) = Left (fl l)
mapEither fl fr (Right r) = Right (fr r)

swapEither (Left x) = Right x
swapEither (Right y) = Left y

-- JSP 920929
splitEitherList [] = ([], [])
splitEitherList (x : xs) =
    let (lefts, rights) = splitEitherList xs
    in  case x of
          Left a -> (a : lefts, rights)
          Right a -> (lefts, a : rights)

fromLeft (Left x) = x
fromRight (Right y) = y
