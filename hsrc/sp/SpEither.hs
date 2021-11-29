module SpEither where
import SP
import EitherUtils(stripLeft,stripRight)
import Spops(concatMapSP)

mapFilterSP f = m
  where m = GetSP $ \x->
	    case f x of
	      Just y  -> PutSP y m
	      Nothing -> m

filterLeftSP = mapFilterSP stripLeft
filterRightSP = mapFilterSP stripRight

filterJustSP = mapFilterSP id

splitSP = concatMapSP (\(x, y) -> [Left x, Right y])

toBothSP = concatMapSP (\x -> [Left x, Right x])
