-- | Useful functions from hbc-library modules ListSet, ListMap, ListUtil, HO
-- and UTF8
module HbcUtils(module HbcUtils,module FudUTF8) where
import Data.List((\\))
import FudUTF8

-- * From ListSet

-- | Union of sets as lists
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ (ys \\ xs)


-- * From ListMap
lookupWithDefault :: (Eq a) => [(a, b)] -> b -> a -> b
lookupWithDefault [] d _ = d
lookupWithDefault ((x,y):xys) d x' = if x == x' then y else lookupWithDefault xys d x'


-- * From ListUtil
mapFst f = map (apFst f)
mapSnd f = map (apSnd f)

breakAt c = apSnd (drop 1) . break (==c)

chopList :: ([a] -> (b, [a])) -> [a] -> [b]
chopList f l = unfoldr f null l
  where
    -- | Repeatedly extract (and transform) values until a predicate hold.  Return the list of values.
    unfoldr :: (a -> (b, a)) -> (a -> Bool) -> a -> [b]
    unfoldr f p x | p x       = []
                  | otherwise = y:unfoldr f p x'
                                  where (y, x') = f x

assoc :: Eq k => (v -> r) -> r -> [(k, v)] -> k -> r
assoc f z xs k = maybe z f (lookup k xs)

-- * From HO

apFst f (x, y) = (f x, y)
apSnd f (x, y) = (x, f y)

curry3 f x y z = f (x,y,z)
uncurry3 f ~(x,y,z) = f x y z
