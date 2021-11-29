module Utils where
--import ListSet(union)
--import HbcWord
import HbcUtils

infixr 1 `thenC`
infixl 1 `ifC`

aboth f (x, y) = (f x, f y)
mapPair (f, g) (x, y) = (f x, g y)
pairwith f x = (x, f x)
swap (x, y) = (y, x)
pair = (,)

setFst (_,y) x = (x,y)
setSnd (x,_) y = (x,y)

oo f g x y = f (g x y)

-- | Apply a function to the nth element of a list
anth :: Int -> (a->a) -> [a] -> [a]
anth _ _ [] = []
anth 1 f (x : xs) = f x : xs
anth n f (x : xs) = x : anth (n - 1) f xs

--dropto p = while (\l -> l /= [] && (not . p . head) l) tail

number :: Int -> [a] -> [(Int,a)]
number _ [] = []
number i (x : xs) = (i, x) : number (i + 1) xs

loop f =
    let yf = f yf
    in  yf

ifC c b = if b then c else id
thenC = flip ifC

gmap g f = foldr (\x -> \ys -> g (f x) ys) []

unionmap f = gmap union f

-- | Remove the first occurence
remove a (b : bs) | a == b = bs
remove a (b : bs) = b : remove a bs
remove a [] = []

-- | Replace the first occurence
replace p [] = [p]
replace (t, v) ((t', v') : ls') | t == t' = (t, v) : ls'
replace p (l : ls') = l : replace p ls'

unconcat [] _ = []
unconcat (n : ns) xs = xs1:unconcat ns xs2
  where (xs1,xs2) = splitAt n xs

-- | lunconcat xss ys = unconcat (map length xss) ys
lunconcat [] _ = []
lunconcat (n : ns) xs = xs1:lunconcat ns xs2
  where (xs1,xs2) = lsplit n xs


-- | lhead xs ys = take (length xs) ys, but the rhs is stricter
lhead (x : xs) (y : ys) = y : lhead xs ys
lhead _ _ = []

-- | ltail xs ys = drop (length xs) ys, but the rhs is stricter
ltail [] ys = ys
ltail _ [] = []
ltail (x : xs) (y : ys) = ltail xs ys

-- | lsplit xs ys = (lhead xs ys,ltail xs ys), but without the space leak, -fpbu
lsplit [] ys = ([], ys)
lsplit _ [] = ([], [])
lsplit (x : xs) (y : ys) =
    let (yhs, yts) = lsplit xs ys
    in  (y : yhs, yts)

-- | JSP 920928
part p [] = ([], [])
part p (x : xs) =
    let (ys, zs) = part p xs
    in  if p x then (x : ys, zs) else (ys, x : zs)

issubset a b = all (`elem` b) a

-- | To avoid problems caused by poor type inference for constructor classes in
-- Haskell 1.3:
mapList = map :: ((a->b)->[a]->[b])

-- From Compat.hs:
--bitxor,bitand::Int->Int->Int
--bitxor x y = wordToInt (bitXor (fromIntegral x) (fromIntegral y))
--bitand x y = wordToInt (bitAnd (fromIntegral x) (fromIntegral y))


-- | @chopList (breakAt c) == segments (/=c)@
segments p [] = []
segments p xs = case span p xs of
                  (xs1,_:xs2) -> xs1:segments p xs2
                  (xs1,_) -> [xs1]
