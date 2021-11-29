module Spops(module Spops,Cont(..)) where
import SP
import EitherUtils(Cont(..))

{- INLINE nullSP -}
nullSP :: SP a b
nullSP = NullSP

putsSP :: [b] -> SP a b -> SP a b
putsSP [] sp = sp
putsSP (x : xs) sp = PutSP x (putsSP xs sp)

{- INLINE putSP -}
putSP :: b -> SP a b -> SP a b
putSP = PutSP

appendStartSP :: [b] -> SP a b -> SP a b
appendStartSP zs (PutSP y sp) = PutSP y (putsSP zs sp)
appendStartSP zs sp = putsSP zs sp

stepSP :: [b] -> Cont (SP a b) a
stepSP ys xsp = putsSP ys (GetSP xsp)

getSP :: Cont (SP a b) a
getSP = GetSP

walkSP sp x =
    case sp of
      PutSP y sp' -> let (ys, sp'') = walkSP sp' x
                     in  (y : ys, sp'')
      GetSP xsp' -> pullSP (xsp' x)
      NullSP -> ([], NullSP)

pullSP sp =
    case sp of
      PutSP y sp' -> let (ys, sp'') = pullSP sp'
                     in  (y : ys, sp'')
      _ -> ([], sp)

runSP sp xs =
    case sp of
      PutSP y sp' -> y : runSP sp' xs
      GetSP xsp -> case xs of
                     x : xs' -> runSP (xsp x) xs'
                     [] -> []
      NullSP -> []

feedSP :: a -> [a] -> SP a b -> SP a b
feedSP x xs sp =
    case sp of
      PutSP y sp' -> PutSP y (feedSP x xs sp')
      GetSP xsp' -> startupSP xs (xsp' x)
      NullSP -> NullSP

startupSP :: [a] -> SP a b -> SP a b
startupSP [] sp = sp
startupSP (x : xs) sp = feedSP x xs sp

delaySP sp = GetSP (\x -> startupSP [x] sp)

mapSP f = m where m = GetSP (\x -> PutSP (f x) m)

idSP = GetSP (\x -> PutSP x idSP)

--and concatMapSP :: (*a->[*b]) -> SP *a *b
concatMapSP f = m where m = GetSP (\x -> putsSP (f x) m)

concmapSP = concatMapSP

concatMapAccumlSP f s0 =
    GetSP (\x ->
           let (s, y) = f s0 x
           in putsSP y (concatMapAccumlSP f s))

mapstateSP = concatMapAccumlSP

mapAccumlSP f s0 =
    GetSP (\x ->
           let (s, y) = f s0 x
           in PutSP y (mapAccumlSP f s))

concatSP = GetSP (\xs -> putsSP xs concatSP)
concSP = concatSP

zipSP (x : xs) = getSP (\y -> putSP (x, y) (zipSP xs))
zipSP [] = nullSP

filterSP p = getSP (\x -> (if p x then putSP x else id) (filterSP p))

splitAtElemSP :: (a -> Bool) -> Cont (SP a b) [a]
splitAtElemSP p xsp =
    let lSP acc =
            getSP (\x -> if p x then xsp (reverse acc) else lSP (x : acc))
    in  lSP []

chopSP splitSP' = splitSP' (\xs -> putSP xs (chopSP splitSP'))
