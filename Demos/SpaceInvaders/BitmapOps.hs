module BitmapOps(doubleBM,explodeBM,bm,Bitmap(..),E(..),Size,Byte) where
--import AllFudgets(BitmapData(..),Point(..),Size)
--import Data.List.Split(chunksOf)

data Bitmap = Bitmap Size [Byte]
data E = E Size [[Bit]] deriving Eq
type Bit = Bool
type Byte = Int
type Size = (Int,Int)

doubleBM = implodeE . doubleE . explodeBM

doubleE (E (w,h) rows) = E (2*w,2*h) (double (map double rows))

double = foldr (\x r->x:x:r) []

{-
instance Show E where
  show (E _ rows) = unlines [[".*"!!fromEnum b|b<-row]|row<-rows]
  showList = (++) . unlines . map show
-}

explodeBM (Bitmap s@(w,h) bytes) = E s rows
  where
    b = (w+7) `div` 8
    rows = map (take w . concatMap explode) (chunksOf b bytes)

implodeE (E s@(w,h) rows) = Bitmap s bytes
   where
     bytes = concatMap (map implode . chunksOf 8) rows

implode = foldr (\b r->fromEnum (b::Bit)+2*r) 0

explode = take 8.expl
  where
    expl n = (n `mod` 2 /= 0):expl (n `div` 2)

bm w h = Bitmap (w,h)

---

chunksOf n [] = []
chunksOf n xs = xs1:chunksOf n xs2
  where (xs1,xs2) = splitAt n xs
