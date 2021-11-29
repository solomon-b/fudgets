-- | Fake PackedString type
module PackedString{-(module PS)-} where
--import Data.PackedString as PS

newtype PackedString = PS String deriving (Eq,Ord)

instance Show PackedString where
  showsPrec n (PS s) r = s++r

instance Read PackedString where
  readsPrec n s0 = [(PS s,r)|(s,r)<-readsPrec n s0]

packString = PS
unpackPS (PS s) = s

nullPS (PS s) = null s
lengthPS (PS s) = length s

appendPS (PS s1) (PS s2) = PS (s1++s2)
mapPS f (PS s) = PS (map f s)
nilPS = PS ""
reversePS (PS s) = PS (reverse s)
