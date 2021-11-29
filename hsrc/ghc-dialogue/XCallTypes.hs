{- unused options -#include <X11/Xlib.h> -#include <X11/Xutil.h> -fvia-C -}
-- -optc-I/usr/X11R6/include
module XCallTypes
 --(module XCallTypes,newCharArray,MutableByteArray(..))
 where

import Data.Bits
import Data.Word(Word32)
import Control.Applicative
import Control.Monad(foldM)
--import MyForeign(Int32)

import Utils(number)
import Xtypes
import Geometry
--import Ap(foldR)
--import PackedString(unpackPS,byteArrayToPS{-,packCString-})

-- #include "structs.h"

getEnum bla = fromEnum
toEnum' bla = toEnum
{-
toEnum' s = (a!)
  where a = listArray (0,length l - 1) l
        l = [s..]

getEnum s = (a!)
  where a = listArray (s,last [s..]) [(0::Int)..]
-}

class ToC a where toC :: a -> Int
class ToCl a where toCl :: [a] -> Int
class FromC a where fromC :: Int -> a

class ToXID a where toXID :: a -> XID
--class FromXID a where fromXID :: XID -> a

instance (ToCl a) => ToC [a] where toC = toCl

instance ToCl EventMask where 
   toCl = fromIntegral . foldr getE (0::Word32)
     where
       getE e m = setBit m (fromEnum e)

instance ToC Bool where toC False = 0
                        toC True = 1

instance FromC Bool where fromC 0 = False
                          fromC _ = True


instance ToXID PixmapId where toXID (PixmapId p) = p
instance ToC Pixel where toC (Pixel p) = fromIntegral p
instance ToXID ColormapId where toXID (ColormapId p) = p
instance ToXID CursorId where toXID (CursorId p) = p
instance ToXID FontId where toXID (FontId p) = p
--instance ToC WindowId where toC (WindowId p) = p
--instance ToC Display where toC (Display p) = p
--instance ToC Width where toC (Width p) = p
--instance ToC Atom where toC (Atom p) = p
--instance ToC PropertyMode where toC (PropertyMode p) = p

--pIoCmd x = primIOToIO x :: IO ()
--pIoCmd x = stToIO x :: IO ()
ioCmd x = x :: IO ()

getValues new getValue vl = do
  vs <- new
  let maskf mask val = do set; return (mask .|. m)
                  where (set,m) = getValue vs val
  mask <- foldM maskf 0 vl
  return (vs,mask)

failu :: String -> IO a
failu = ioError . userError

--unpackCharArray len a = fmap (take len . unpackPS . byteArrayToPS) $
--    stToIO $ unsafeFreezeByteArray a

--cstring :: Addr -> String -- This type looks a bit suspicious... /TH 990211
--cstring = unpackCString

getArray new mod l = do
       arr <- new size
       mapM_ (mod arr) (number 0 l)
       return (arr,size)
   where size = length l


{-
H_ARRAY(int)
newInt = newintArray 1
readInt i = CINDEX(int) (i::Cint) (0::Int) :: IO Int
writeInt i v = SINDEX(int,i::Cint,0::Int,v::Int)
-}

mkPoint x y = Point <$> x <*> y
mkRect x y w h = Rect <$> mkPoint x y <*> mkPoint w h

--mkAtom a = fmap Atom a
--mkSelection s t p = Selection <$> mkAtom s <*> mkAtom t <*> mkAtom p
--mkSelection s t p = Selection <$> s <*> t <*> p

instance FromC ModState where 
 fromC ni = [toEnum i|i<-[15,14..0],testBit ni i]
{-
     concatMap toModifier [15,14..0]
   where
     toModifier i = [toEnum i|testBit ni i]
--   toe = toEnum' Shift -- . fromIntegral
--   n = fromIntegral ni :: Word32
-}

notImplemented x = take 79 ("Not implemented: "++show x)++"\n"

