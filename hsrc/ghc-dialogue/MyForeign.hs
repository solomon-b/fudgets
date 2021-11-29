module MyForeign(module MyForeign,(<$>),(<*>),F.Int32,F.CSize(..)) where
import Control.Applicative
import qualified Foreign as F
import qualified Foreign.C as F
import Foreign.C.String(castCCharToChar,castCharToCChar)
--import CCall
import Control.Exception(bracket)
--import Ap
-- Emulate GHC 4.08 libraries on top of GHC 5.00 libraries...

newtype Addr = Addr (F.Ptr F.Word8) deriving (Eq,Show)
type AddrOff = Int

--instance CCallable Addr
--instance CReturnable Addr

class Storable a where
  sizeOf      :: a -> Int
  alignment   :: a -> Int

  peekElemOff :: Addr -> Int          -> IO a
  pokeElemOff :: Addr -> Int     -> a -> IO ()

  peekByteOff :: Addr -> AddrOff      -> IO a
  pokeByteOff :: Addr -> AddrOff -> a -> IO ()

  peek        :: Addr                 -> IO a
  poke        :: Addr            -> a -> IO ()

  peek a = peekByteOff a 0
  poke a = pokeByteOff a 0

  peekElemOff a i =
    let iox = peekByteOff a (i*sizeOf (u iox))
	u :: IO a -> a
	u = undefined
    in iox
  pokeElemOff a i x = pokeByteOff a (i*sizeOf x) x

  peekByteOff a i = peek (a `plusAddr` i)
  pokeByteOff a i = poke (a `plusAddr` i)

malloc n = Addr <$> F.mallocBytes n
mallocElem x = malloc (sizeOf x)
mallocElems x n = malloc (n*sizeOf x)
free (Addr p) = F.free p
alloca n = bracket (malloc n) free
allocaElem x = bracket (mallocElem x) free
allocaElems x n = bracket (mallocElems x n) free

nullAddr = Addr F.nullPtr
plusAddr (Addr p) n = Addr (F.plusPtr p n)
minusAddr (Addr p1) (Addr p2) = F.minusPtr p1 p2

instance Storable Addr where
  sizeOf (Addr a) = F.sizeOf a
  alignment (Addr a) = F.alignment a

  peek a = Addr <$> fpeek a
  poke a (Addr x) = fpoke a x

instance Storable Char where
  sizeOf _ = 1
  alignment _ = 1

  peek a = castCCharToChar <$> fpeek a
  poke a c = fpoke a (castCharToCChar c)

fpeek (Addr p) = F.peek (F.castPtr p)
fpoke (Addr p) = F.poke (F.castPtr p)

instance Storable Int where
  sizeOf x = F.sizeOf x
  alignment x = F.alignment x

  peek = fpeek
  poke = fpoke


instance Storable F.Int32 where
  sizeOf x = F.sizeOf x
  alignment x = F.alignment x

  peek = fpeek
  poke = fpoke


instance Storable F.Word32 where
  sizeOf x = F.sizeOf x
  alignment x = F.alignment x

  peek = fpeek
  poke = fpoke
