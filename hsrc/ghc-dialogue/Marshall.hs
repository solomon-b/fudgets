module Marshall where
import MyForeign
import Control.Monad(zipWithM_)
--import Ap

class HasAddr a where
   addrOf :: a -> Addr
   --atAddr :: Addr -> a

freePtr p = free (addrOf p)

class HasAddr a => IsPtr a where
   nullPtr :: a
   newPtr :: IO a
   newArray :: Int -> IO a

   newPtr = newArray 1

class (Storable h, HasAddr c) => CVar c h | c -> h where
  readCVar :: c -> IO h
  writeCVar :: c -> h -> IO ()
  indexCVar :: c -> Int -> IO h
  writeArray :: c -> [h] -> IO ()
  readArray :: c -> Int -> IO [h]

  readCVar = peek . addrOf
  writeCVar = poke . addrOf
  indexCVar = peekElemOff . addrOf
  writeArray arr = zipWithM_ (pokeElemOff (addrOf arr)) [0..]
  readArray arr n = mapM (indexCVar arr) [0..(n-1)]

toArray xs =
  do let n = length xs
     a <- newArray n
     writeArray a xs
     return (a,n)

{-
instance (Storable a,Storable b) => (Storable (a,b)) where
  sizeOf (a,b) = sizeOf a+sizeOf b
  alignment (a,b) = max (alignment a) (alignment b)
  poke p (a,b) = poke p a >> poke (p `plusAddr` sizeOf a) b
  peek p = do a <- peek p
	      b <- peek (p `plusAddr` sizeOf a) p
	      return (a,b)
-}

-------------------
class PrimArg ha pa r | ha->pa where
  --marshall :: haskell -> IO (prim,IO ())
  marshall :: (pa->r)->ha->r

-- marshallM x = marshall return x -- GHC generates buggy code, it seems

instance PrimArg Int Int c where marshall = id
instance PrimArg Int32 Int32 c where marshall = id
--instance PrimArg Char Char c where marshall = id
instance PrimArg Bool Bool c where marshall = id
--instance PrimArg () Int c where marshall f () = f 0
instance PrimArg Addr Addr c where marshall = id
instance PrimArg CLong CLong c where marshall = id
instance PrimArg CInt32 CInt32 c where marshall = id
instance PrimArg CString CString c where marshall = id

class    Bind f                where bind :: IO o->(o->f)->f
				     thn :: f->IO ()-> f
instance Bind (IO a)           where bind = (>>=)
				     thn f io = do r<-f;io;return r
instance Bind f => Bind (a->f) where bind ioo oaf = bind ioo . flip oaf
				     thn f io a = f a `thn` io

instance Bind f => PrimArg String CString f where
  marshall f str =
     marshallString str `bind` \ cstr ->
     f cstr `thn`
     freePtr cstr

class PrimResult prim haskell where
  unmarshall :: prim -> haskell

instance PrimResult () () where unmarshall = id
instance PrimResult Bool Bool where unmarshall = id
instance PrimResult Char Char where unmarshall = id
instance PrimResult Int Int where unmarshall = id
instance PrimResult Int32 Int32 where unmarshall = id

--instance PrimResult Int (IO Int) where unmarshall = return

instance PrimResult (IO ()) (IO ()) where unmarshall = id
instance PrimResult (IO Int) (IO Int) where unmarshall = id
instance PrimResult (IO Int32) (IO Int32) where unmarshall = id
instance PrimResult (IO Bool) (IO Bool) where unmarshall = id
instance PrimResult (IO CString) (IO CString) where unmarshall = id

unmarshallM m = unmarshall =<< m
unmarshallArray a n = mapM unmarshall =<< readArray a n

instance PrimResult (IO CString) (IO (Maybe String)) where
  unmarshall addrIO =
    do cstr <- addrIO
       if addrOf cstr==nullAddr
        then return Nothing
        else Just <$> unmarshall cstr

instance PrimResult CString (IO String) where unmarshall = unmarshallString
instance PrimResult (IO CString) (IO String) where unmarshall = unmarshallM

instance (PrimArg ha pa pb,PrimResult pb hb) => PrimResult (pa->pb) (ha->hb) where
    -- have: marshall      :: (pa->pb)->(ha->pb)
    -- have: unmarshall    :: pb->hb
    -- produce: unmarshall :: (pa->pb)->(ha->hb)
    unmarshall papb ha = unmarshall (marshall papb ha)

--- Primitive marshalling

newtype CString = CString Addr deriving (Eq)
instance HasAddr CString where addrOf (CString a) = a --; atAddr = CString
instance CVar CString CString
instance Storable CString where
  sizeOf (CString a) = sizeOf a
  alignment (CString a) = alignment a
  peek p = CString <$> peek p
  poke p (CString a) = poke p a

nullStr = CString nullAddr

marshallString :: String -> IO CString
marshallString s = marshallString' s (length s)

marshallString' :: String -> Int -> IO CString
marshallString' s n = 
  do a <- mallocElems (head s) (n+1)
     zipWithM_ (pokeElemOff a) [0..n-1] s
     pokeElemOff a n '\0'
     return (CString a)


unmarshallString :: CString -> IO String
unmarshallString (CString addr) = get 0
  where 
    get i =
      do c <- peekElemOff addr i
	 if c=='\0'
	  then return []
	  else (c:) <$> get (i+1)
              
unmarshallString' :: CString -> Int -> IO String
unmarshallString' (CString addr) n = get 0
  where
    get i =
      if i<n
      then (:) <$> peekElemOff addr i <*> get (i+1)
      else return []
     

---

-- | Pointer to long int (same size as pointers, 64 bits on 64-bit systems)
newtype CLong = CLong Addr
newtype CXID = CXID Addr

instance HasAddr CLong where addrOf (CLong a) = a --;atAddr = CLong
instance HasAddr CXID where addrOf (CXID a) = a --;atAddr = CLong

instance IsPtr CLong where
  nullPtr = CLong nullAddr
  newPtr = CLong <$> mallocElem (0::Int)
  newArray n = CLong <$> mallocElems (0::Int) n

instance IsPtr CXID where
  nullPtr = CXID nullAddr
  newPtr = CXID <$> mallocElem (0::Int)
  newArray n = CXID <$> mallocElems (0::Int) n

instance CVar CLong Int

-- | Pointer to C int (32 bits even on 64-bit system)
newtype CInt32 = CInt32 Addr

instance HasAddr CInt32 where addrOf (CInt32 a) = a --;atAddr = CInt

instance IsPtr CInt32 where
  nullPtr = CInt32 nullAddr
  newPtr = CInt32 <$> mallocElem (0::Int32)
  newArray n = CInt32 <$> mallocElems (0::Int32) n

instance CVar CInt32 Int32
