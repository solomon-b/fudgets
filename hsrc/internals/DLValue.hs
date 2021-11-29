{-# LANGUAGE CPP #-}
module DLValue where

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
data DLValue = DLValue (forall a. a)
#elif defined(__NHC__) || defined(__PFE__)
-- not supported
data DLValue = DLValue
#else
data DLValue = DLValue a 
#endif

instance Show DLValue where
   showsPrec i d = ("DLValue"++)

instance Read DLValue
