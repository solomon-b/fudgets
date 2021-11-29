{-# LANGUAGE CPP #-}
module UnsafeGetDLValue where
import DLValue

--unsafeDLValue :: DLValue -> anytype
#if defined(__NHC__) || defined(__PFE__)
unsafeGetDLValue DLValue = error "unsafeGetDLValue not implemented yet"
#else
unsafeGetDLValue (DLValue v) = v
#endif

-- This is unsafe, because the response from DLSym contains a DLValue of
-- a particular type. unsafeGetDLValue allows it to be used at any type.
