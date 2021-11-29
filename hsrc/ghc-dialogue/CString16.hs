module CString16 where
import Marshall

-- A quick hack...

type CString16 = CString -- !!

marshallString16 s = marshallString16' s (length s)

marshallString16' s n = marshallString' (split s) (2*n)
  where
    split [] = []
    split (c:cs) = toEnum (i `div` 256):toEnum (i `mod` 256):split cs
      where i = fromEnum c

-- unmarshallString16 = ...
