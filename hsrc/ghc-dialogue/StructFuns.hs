{-# LANGUAGE CPP #-}
{- Obsolete OPTIONS -fvia-C -}
module StructFuns(module StructFuns,module CSizes) where
import MyForeign
import Marshall
import Xlib
import CSizes
import Xtypes(XID(..),Atom(..),Time,WindowId(..),Window)
import Visual(VisualID(..))
import HbcWord(Word)
import Data.Word(Word32)

--import GlaExts hiding (Word,(<#),Addr)

{-
#include "structs.h"
#include "cimports.h"
-}
#include "ccalls.h"

H_STRUCTTYPE(timeVal)
H_STRUCTTYPE(sockAddr)

type Cchar = Char
type Clong = Int
type Cshort = Int
