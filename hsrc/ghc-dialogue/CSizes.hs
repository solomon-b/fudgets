{-# LANGUAGE CPP #-}
module CSizes where

#define CSIZE(ctype) foreign import ccall "cfuns.h" fudsizeof_/**/ctype :: Int

#include "csizes.h"
