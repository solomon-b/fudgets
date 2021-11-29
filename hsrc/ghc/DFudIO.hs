{-# LANGUAGE CPP #-}
module DFudIO(Fudlogue,fudlogue,fudlogue'{-,fudIO,fudIO'-},HasCache(..)) where

import FDefaults
import FudIO(fudIO1)
--import FudIO2(fudIO2)
import Fudget
--import Xtypes
--import Cache(allcacheF)
import NewCache(allcacheF)
import CmdLineEnv(argFlag)

#include "defaults.h"

data Fudlogue = Pars [Pars]
data Pars = Cache Bool

parameter_class(Cache,Bool)
parameter_instance(Cache,Fudlogue)

fudlogue = fudlogue' standard
fudlogue' :: Customiser Fudlogue -> F a b -> IO ()
fudlogue' pmod f = fudIO1 (cache pmod f)
{-
fudIO = fudIO' standard
--fudIO' :: Customiser Fudlogue -> F a b -> IO ()
fudIO' pmod inCh outCh f = fudIO2 inCh outCh (cache pmod f)
-}

cache pmod = if getCache ps then allcacheF else id
  where ps = pmod (Pars [Cache usecache])

usecache = argFlag "cache" True
