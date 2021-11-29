module SPstateMonad where
import SP(SP)
import Spops
import StateMonads(Mk(..),Ms(..),toMs,toMsc,loadMs,storeMs)

type SPms i o s ans = Ms (SP i o) s ans

putsSPms :: [o] -> SPms i o s ()
putSPms :: o -> SPms i o s ()
getSPms :: SPms i o s i
nullSPms :: SPms i o s ()
loadSPms :: SPms i o s s
storeSPms :: s -> SPms i o s ()
stateMonadSP :: s -> SPms i o s ans -> (ans -> SP i o) -> SP i o

putsSPms  = toMsc . putsSP
putSPms   = toMsc . putSP
getSPms   = toMs getSP
nullSPms  = return ()
loadSPms  = loadMs
storeSPms = storeMs

stateMonadSP s0 (Mk spm) sp = spm (\ans state->sp ans) s0
