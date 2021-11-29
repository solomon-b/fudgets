module SPmonad where
import SP(SP)
import ParSP(seqSP)
import Spops
import StateMonads(Mk(..),toMkc) --,bmk,toMs

type SPm i o ans = Mk (SP i o) ans

putsSPm :: [o] -> SPm i o ()
putsSPm = toMkc . putsSP

putSPm :: o -> SPm i o ()
putSPm = toMkc . putSP

getSPm :: SPm i o i
getSPm = Mk getSP

nullSPm :: SPm i o ()
nullSPm = return ()

monadSP :: (SPm i o ()) -> SP i o
monadSP (Mk spm) = spm (const nullSP)

toSPm :: (SP i o) -> SPm i o ()
toSPm sp = toMkc (seqSP sp)

