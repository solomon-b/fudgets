module CompOps where
import CompF(compF)
import ParF(parF)
import CompFfun
import Fudget
import SerCompF(serCompF)
import CompSP(serCompSP,compEitherSP)
import ParSP(parSP)

-- Change priority??
infixl 5 >+<
infixl 5 >*<
infixr 4 >==<
infixr 8 -==-, -+-, -*-
infixr 7 >^^=<
infixr 5 >..=<
infixl 6 >=^^<
infixl 6 >=..<
infixr 7 >^=<
infixr 6 >.=<
infixl 6 >=^<
infixl 6 >=.<

-- Infix operators for common stream processor combinators.

sp1 -==- sp2 = serCompSP sp1 sp2
sp1 -+- sp2 = compEitherSP sp1 sp2
sp1 -*- sp2 = parSP sp1 sp2

-- Infix operators for common fudget combinators.

w1 >+< w2 = compF w1 w2
w1 >*< w2 = parF w1 w2
w1 >==< w2 = serCompF w1 w2

(>^^=<) :: (SP a b) -> (F e a) -> F e b
f >^^=< w = postProcessHigh f w
(>=^^<) :: (F c d) -> (SP e c) -> F e d
w >=^^< f = preProcessHigh w f
(>^=<) :: (a -> b) -> (F e a) -> F e b
f >^=< w = postMapHigh f w
(>=^<) :: (F c d) -> (e -> c) -> F e d
w >=^< f = preMapHigh w f

f >..=< w = postProcessLow f w
w >=..< f = preProcessLow w f
f >.=< w = postMapLow f w
w >=.< f = preMapLow w f
