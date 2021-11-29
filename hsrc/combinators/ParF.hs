module ParF(parF) where
--import Fudget
import EitherUtils(stripEither)
import CompF(compF)
--import SerCompF(serCompF)
import CompFfun(postMapHigh,preProcessHigh)
import SpEither(toBothSP)

-- Quick (i.e. slow) implementation
parF f1 f2 = stripEither `postMapHigh` compF f1 f2 `preProcessHigh` toBothSP
