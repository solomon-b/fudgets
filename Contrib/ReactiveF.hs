-- | Program stateful abstract fudgets in a monadic style
module ReactiveF(reactiveF,reactiveSP,module ReactionM) where
import Fudgets
import ReactionM

reactiveF  = mapstateF  . mf
reactiveSP = mapstateSP . mf

mf rM s m = react (rM m) s
