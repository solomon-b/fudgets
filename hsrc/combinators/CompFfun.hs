module CompFfun where
import CompSP
import Fudget
import Message
--import Path(Path(..))
--import SP

postProcessHigh postsp (F sp) = F{-ff-} (postProcessHigh' postsp sp)
postProcessLow postsp (F sp) = F{-ff-} (postProcessLow' postsp sp)
preProcessHigh (F sp) presp = F{-ff-} (preProcessHigh' sp presp)
preProcessLow (F sp) presp = F{-ff-} (preProcessLow' sp presp)
preMapHigh (F sp) pre = F{-ff-} (preMapHigh' sp pre)
postMapHigh post (F sp) = F{-ff-} (postMapHigh' post sp)
preMapLow (F sp) pre = F{-ff-} (preMapLow' sp pre)
postMapLow post (F sp) = F{-ff-} (postMapLow' post sp)

postProcessHighK postsp (K sp) = K{-kk-} (postProcessHigh' postsp sp)
postProcessLowK postsp (K sp) = K{-kk-} (postProcessLow' postsp sp)
preProcessHighK (K sp) presp = K{-kk-} (preProcessHigh' sp presp)
preProcessLowK (K sp) presp = K{-kk-} (preProcessLow' sp presp)
preMapHighK (K sp) pre = K{-kk-} (preMapHigh' sp pre)
postMapHighK post (K sp) = K{-kk-} (postMapHigh' post sp)
preMapLowK (K sp) pre = K{-kk-} (preMapLow' sp pre)
postMapLowK post (K sp) = K{-kk-} (postMapLow' post sp)

postProcessHigh' :: (SP a b) -> (Fa c d e a) -> Fa c d e b
postProcessHigh' p f = serCompSP (idLowSP p) f

postProcessLow' :: (SP a b) -> (Fa c a d e) -> Fa c b d e
postProcessLow' p f = serCompSP (idHighSP p) f

preProcessHigh' :: (Fa a b c d) -> (SP e c) -> Fa a b e d
preProcessHigh' f p = serCompSP f (idLowSP p)

preProcessLow' :: (Fa a b c d) -> (SP e a) -> Fa e b c d
preProcessLow' f p = serCompSP f (idHighSP p)

preMapHigh' :: (Fa a b c d) -> (e -> c) -> Fa a b e d
preMapHigh' f pre = preMapSP f (aHigh pre)

preMapLow' :: (Fa a b c d) -> (e -> a) -> Fa e b c d
preMapLow' f pre = preMapSP f (aLow pre)

postMapHigh' :: (a -> b) -> (Fa c d e a) -> Fa c d e b
postMapHigh' post f = postMapSP (aHigh post) f

postMapLow' :: (a -> b) -> (Fa c a d e) -> Fa c b d e
postMapLow' post f = postMapSP (aLow post) f

prepostMapHigh' :: (a -> b) -> (c -> d) -> (Fa e f b c) -> Fa e f a d
prepostMapHigh' pre post f = prepostMapSP (aHigh pre) (aHigh post) f


prepostMapHigh pre post (F sp) = F{-ff-} (prepostMapHigh' pre post sp)
prepostMapHighK pre post (K sp) = K{-kk-} (prepostMapHigh' pre post sp)

prepostMapLow' :: (a -> b) -> (c -> d) -> (Fa b c e f) -> Fa a d e f
prepostMapLow' pre post f = prepostMapSP (aLow pre) (aLow post) f

prepostMapLow pre post (F sp) = F (prepostMapLow' pre post sp)
prepostMapLowK pre post (K sp) = K (prepostMapLow' pre post sp)
