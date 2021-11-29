module IdempotSP where
import Spops(getSP,putSP)
import SP(SP)

idempotSP :: Eq a => SP a a
idempotSP =
    getSP $ \ x ->
    putSP x $
    idempotSP' x
  where
    idempotSP' x =
      getSP $ \ x' ->
      (if x'==x
       then id
       else putSP x') $
      idempotSP' x'

