module QuitF(quitIdF, quitF) where
import FudgetIO
import HaskellIO
import NullF
import DialogueIO hiding (IOError) -- Exit

--quitF :: F a b
quitF =
    getHigh $ \ _ ->
    hIOSuccF (Exit 0) $
    nullF

--quitIdF :: (a -> Bool) -> F a a
quitIdF p =
    getHigh $ \ msg ->
    (if p msg
     then hIOSuccF (Exit 0)
     else putHigh msg) $
    quitIdF p
