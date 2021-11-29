{-# LANGUAGE CPP #-}
module GetModificationTime where
import HaskellIO
import DialogueIO

#ifdef VERSION_old_time
getModificationTime path err cont =
    hIOerr (GetModificationTime path) err $ \ (ClockTime t) ->
    cont t
#else
getModificationTime path err cont =
    hIOerr (GetModificationTime path) err $ \ (UTCTime t) ->
    cont t
#endif
