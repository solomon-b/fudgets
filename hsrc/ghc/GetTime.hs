{-# LANGUAGE CPP #-}
module GetTime where
import HaskellIO(hIO)
import DialogueIO

#ifdef VERSION_old_time
getTime      cont = hIO GetTime      $ \ (ClockTime t)    -> cont t
getLocalTime cont = hIO GetLocalTime $ \ (CalendarTime t) -> cont t
#endif

#ifdef VERSION_time
getCurrentTime cont = hIO GetCurrentTime $ \ (UTCTime t)   -> cont t
getZonedTime   cont = hIO GetZonedTime   $ \ (ZonedTime t) -> cont t
#endif