{-# LANGUAGE CPP #-}
module IOUtil(-- ** Some utilities that are a little dirty, but not very
              getEnvi, progName, progArgs,
              -- ** Backward compatibility
              IOUtil.catch, try, getModificationTime) where

import qualified Control.Exception as E
import qualified System.Directory as D(getModificationTime)
import System.Environment(getEnv,getProgName,getArgs)
#ifdef VERSION_old_time
import Data.Time(UTCTime)
import Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import System.Time(ClockTime(..))
#endif
import UnsafePerformIO(unsafePerformIO)

getEnvi :: String -> Maybe String
getEnvi s = either (const Nothing) Just $ unsafePerformIO $ try (getEnv s)

progName :: String
progName = unsafePerformIO getProgName

progArgs :: [String]
progArgs = unsafePerformIO getArgs

-- * GHC 6.12-7.6 compatibility
catch = E.catch :: IO a -> (IOError -> IO a) -> IO a
try   = E.try   :: IO a -> IO (Either IOError a)

#ifdef VERSION_old_time
getModificationTime path = toClockTime `fmap` D.getModificationTime path

class    ToClockTime a         where toClockTime :: a -> ClockTime
instance ToClockTime ClockTime where toClockTime = id
instance ToClockTime UTCTime   where 
    toClockTime = flip TOD 0 . floor . realToFrac . utcTimeToPOSIXSeconds
#else
getModificationTime path = D.getModificationTime path
#endif
