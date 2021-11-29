module ShowFailure where

import qualified DialogueIO

showFailure :: DialogueIO.IOError -> String
showFailure = show
