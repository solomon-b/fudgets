module StdIoUtil(linesSP, inputLinesSP, echoK, echoStderrK,
          appendChanK, appendStdoutK, appendStderrK,
          outputF, stdioF, stderrF, stdoutF, stdinF) where
--import Command
import CompOps((>==<))
import CompSP(serCompSP)
--import Event
import HaskellIO(hIOSucc)
import Srequest
import IoF
import Message(message)
import Transceivers(receiverF)
import Sockets
import Spops
--import Xtypes
import Fudget
--import FudgetIO
import NullF(getK{-,F,K-})
import ContinuationIO(stdout,stderr)
import DialogueIO hiding (IOError)

stdoutF = outputF stdout
stderrF = outputF stderr
stdioF = stdinF >==< stdoutF

outputF :: String -> F String a
outputF = ioF . outputK

stdinF = sIO GetStdinSocket $ \ (Socket s) -> receiverF s

{-
outputK chan =
    let f msg =
            case msg of
              High s -> [Low (DoIO (AppendChan chan s))]
              _ -> []
    in  concmapSP f
-}

outputK chan =
  getK $ message (const $ outputK chan) $ \ s ->
  appendChanK chan s $
  outputK chan

appendChanK chan s = hIOSucc (AppendChan chan s)

appendStdoutK s = appendChanK stdout s
appendStderrK s = appendChanK stderr s
echoK         s = appendStdoutK (s ++ "\n")
echoStderrK   s = appendStderrK (s ++ "\n")

linesSP = lnSP []
  where
    lnSP acc =
      getSP $ \msg ->
      case msg of
        '\n' -> putSP (reverse acc) (lnSP [])
	c    -> lnSP (c : acc)

inputLinesSP = linesSP `serCompSP` concatSP -- inefficient!!!
