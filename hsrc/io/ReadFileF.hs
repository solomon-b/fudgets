module ReadFileF(readFileF,readBinaryFileF,readXdgFileF,readDirF) where
import Fudget() -- synonym KEvent, for hbc
import NullF
import FudgetIO
import IoF(ioF)
import HaskellIO(haskellIO)
import Message(Message(..))
import DialogueIO hiding (IOError)

readFileF = ioF readFileK
readBinaryFileF = ioF readBinaryFileK
readXdgFileF = ioF . readXdgFileK

readFileK = readFileK' ReadFile
readBinaryFileK = readFileK' ReadBinaryFile
readXdgFileK = readFileK' . ReadXdgFile

readFileK' req =
  getK $ \ msg ->
  case msg of
    High filename ->
      haskellIO (req filename) $ \ resp ->
      putHigh (filename,case resp of
                          Str s -> Right s
		          Failure err -> Left err)
      readFileK
    _ -> readFileK

readDirF = ioF readDirK

readDirK =
  getK $ \ msg ->
  case msg of
    High dirname ->
      haskellIO (ReadDirectory dirname) $ \ resp ->
      putHigh (dirname,case resp of
		         StrList filenames -> Right filenames
			 Failure err -> Left err)
      readDirK
    _ -> readDirK
