module WriteFile where
import HaskellIO(haskellIOF)
--import CompOps((>^=<))
import Cont(contMap)
--import NullF
import DialogueIO hiding (IOError)

writeFileF = writeFileF' WriteFile
writeXdgFileF = writeFileF' . WriteXdgFile

writeFileF' write = contMap wr
    where
      wr (file,contents) cont =
        haskellIOF (write file contents) $ \ resp ->
	cont (file,post resp)

      post (Failure e) = Left e
      post Success = Right ()
