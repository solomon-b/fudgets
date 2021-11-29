module AppStorage where
import System.Directory(XdgDirectory(..))
import ReadFileF
import WriteFile
import HbcUtils(apSnd)
import CompOps
import NullF
import Spops

appStorageF :: (Read a,Show a) => String -> a -> F a a
appStorageF key d =
  either (const d) (maybe d id . readM) . snd
  >^=< startupF [key] (readXdgFileF XdgData)
  >=^^< nullSP
  >==< writeXdgFileF XdgData
  >=^< (,) key . show

readM x = case reads x of
            [(x,s)] | lex s == [("","")] -> Just x
            _ -> Nothing
