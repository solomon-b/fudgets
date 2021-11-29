module DStringF(
  stringF,stringF',--stringF'',
  passwdF,passwdF',passwdF'',
  intF,intF',intF''
  ,stringInputF,intInputF,passwdInputF
  ,stringInputF',intInputF',passwdInputF'
  ) where
import FDefaults
import StringF
--import Fudget
import CompOps
--import Geometry(Rect)
--import Xtypes
--import SpEither(filterRightSP)
import InputMsg(InputMsg,mapInp)
import InputSP(inputDoneSP)--InF(..),
import EitherUtils(mapEither)
import Data.Char(isDigit)

stringInputF = stringInputF' standard
intInputF = intInputF' standard
passwdInputF = passwdInputF' standard
stringInputF' pmod = inputDoneSP >^^=< stringF' pmod
intInputF' pmod = inputDoneSP >^^=< intF' pmod
passwdInputF' pmod = inputDoneSP >^^=< passwdF' pmod

stringF = stringF' standard
stringF' = noPF . stringF''

passwdF = passwdF' standard
passwdF' = noPF . passwdF''
passwdF'' = stringF'' . (.setShowString (map (const '*')))

intF = intF' standard
intF' = noPF . intF''

intF'' :: (Customiser StringF) -> PF StringF Int (InputMsg Int)
intF'' pmod = mapInp read' >^=<
              stringF'' (pmod.pm) >=^<
	      mapEither id show
  where
    pm = setAllowedChar isDigit -- . setInitSize "1999999999"
    read' "" = 0
    read' s = read s
