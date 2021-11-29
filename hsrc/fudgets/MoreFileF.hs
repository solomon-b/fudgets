module MoreFileF(
  moreFileShellF,
  moreShellF,moreShellF',
  moreFileF
) where
import MoreF(moreF')
import Command
import CompOps((>==<), (>=^<), (>^=<), (>=^^<))
import Shells(unmappedShellF, unmappedSimpleShellF)
import Fudget
import FRequest
import ReadFileF(readFileF)
--import Message(Message(..))
import SpEither(toBothSP)
import Spops(concmapSP)
--import Xtypes
import EitherUtils(stripEither)
import FDefaults
import InputMsg(InputMsg)
import DialogueIO() -- instances, for hbc


moreShellF' pmod name = unmappedSimpleShellF name $ moreF' pmod
moreShellF = moreShellF' standard

moreFileF :: F String (InputMsg (Int, String))
moreFileF = moreFileF' standard
moreFileF' pmod = moreF' pmod >=^< contents >==< readFileF

moreFileShellF =
  let startcmds = [XCmd $ StoreName "More Fudget"]
      titleK = K{-kk-} $ concmapSP changeName  -- titleK never outputs anything
  in stripEither >^=< unmappedShellF startcmds titleK moreFileF >=^^< toBothSP

contents (name,file) =
  case file of
    Right s -> lines s
    Left err -> [name++": "++show err] -- missing Show instance for IOError
    --Left err -> [name++": error"]

changeName msg =
  case msg of
    High name -> [Low $ XCmd $ StoreName name, Low $ XCmd MapRaised]
    _ -> []
