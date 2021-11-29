module QuitButtonF where
import Spacer(marginHVAlignF)
import Alignment
import DButtonF
import FDefaults
import CompOps((>==<))
--import Defaults(buttonFont)
--import Event(Event(..))
--import Fudget
--import PushButtonF
import QuitF
import AuxTypes(Modifiers(..))
import Graphic( )

quitButtonF =
    quitF >==<
    marginHVAlignF 0 aRight aBottom
    (buttonF' (setKeys [([Mod1], "q")]) "Quit")


