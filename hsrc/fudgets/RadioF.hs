module RadioF(radioF, oldRadioGroupF) where
import Spacer(noStretchF)
--import Alignment(Alignment(..))
--import ButtonGroupF
import CompOps((>==<), (>=^<))
import HbcUtils(lookupWithDefault)
--import Fudget
--import Geometry(Point, Rect, Size(..))
import LayoutF(listLF)
--import Placers
import Loops(loopLeftF)
import SerCompF(absF)
import Spops
import EitherUtils(stripEither)
import ToggleButtonF(oldToggleButtonF')
--import Xtypes
import Utils(pair)

radioF placer inside fname alts startalt =
  oldRadioGroupF placer inside fname (map fst alts) startalt (lookupWithDefault alts (error "radioF"))

oldRadioGroupF placer inside fname alts startalt show_alt =
    let radioAlts = radioButtonsF placer inside fname alts show_alt
        buttons = radioAlts >=^< stripEither
    in  loopLeftF (excludeF startalt >==< buttons) >=^< (`pair` True)

radioButtonsF placer inside fname alts show_alt =
  listLF placer (map radiobutton alts)
  where
     radiobutton alt =
        (alt, noStretchF False True $ 
              oldToggleButtonF' inside fname [] (show_alt alt))

excludeF start =
    absF (putsSP [Left (start, True)] (excl start))
  where
    excl last' =
      getSP $ \msg ->
      case msg of
	(new, False) -> if new == last'
			then putsSP [Left (new, True)] (cont new)
			else same
	(new, True)  -> if new == last'
		        then putsSP [Right new] (cont new)
		        else putsSP [Left (last', False), Right new] (cont new)
      where
        same = excl last'
	cont last'' = excl last''

