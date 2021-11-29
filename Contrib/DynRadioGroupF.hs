module DynRadioGroupF where
import Fudgets

dynRadioGroupF alts startalt = dynRadioGroupF' standard alts startalt

dynRadioGroupF' pm alts startalt =
    loopThroughRightF (mapstateF ctrl (alts,startalt))
                      (dynF (rgF alts startalt))
  where
    rgF = radioGroupF' pm

    ctrl (alts,current) = either fromRadioGroupF fromOutside
      where
        fromRadioGroupF choice = ((alts,choice),[Right (alts,choice)])
	fromOutside (alts',current') =
	  ((alts',current'),
	   if map fst alts' == map fst alts
	   then [Left (Right current')]
	   else [Left (Left (rgF alts' current'))])
