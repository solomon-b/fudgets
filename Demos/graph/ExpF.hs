module ExpF(expF) where
import Fudgets
import Show
import Parser
import Lex
import Diff
import Exp(Exp)

expF =
    noStretchF False True $
    loopLeftF (toBothF>==<((throughExpInputF,LeftOf)>#==<diffF))
    --loopLeftF (toBothF>==<(throughExpInputF>==<diffF))

throughExpInputF = stripEither >^=< throughF expInputF

expInputF =
	concatMapF parse>==<
	(inputDoneSP>^^=<spacerF vCenterS (labLeftOfF "f(x)=" stringF))>=^<
	show_Cmds

parse s =
	case (cmds . lex') s of
	   Right (e,[])-> [e]
	   _ -> []	-- should generate error message

diffF =	map diffCmd>^=<
		(bufSP>^^=<
		(idF>+<buttonF "Derivate"))

bufSP =
  let buf b =
        getSP $ \x->
	case x of
         Left d -> buf (Just d)
         Right _ -> case b of
		    Nothing -> buf b
		    Just d -> putSP d $ buf b
  in buf Nothing




