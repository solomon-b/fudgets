module FUtil(module FUtil,F) where
import Fudgets

ignoreI :: F a b -> F aa b
ignoreI f = f >=^< (\x->error "ignoreI") 
ignoreO :: F a b -> F a bb
ignoreO f = (\x->error "ignoreO") >^=< f

stripLeftF = concatMapF g where g (Left x) = [x]; g _ = []
stripRightF = concatMapF g where g (Right x) = [x]; g _ = []

{-
debugF :: (a ->String) -> (b->String) -> String -> F a b -> F a b
debugF iShow oShow title f =
	let disp s = labLeftOfF s displayF
	    it = disp "Input: "
	    im = disp "                              "
	    ot = disp "Output:"
	    om = im
	    ts = ignoreI (untaggedListLF verticalP [it, ot])
	    ms = untaggedListLF verticalP [im, om] >=^< (\x->case x of { Right x -> (0,iShow x); Left y -> (1,oShow y)})
	    box = untaggedListLF horizontalP [ts, ms] >=^< (\x->(1,x))
	    shell = shellF ("Debug - "++title) box
	    tb :: F a b -> F a (Either b a)
	    tb f = idRightF f >==< toBothF
--	    g (Right (Left x)) = [x]
--	    g _             = []
	in  stripLeftF >==< stripRightF >==< tb shell >==< tb f

debugShowF t f = debugF show show t f
-}

intWF :: Int -> Int -> F Int (InputMsg Int)
intWF s n = startupF [n] intF

