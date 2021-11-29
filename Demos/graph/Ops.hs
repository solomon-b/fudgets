module Ops(Bop(..),Uop(..),unops,addops,mulops,binoplex,binopfun,unoplex,unopfun,
	show_Bop,show_Uop,addopt,mulopt{-,powopt-})
where

data Bop = Badd | Bsub | Bmul | Bdiv | Bmin -- | Bpow
           deriving (Eq)

data Uop = Uneg | Usin | Ucos | Utan | Uexp | Ulog | Uln | Usqrt |
	   Uabs | Ufloor | Uceil | Uround | Utanh | Ucosh | Usinh | Ugamma |
	   Uatan | Uacos | Uasin | Usgn | Utheta
		deriving (Eq)

assocdef x xs e = maybe e id $ lookup x xs

binoplex binop = fst (assocdef binop binoptab (error "undefined binop"))
binopfun binop = assocdef binop (map snd binoptab) (error "undefined binop")

binoptab = addopfuncs ++ mulopfuncs -- ++ powopfuncs

addopfuncs =
  [("+",(Badd,(+))),
   ("-",(Bsub,(-)))
  ]

mulopfuncs =
  [("*",(Bmul,(*))),
   ("/",(Bdiv,(/))),
   ("!",(Bmin,min))
  ]

{-
powopfuncs =
  [("^",(Bpow,pow))
  ]
-}

addops = map fst addopfuncs
mulops = map fst mulopfuncs
--powops = map fst powopfuncs

addopt = map (fst . snd) addopfuncs
mulopt = map (fst . snd) mulopfuncs
--powopt = map (fst . snd) powopfuncs

unoplex unop = fst (assocdef unop unoptab (error "undefined unop"))
unopfun unop = assocdef unop (map snd unoptab) (error "undefined unop")

unoptab =
  [("-",(Uneg,negate)),
   ("sin",(Usin,sin)),
   ("cos",(Ucos,cos)),
   ("tan",(Utan,tan)),
   ("exp",(Uexp,exp)),
   ("log",(Ulog,log)),
   ("ln",(Uln,log)),
   ("log",(Uln,log)),
   ("sqrt",(Usqrt,sqrt)),
   ("fabs",(Uabs,abs)),
   ("abs",(Uabs,abs)),
   ("floor",(Ufloor,fromIntegral.floor)),
   ("round",(Uround,fromIntegral.round)),
   --("ceil",(Uceil,ceil)),
   ("tanh",(Utanh,tanh)),
   ("cosh",(Ucosh,cosh)),
   ("sinh",(Usinh,sinh)),
   --("gamma",(Ugamma,gamma)),
   ("atan",(Uatan,atan)),
   ("acos",(Uacos,acos)),
   ("asin",(Uasin,asin)),
   ("sgn",(Usgn,sgn)),
   ("theta",(Utheta,theta))
  ]

theta :: Double->Double
theta x = if x<0.0 then 0.0 else 1.0
sgn x = theta x - theta (0.0-x)

--pow x y = exp (y * log x)
--pow x y = x**y
--pow x y = x^^y

unops = map fst unoptab

show' tab x = assocdef x (map (\(x,(y,_))->(y,x)) tab) (error "show' in Ops.hs")

show_Uop = show' unoptab
show_Bop = show' binoptab
