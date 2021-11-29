module LoopCompF(loopCompF,loopCompSP,loopThroughBothF,loopThroughBothSP) where
import Fudget        
import CompFfun(prepostMapHigh)
import CompF(compF)
import CompSP(compSP,prepostMapSP)
import Loop(loopLeftSP)
import Loops(loopLeftF)

-- loopComp = symmetric version of loopThroughRight

loopCompF :: F (Either (Either r2l inl) (Either l2r inr))
               (Either (Either l2r outl) (Either r2l outr)) ->
	     F (Either inl inr) (Either outl outr)
loopCompF = loopLeftF . prepostMapHigh pre post

loopThroughBothF fud1 fud2 = loopCompF (fud1 `compF` fud2)
loopThroughBothSP sp1 sp2 = loopCompSP (sp1 `compSP` sp2)

loopCompSP = loopLeftSP . prepostMapSP pre post

post (Left  (Left  x)) = Left (Left x)
post (Left  (Right x)) = Right (Left x)
post (Right (Left x)) = Left (Right x)
post (Right (Right x)) = Right (Right x)
-- post = either (either (Left.Left) (Right.Left))  (either (Left.Right) (Right.Right))

pre (Right (Left x)) = Left (Right x)
pre (Right (Right x)) = Right (Right x)
pre (Left (Left x)) = Right (Left x)
pre (Left (Right x)) = Left (Left x)
