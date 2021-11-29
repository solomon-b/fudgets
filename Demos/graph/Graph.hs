import Fudgets
import PlotF
import ExpF
import Exp(Cmd(..))
import Eval
import Root
import Show(showf,showp)

main = fudlogue $ shellF' (setSizing Static) "Graph" $
       if argFlag "namel" True
       then nameLayoutF layout mainF
       else mainF

layout = v [l f,
	    l gr,
	    h [v [--l p,
		  l a]{-,l q-}]]
  where --al = alignSepNL 0 aCenter aCenter
	h = hBoxNL
	v = vBoxNL
	l = leafNL

f:gr:a:p:q:_ = map show [(1::Int)..]
        
mainF = --nameF q (stubF quitButtonF)>==<
        (nameF a showareaF>+<nullF{-nameF p showpointF-})>==<
	nameF gr grF>==<
	nameF f funcF

funcF =(Left . map evalr)>^=<expF
  where
    evalr cmd =
      case cmd of
        Fun e -> RFunc (eval e,root e)
	Approx a n e -> RLines a n (eval e)

grF = plotF Nothing

showareaF = displayF >=^<showarea
--showpointF = displayF >=^<showp

showarea (xa,(y1,y2)) =
	"x: " ++ showinterval xa ++ ", y: " ++ showinterval (y2,y1)

showinterval (a,b) = "[" ++ showf a ++ ", " ++ showf b ++ "]"

