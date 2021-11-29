module HyperGraphicsF where
--import Fudget
--import Defaults(paperColor)
--import InputMsg(inputDone)
import Utils(swap)
--import Xtypes(ColorName)
import Loops(loopThroughRightF)
import SerCompF(mapstateF)
--import Graphic
import Drawing
import DrawingOps
import GraphicsF
import FDefaults
import HbcUtils(assoc)
import Sizing(Sizing(..))
--import GCAttrs() -- instances
import Event(Pressed(..))
--import Maptrace(ctrace) -- debugging
--import SpyF(teeF) -- debugging
--import CompOps((>==<)) -- debugging

hyperGraphicsF x = hyperGraphicsF' standard x

{-
hyperGraphicsF' :: (Eq annot,Graphic g) =>
                  Customiser (GraphicsF (Drawing annot g)) ->
                  Drawing annot g ->
		  F (Either (Drawing annot g) (annot,Drawing annot g))
		    annot
-}
hyperGraphicsF' custom init =
    loopThroughRightF
	(mapstateF ctrl state0)
	({-teeF show "\n" >==<-} graphicsDispF' (custom . params))
  where
    --tr x = ctrace "hyper" (show x) x -- debugging
    params = setInitDisp init .
	     setGfxEventMask [GfxButtonMask] .
	     setSizing Dynamic
    state0 = (annotPaths init,init)

    ctrl state@(paths,drawing) = either input output
      where
        same = (state,[])
        output = either new newpart
	  where
	    --new d = newpart' d []
	    -- avoid space leak for this common case:
	    new d = ((annotPaths d,d),[Left (replaceAllGfx d)])
	    newpart (a,d) = assoc (newpart' d) same paths a
	    newpart' d path = ((paths',drawing'),[Left (replaceGfx path d)])
	      where drawing' = replacePart drawing path d
	            paths' = annotPaths drawing'
			-- Space leak: drawing' isn't used until user clicks
			-- in the window, so the old drawing is retained in
			-- the closure for drawing'

        input msg =
	    case msg of
	      GfxButtonEvent { gfxType=Pressed, gfxPaths=gfxPaths } ->  mouse gfxPaths
	      _ -> same
	  where
	    lblPart = maybeDrawingPart drawing . drawingAnnotPart drawing . fst
	    mouse paths =
	      --ctrace "hyper" (show paths) $
	      case [lbl|Just (LabelD lbl _)<-map lblPart paths] of
		lbl:_ -> (state,[Right lbl])
{- -- All nodes have unique paths now, so this should not be necessary:
		    Just d ->
		      case annotChildren d of
		        ([],LabelD a _):_ -> (state,[Right a])
			_ -> same
-}
		_ -> same

    annotPaths = map swap . drawingAnnots
