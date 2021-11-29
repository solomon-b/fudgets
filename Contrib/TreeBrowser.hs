module TreeBrowser(Tree(..),treeBrowserF',treeDisplayF') where
import AllFudgets hiding (Tree(..))
import qualified ReactiveF as R

data Tree leaf node
  = Leaf leaf
  | Node node [Tree leaf node]
  deriving (Show)


--treeBrowserF = treeBrowserF standard

treeDisplayF' t = treeBrowserF'' drawStaticTree t
treeBrowserF' t = treeBrowserF'' drawTree t

--treeBrowserF' ::Tree n l -> F (Tree n l) ([n],Maybe l)
treeBrowserF'' drawTree t =
    wCreateGCtx rootGCtx (gcFgA linecolor) $ \ lgc ->
    wCreateGCtx rootGCtx (gcFgA paperColor) $ \ bggc ->
    treeBrowserF''' drawTree (bggc,lgc) t


treeBrowserF''' drawTree gcs@(bggc,lgc) t =
   loopThroughRightF (R.reactiveF ctrl d0) (graphicsDispF' pm)
  where
    pm1 :: Graphic a => Customiser (GraphicsF a)
    pm1 = setBgColor bgColor . setSizing Dynamic
    pm = pm1 . setInitDisp d0
    d0 = drawTree gs t
    ctrl = either fromLoop fromOutside
      where
	fromOutside t = do let d = drawTree gs t 
			   R.set d
			   R.put (toLoop $ replaceAllGfx d)

	fromLoop gfxevent =
	  do (path,lbl) <- clickedpart gfxevent
	     case lbl of
	       Left vis -> toggle path (not vis)
	       Right part -> R.put (toOutside part)

	toggle path vis =
	  do (ppath,LabelD rt@(Right (Node n _))
	                   (ComposedD _ (_:ds))) <- parentpathpart path
	     let td' = LabelD rt (nodeD gs vis n ds)
	     R.update $ \ d -> replacePart d ppath td'
	     R.put (toLoop $ replaceGfx ppath td')

    toLoop = Left
    toOutside = Right

    --gd = softAttribD [GCLineStyle LineOnOffDash] . g
    --gd x = g x
    gl = hardAttribD lgc . g
    gbg = hardAttribD bggc . g
    gs = (gbg,gl)

clickedpart gfxevent =
  case gfxevent of
    GfxButtonEvent{gfxType=Pressed, gfxPaths=(path,_):_} -> pathlbl path
    _ -> R.rfail

pathlbl path =
  do (lpath,LabelD lbl _) <- pathpart path
     return (lpath,lbl)

pathpart path =
  do drawing <- R.get
     let lpath = drawingAnnotPart drawing path
     part <- R.lift $ maybeDrawingPart drawing lpath
     return (lpath,part)

parentpathpart = pathpart . up

drawStaticTree gs t = placedD (verticalLeftP' 0) $ drawTree' gs Nothing t
drawTree gs t = placedD (verticalLeftP' 0) $ drawTree' gs opendepthlimit t

staticNodeD gs n ds = boxD (hboxD' 0 [sframeD 3 gs (g n)]:ds)

nodeD gs@(gbg,gl) vis n ds =
    boxVisibleD vcnt (hboxD' 0 [sframeD 3 gs (g n),gl hLineFD,markD]:ds)
  where
    vcnt = 1+(if vis then length ds else 0)
    markD = labelD (Left vis) $ circleD (if vis then g "-" else g "+")

    circleD d =
        spacedD centerS $
	stackD [gbg filledEllipse,gl ellipse,spacedD sqpadS d]
      where sqpadS  = resizeS sq `compS` centerS
	    sq (Point w h) = Point m m where m = max w h

sframeD sep gs d = spacedD (vMarginS sep 0) $ frameD gs d

frameD (gbg,gl) d =
   stackD [gbg $ filler False False 1,gl frame,padD 2 d]


nodeD' gs Nothing = staticNodeD gs
nodeD' gs (Just d) = nodeD gs (d>0)

drawTree' gs@(_,gl) depth t =
    spacedD leftS $
    labelD (Right t) $
    case t of
      Leaf l -> --hboxD' 0 [gl hLineFD,frameD gs $ g l]
		sframeD 1 gs $ g l
      Node n ts -> nodeD' gs depth n [drawTrees gs (fmap (+(-1)) depth) ts]

drawTrees gs@(_,gl) depth ts = vboxlD' 0 $ zipWith drawSubTree [n-1,n-2..0] ts
  where
    n = length ts
    drawSubTree i t = placedD (tableP' 2 Vertical 0) $
		      boxD [fork i,line i,drawTree' gs depth t]
    --drawSubTree i t = hboxD' 0 [fork i,drawTree' gs depth t]
    fork 0 = gl lowerRightFD
    fork _ = gl forkRightFD
    line 0 = blankD 0
    line _ = gl vLineFD


---

lowerRightFD = flex' (pP 14 10) f
  where f (Rect p s) =
	    [DrawLines CoordModePrevious [p+pP mw 0,pP 0 mh,pP mw 0]]
	  where Point mw mh = rectMiddle (Rect 0 s)

forkRightFD = flex' (pP 14 10) f
  where f (Rect p s@(Point w h)) =
	    [DrawLine (Line (p+pP mw 0) (p+pP mw h)),
	     DrawLine (Line (p+m) (p+pP w mh))]
	  where m@(Point mw mh) = rectMiddle (Rect 0 s)

vLineFD = flex' (pP 10 1) f
  where f (Rect p s@(Point _ h)) = [DrawLine (Line (p+pP mw 0) (p+pP mw h))]
	  where Point mw _ = rectMiddle (Rect 0 s)

hLineFD = flex' 10 f
  where f (Rect p s@(Point w _)) = [DrawLine (Line (p+pP 0 mh) (p+pP w mh))]
	  where Point _ mh = rectMiddle (Rect 0 s)

linecolor = argKey "linecolor" "blue"
opendepthlimit = argReadKey "opendepthlimit" (Just opendepth) -- not a good name
opendepth = argReadKey "opendepth" 1::Int
