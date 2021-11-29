module Main where
import Fudgets
import GraphF

main = fudlogue $ shellF "FudPaint" mainF

mainF = hBoxF (graphF >==< vBoxF toolscolorsF)

toolscolorsF =
    noStretchF True True $ spacerF centerS $ vBoxF $
    stripEither >^=< (toolsF >+< colorsF)

toolsF =
    "Tools" `labAboveF`
    ((ChangeTool. tool)>^=< radioGroupF' pm (map (pairwith pict) tools) LineTool)
  where pm = setLabelInside True

colorsF =
    "Colors" `labAboveF`
    (ChangeColor >^=< radioGroupF' pm (map col colors) "Black")
  where col c = (c,stackD [fgD c (g (cpict BlockTool)),
                           g (cpict RectTool)])
        pm = setLabelInside True . setPlacer (matrixP' 3 Horizontal 0)

tools = [LineTool, RectTool, BlockTool, OvalTool, FilledOvalTool]

colors = [ "Black", "Grey", "White",
           "Blue", "Blue3", "Blue4",
           "Green", "Green3", "Green4",
           "Yellow","Orange", "Brown",
 	   "Red", "Red3", "Red4" ]

data Tool = LineTool | RectTool | BlockTool | OvalTool | FilledOvalTool
            deriving (Eq)

tool LineTool = drawline
tool RectTool = drawrect
tool BlockTool = fillrect
tool OvalTool = drawoval
tool FilledOvalTool = filloval

pict = pict' (Point 30 15)
cpict = pict' (Point 15 15)
pict' s t = FixD (s+1) [tool t 0 (s-1)]

{-
pict LineTool = "Line"
pict RectTool = "Rect"
pict BlockTool = "Block"
pict OvalTool = "Oval"
pict FilledOvalTool = "Blob"
-}

drawline p0 p1 = DrawLine (Line p0 p1)
drawrect p0 p1 = DrawRectangle (rect' p0 p1)
fillrect p0 p1 = FillRectangle (rect' p0 p1)
drawoval p0 p1 = DrawArc (rect' p0 p1) 0 (64*360)
filloval p0 p1 = FillArc (rect' p0 p1) 0 (64*360)

rect' p1 p2 =
  let p0 = pmin p1 p2
      p = pmax p1 p2
  in Rect p0 (padd (psub p p0) (Point 1 1))
