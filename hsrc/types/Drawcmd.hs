module Drawcmd(move, moveDrawCommands, moveDrawCommand) where
import DrawTypes
import Geometry(Move(..))
--import Xtypes

instance Move DrawCommand where move = flip moveDrawCommand

moveDrawCommand cmd v =
    case cmd of
      DrawLine l -> DrawLine (move v l)
      DrawLines cm ps -> DrawLines cm (movePoints cm ps v)
      DrawImageString p s -> DrawImageString (move v p) s
      DrawString p s -> DrawString (move v p) s
      DrawRectangle r -> DrawRectangle (move v r)
      FillRectangle r -> FillRectangle (move v r)
      FillPolygon shape coordMode ps ->
	FillPolygon shape coordMode (movePoints coordMode ps v)
      DrawArc r a1 a2 -> DrawArc (move v r) a1 a2
      FillArc r a1 a2 -> FillArc (move v r) a1 a2
      CopyArea d r p -> CopyArea d r (move v p)
      CopyPlane d r p n -> CopyPlane d r (move v p) n
      DrawPoint p -> DrawPoint (move v p)
      CreatePutImage r fmt pxls -> CreatePutImage (move v r) fmt pxls
      DrawImageStringPS p s -> DrawImageStringPS (move v p) s
      DrawStringPS p s -> DrawStringPS (move v p) s
      DrawImageString16 p s -> DrawImageString16 (move v p) s
      DrawString16 p s -> DrawString16 (move v p) s

movePoints cm ps v =
 case cm of
   CoordModeOrigin -> move v ps
   CoordModePrevious ->
     case ps of
       p:ps' -> move v p:ps'
       [] -> []

moveDrawCommands cmds p = map (`moveDrawCommand` p) cmds
