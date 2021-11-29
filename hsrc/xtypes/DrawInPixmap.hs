module DrawInPixmap where
--import Geometry
import XDraw

-- convenient abbreviations for drawing in pixmaps:
pmDrawLine pm gc l = draw (Pixmap pm) gc (DrawLine l)
pmDrawLines pm gc mode ps = draw (Pixmap pm) gc (DrawLines mode ps)
pmDrawImageString pm gc p s = draw (Pixmap pm) gc (DrawImageString p s)
pmDrawString pm gc p s = draw (Pixmap pm) gc (DrawString p s)
pmDrawImageString16 pm gc p s = draw (Pixmap pm) gc (DrawImageString16 p s)
pmDrawString16 pm gc p s = draw (Pixmap pm) gc (DrawString16 p s)
pmDrawImageStringPS pm gc p s = draw (Pixmap pm) gc (DrawImageStringPS p s)
pmDrawStringPS pm gc p s = draw (Pixmap pm) gc (DrawStringPS p s)
pmDrawRectangle pm gc r = draw (Pixmap pm) gc (DrawRectangle r)
pmFillRectangle pm gc r = draw (Pixmap pm) gc (FillRectangle r)
pmFillPolygon pm gc shape mode ps =
   draw (Pixmap pm) gc (FillPolygon shape mode ps)
pmDrawArc pm gc r a1 a2 = draw (Pixmap pm) gc (DrawArc r a1 a2)
pmFillArc pm gc r a1 a2 = draw (Pixmap pm) gc (FillArc r a1 a2)
pmCopyArea dst gc src r p = draw (Pixmap dst) gc (CopyArea src r p)
pmCopyPlane dst gc src r p i = draw (Pixmap dst) gc (CopyPlane src r p i)
pmDrawPoint dst gc p = draw (Pixmap dst) gc (DrawPoint p)
pmCreatePutImage dst gc r s d = draw (Pixmap dst) gc (CreatePutImage r s d)
