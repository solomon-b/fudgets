module DrawInWindow where
--import Geometry
import XDraw

-- convenient abbreviations for drawing in windows:
wDrawLine gc l = draw MyWindow gc (DrawLine l)
wDrawLines gc mode ps = draw MyWindow gc (DrawLines mode ps)
wDrawImageString gc p s = draw MyWindow gc (DrawImageString p s)
wDrawString gc p s = draw MyWindow gc (DrawString p s)
wDrawImageString16 gc p s = draw MyWindow gc (DrawImageString16 p s)
wDrawString16 gc p s = draw MyWindow gc (DrawString16 p s)
wDrawImageStringPS gc p s = draw MyWindow gc (DrawImageStringPS p s)
wDrawStringPS gc p s = draw MyWindow gc (DrawStringPS p s)
wDrawRectangle gc r = draw MyWindow gc (DrawRectangle r)
wFillRectangle gc r = draw MyWindow gc (FillRectangle r)
wFillPolygon gc shape mode ps = draw MyWindow gc (FillPolygon shape mode ps)
wDrawArc gc r a1 a2 = draw MyWindow gc (DrawArc r a1 a2)
wFillArc gc r a1 a2 = draw MyWindow gc (FillArc r a1 a2)
wDrawCircle gc p r = draw MyWindow gc (drawCircle p r)
wFillCircle gc p r = draw MyWindow gc (fillCircle p r)
wCopyArea gc src r p = draw MyWindow gc (CopyArea src r p)
wCopyPlane gc src r p i = draw MyWindow gc (CopyPlane src r p i)
wDrawPoint gc p = draw MyWindow gc (DrawPoint p)
wCreatePutImage gc r s d = draw MyWindow gc (CreatePutImage r s d)
