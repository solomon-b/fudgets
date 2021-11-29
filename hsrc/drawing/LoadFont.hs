module LoadFont(loadQueryFont, queryFont, loadFont, loadQueryFontF, 
                queryFontF, loadFontF,safeLoadQueryFont, safeLoadQueryFontF,
                listFonts,listFontsF,listFontsWithInfo,tryLoadFont) where
import Command(XRequest(..))
import Event
import Font(fsl2fs) --FontStruct,FontStructList,
--import Fudget
--import Geometry(Line, Point, Rect, Size(..))
--import EitherUtils(stripMaybe,mapMaybe)
import Data.Maybe(fromJust)
import HbcUtils(mapSnd)
import Cont(tryM)
import Xrequest
import Xtypes

lf k fontname =
    let cmd = LoadFont fontname
        expected (FontLoaded fid) = Just fid
        expected _ = Nothing
    in k cmd expected

loadFont x = lf xrequest x
loadFontF = lf xrequestF

qf k fid =
    let cmd = QueryFont fid
        expected (FontQueried fs) = Just (fsl2fs (fromJust fs))
        expected _ = Nothing
    in  k cmd expected

queryFont x = qf xrequest x
queryFontF = qf xrequestF

lqf k fontname =
    let cmd = LoadQueryFont fontname
        expected (FontQueried optfs) = Just (fmap fsl2fs optfs)
        expected _ = Nothing
    in  k cmd expected

loadQueryFont x = lqf xrequest x
loadQueryFontF = lqf xrequestF

safeLqf lqf fn k =
  tryM (lqf fn)
       (lqf ("fixed"::FontName) $ \ (Just fs) -> k fs)
       k

safeLoadQueryFont x = safeLqf loadQueryFont x
safeLoadQueryFontF = safeLqf loadQueryFontF

lif k pattern maxnames =
    let cmd = ListFonts pattern maxnames
        expected (GotFontList fns) = Just fns
	expected _ = Nothing
    in k cmd expected

listFonts x = lif xrequest x
listFontsF = lif xrequestF

listFontsWithInfo pattern maxnames =
    let cmd = ListFontsWithInfo pattern maxnames
        expected (GotFontListWithInfo fis) = Just (mapSnd fsl2fs fis)
--      expected (GotFontListWithInfo fis) = Just (map ((,) pattern . fsl2fs) fis)
	expected _ = Nothing
    in xrequest cmd expected

-- Since loadFont succeeds and returns a FontId even if the font doesn't exist:
tryLoadFont fn k =
  listFonts fn 1 $ \ fns ->
  case fns of
    fn:_ -> loadFont fn (k . Just)
    _ -> k Nothing
