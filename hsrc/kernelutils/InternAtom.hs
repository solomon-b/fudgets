module InternAtom(internAtomK, internAtomF, internAtom,
                  atomNameK, atomNameF, atomName) where
import Command
import Event
--import Font(FontStruct)
--import Fudget
--import Geometry(Line, Point, Rect, Size(..))
--import LayoutRequest(LayoutRequest)
import Xrequest
--import Xtypes

internAtomK = ia xrequestK

internAtomF = ia xrequestF

internAtom a e = ia xrequest a e

ia k atomNm ifExists =
    let gotit (GotAtom atom) = Just atom
        gotit _ = Nothing
    in  k (InternAtom atomNm ifExists) gotit

atomNameK = an xrequestK

atomNameF = an xrequestF

atomName a = an xrequest a

-- Here we need a Maybe String, so add an extra Just (the inner Just
-- is stripped by the fudlogue).

an k atom = 
    let gotit (GotAtomName (Just s)) = Just (Just s)
        gotit _ = Nothing
    in  k (GetAtomName atom) gotit
