module FilePickF(oldFilePickF,smallPickListF) where
import List2(sort)
import MoreF(pickListF)--,PickListRequest(..)
import ListRequest(replaceAll)
import CompOps
--import Defaults(menuFont)
import FilePaths
import IoF(ioF)
import HaskellIO(hIOerr)
import Fudget
import NullF(startupF,getK,putK)--F,K,
--import FudgetIO
import ReadFileF(readDirF)
--import Geometry(Point(..))
import LayoutDir(Orientation(..))
import LayoutOps
--import Spacer(layoutModifierF)
import Loops(loopLeftF)
import SerCompF(bypassF)
import InputMsg(stripInputMsg)--InputMsg(..),
import InputSP(inputLeaveDoneSP)--,inputDoneSP
import InputF(inputThroughF)--,InF(..)
import DStringF(stringF)
import EitherUtils(stripEither)
--import Message(Message(..))
import DialogueIO hiding (IOError)

dirF = aFilePath >^=< bypassF (inputLeaveDoneSP >^^=< stringF) -- startpath ?!!!

shownameF = inputThroughF stringF

startpath = "."

lsF = paths>^=<readDirF>=^<filePath
  where
    paths (dir,resp) =
        case resp of
          Right files -> (map (extendPath sdir) . sort . filter (/=".")) files
	  --Left err   -> [show err, filePath sdir]
	  Left err   -> [aFilePath "Error", sdir]
      where sdir = aFilePath dir

smallPickListF f = {-layoutModifierF lf-} (snd.stripInputMsg>^=<pickListF f>=^<replaceAll)
--  where lf (Layout _ fh fv) = Layout (Point 240 120) fh fv

oldFilePickF =
    let showdirF =
            startupF [aFilePath startpath]
                     ((compactPath >^=< filePickListF) >==< lsF)
	filePickListF = smallPickListF pathTail
        routeK =
	    getK $ \ msg ->
	    case msg of
	      High p ->
	        let s = filePath p
		    cont r = putK (High (r s)) routeK
		    fileCont = cont Right
		    dirCont = cont Left
		    checkCont (Str ('d':_)) = dirCont
		    checkCont _ = fileCont
		in hIOerr (StatusFile s) (const fileCont) checkCont
        f1 >=#=< f2 = (f1,Below) >#==< f2
    in  shownameF >=#=<
        loopLeftF (((ioF routeK >==< showdirF) >=#=< dirF) >=^< stripEither)
