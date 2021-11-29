module FilePickPopupF(filePickF,filePickF',filePickPopupF,filePickPopupF',filePickPopupOptF,filePickPopupOptF',startDir,aFilePath,popup) where
import AllFudgets
import EndButtonsF
--import Files
import Data.List(sort,partition)
import HbcUtils(uncurry3)
import DialogueIO hiding (IOError)
import CompletionStringF
import UnsafePerformIO(unsafePerformIO)
import System.Directory(getCurrentDirectory)

popup = ("OK",Nothing)

filePickPopupF = filePickPopupF' noextrabuttons
filePickPopupOptF = filePickPopupOptF' noextrabuttons
filePickF = filePickF' noextrabuttons

noextrabuttons = [] :: ([(AFilePath->AFilePath,KeySym,String)])

filePickPopupF' btns = mapFilterSP ok >^^=< filePickPopupOptF' btns
  where
    ok (x,Nothing) = Nothing
    ok (x,Just y) = Just (x,y)


filePickPopupOptF' btns =
  delayF $ popupShellF "File Selection" Nothing (filePickF' btns >=^< snd)


filePickF' btns =
    loopThroughRightF (ctrlF (aFilePath startDir,"")) (partsF btns)
  where
    ctrlF st@(dir,file) = getF $ either internal external
      where
        same = ctrlF st

        internal = either (either dirDisp goto) (either fileInput done)

	external (lbl,optpath) = putF (toButtons lbl) (external2 optpath)
          where
	    external2 Nothing = putF (toDirDispDir dir) same
	    external2 (Just s) = newpath (aFilePath s)

        dirDisp = either pick dirList
        dirList list = putF (toFileCompletion (completeFromList (map fst list))) same

        pick inp =
	    pathPartsF (compactPath path) $ \ st'@(dir',file') ->
	    if file'==""
	    then change (dir',file) -- keep previous file name
	    else case inputDone inp of
	           Nothing -> change st'
		   Just _ -> if file'==file -- Hmm. Check dir too.
		             then output path
			     else same -- false double click
	  where
	    path = stripInputMsg inp

	goto f = change (f dir,file)

        fileInput = either completionList filename

        completionList = flip putF same . toDirDispCompletions

	filename inp =
	    case inputDone inp of
	      Nothing -> ctrlF (dir,stripInputMsg inp)
	      Just "" -> same
	      --Just name@('/':_) -> newname (aFilePath name)
	      --Just name -> newname (extendPath dir name)
	      Just path' -> newname (joinPaths dir (aFilePath path'))
	  where
	     newname path =
	       pathPartsF (compactPath path) $ \ st'@(dir',file') ->
	       if file'==""
	       then change st'
	       else output (extendPath dir' file')

	done = either ok cancel
	  where
	    ok _ = putF (out (Just (filePath (extendPath dir file)))) same
	    cancel _ = putF (out Nothing) same

        newpath path = pathPartsF (compactPath path) change

	change st@(dir',file') =
	   putsF ([toFileInput file']++
	          (if dir'/=dir then [toDirDispDir dir'] else [])) $
	   ctrlF st

        output path =
	    putF (out (Just (filePath path'))) (newpath path')
	  where
	    path' = compactPath path

        out = Right
	toDirDisp = Left. Left. Left
	toDirDispDir = toDirDisp. Right
	toDirDispCompletions = toDirDisp. Left
	toFileInput= Left. Right. Left. Right
	toFileCompletion=Left. Right. Left. Left
	toButtons=Left. Right. Right

partsF btns =
  vBoxF ((dirDispF>+<gotoButtonsF btns)>+<(fileInputF>+<endButtonsF'))

dirDispF =
    stripEither >^=< vBoxF (pathDispF >+< dirListF) >=^^< concatMapSP pre
  where
    pre (Left completions) = [Right (Left completions)]
    pre (Right path) = [Left newdir,Right (Right newdir)]
      where newdir = filePath path

    pathDispF = displayF' pm
      where pm = setAlign aCenter.
                 setBgColor bgColor.
		 setBorderWidth 0.
		 setFont labelFont

    dirListF = loopThroughRightF (mapstateF ctrl []) (pickListF fst >+< lsF)
      where
        ctrl list = either (either fromPickListF fromLsF) fromInput
	  where
	    fromInput = either completions newDir
	    completions cs = put (toPickListF hilite)
	      where
	        hilite = highlightItems
	                       [i|(i,(n,_))<-zip [0..] list,n `elem` ns]
		ns = [pre++compl|(pre,compl)<-cs]
	    newDir = put . toLsF
	    fromPickListF msg =
	      put2 (outPick . mapInp (snd.snd) $ msg)
	           (toPickListF.highlightItems.(:[]).fst.stripInputMsg $ msg)
	    fromLsF list' = (list',[outDir list',toPickListF (replaceAll list')])

	    put x = (list,[x])
	    put2 x y = (list,[x,y])

	    out = Right
	    outDir = out . Right
	    outPick = out . Left
	    loop = Left
	    toPickListF = loop . Left
	    toLsF = loop . Right

    lsF = showFilesF>==<paths>^=<readDirF
      where
	paths (dir,resp) =
	    case resp of
	      Right files -> (map (extendPath sdir) . sortFiles) files
	      Left err   -> [extendPath sdir (show err)]
	  where sdir = aFilePath dir
	        sortFiles files =
		  case partition isDotFile files of
		    (dfs,fs) -> sort fs ++
		                sort [ f | f<-dfs, f `notElem` [".",".."]]
		isDotFile ('.':_) = True
		isDotFile _ = False

	showFilesF = contMapF showFiles
	showFiles = conts showFile
        showFile = if argFlag "slowshowfile" True
	           then slowShowFile
		   else fastShowFile

	slowShowFile path c =
	  isDirF (filePath path)
		 (c (file++"/",path))
		 (c (file,path))
	  where file = pathTail path

        fastShowFile path c = c (pathTail path,path)

gotoButtonsF btns =
   haskellIOF (GetEnv "HOME") $ \ homeresp ->
   noStretchF False True $
   matrixF 4 (untaggedListF ([rootF,parentF]++homeF homeresp++extra btns))
  where
    rootF = fButtonF (const rootPath) "r" "/ Root"
    parentF = fButtonF (compactPath.flip extendPath "..") "p" ".. Parent"

    homeF resp=
      case resp of
        Str d@(_:_) -> [fButtonF (const (aFilePath d)) "h" "Home"]
	_ -> []

    fButtonF f k lbl = const f >^=< buttonF' (setKeys [([Mod1],k)]) lbl

    extra = map (uncurry3 fButtonF)

fileInputF = "File" `labLeftOfF` completionStringF

pathPartsF path cont =
    isDirF (filePath path) yes no
  where
    yes = cont (path,"")
    no = cont (pathHead path,pathTail path)
    {-
    no  = case path of
            [] -> cont ([],[]) -- not likely to happen, since the root is a dir
	    file:dir -> cont (dir,file)
    -}


--- Candidates for inclusion in the Fudget library:

isDirF file yes no =
  haskellIOF (StatusFile file) $ \ resp ->
  case resp of
    Str ('d':_) -> yes
    _ -> no

contMapF f =
  getF $ \ x ->
  f x $ \ y ->
  putF y $
  contMapF f

----

-- #ifdef __NHC__
--startDir = argKey "startdir" "/"  -- hmm. should retreive current directory.
-- #else
startDir = unsafePerformIO getCurrentDirectory
-- #endif

{-
 - reloadknapp?
 - alltid absolut path
 - meddelande för att
     välja kataloger
     spara

 - completion och uppnerpilar
 -

 - pixlar kvar i picklistan
-}
