{-# LANGUAGE CPP #-}
module FileShellF(
#ifndef __NHC__
  fileShellF,fileShellF',
   textFileShellF,textFileShellF',
   showReadFileShellF,showReadFileShellF'
#endif
 ) where
import Fudgets
import FilePickPopupF
import TitleShellF(titleShellF')
import MenuBarF
#ifndef __NHC__
          (menuBarF,menu,idT,item,cmdItem,key)
#endif
import DialogueIO
import Prelude hiding (IOError)
import Data.Maybe(isJust,fromJust,fromMaybe)
import Data.Char(isSpace)

#ifndef __NHC__

textFileShellF = textFileShellF' standard
textFileShellF' customiser = fileShellF' customiser textConv
  where textConv = (id,Right,Just "")

showReadFileShellF empty = showReadFileShellF' standard empty
showReadFileShellF' customiser empty =
    fileShellF' customiser (show,parse,empty)
  where
    parse contents =
      case reads contents of
        [(x,cs)] | all isSpace cs -> Right x
	_ -> Left "Syntax error in input"



fileShellF = fileShellF' standard

fileShellF' customiser conv title0 appF =
    stubF $ loopOnlyF $ titleShellF' customiser title0 mainF
  where
    mainF = ctrlF title0 conv >==<
            (popupsF>+<vBoxF (fileShellMenuBarF hasNew>+<appF))
      where hasNew = case conv of (_,_,e) -> isJust e

    popupsF = filePickPopupF' >+<(messagePopupF>+<nullF{-confirmPopupF-})
      where
        filePickPopupF' = putsF (map f (take 1 args)) filePickPopupF
	f x = ((Open,popup),x)


messageF = putF . toMessage
toFilePick = Right . Left . Left
toMessage = Right . Left . Right . Left
--toConfirm = Right . Left . Right . Left
toApp = Right . Right . Right
toTitle = Left

ctrlF title0 (show,parse,optEmpty) = start
 where
  start = loop Nothing Nothing
  changeTitle name = putF (toTitle (title0++": "++name))
  loop filename document =
    getF $ {-either quitMsg-} (either fromPopups (either fromMenu fromApp))
   where
    same = loop filename document
    newName name document' = changeTitle name $ loop (Just name) document'
    errMsg err = messageF err same

    quitMsg () = terminateProgram

    fromPopups = either fromFilePick (either fromMessage fromConfirm)
    fromMessage _ = same
    fromConfirm _ = same -- !!

    fromMenu filecmd =
      case filecmd of
	Open -> putF (toFilePick (Open,("Open",Nothing))) same
        Save -> flip (maybe same) document $ \ doc ->
	        flip (maybe (fromMenu SaveAs)) filename $ \ name ->
		saveF show name doc errMsg $
	        same
	SaveAs -> flip (maybe same) document $ \ _ ->
	          putF (toFilePick (SaveAs,("Save",Just (fromMaybe "" filename)))) same
	Quit -> terminateProgram
	New  -> flip (maybe $ errMsg "New not implemented") optEmpty $ \empty ->
	        changeTitle "Empty file" $
	        putF (toApp empty) $
		start
	_ -> same

    terminateProgram = hIOSuccF (Exit 0) same

    fromFilePick ((action,_),filename) =
      case action of
        Open -> openF parse filename errMsg
		      (\ contents ->
	               putF (toApp contents) $
	               newName filename (Just contents))
        SaveAs -> saveF show filename (fromJust document) errMsg $
                  newName filename document
	_ -> undefined -- shouldn't happen

    fromApp inpmsg =
      case inputDone inpmsg of
	Just doc ->
	  case filename of
	    Just name -> saveF show name doc errMsg $
	                 loop filename (Just doc)
            Nothing   -> putF (toFilePick (SaveAs,("Save",filename))) $
	                 loop filename (Just doc)
	Nothing -> loop filename (Just (stripInputMsg inpmsg))


data FileMenuItem = New | Open | Save | SaveAs | Quit deriving (Eq)


fileShellMenuBarF hasNew =
    fromLeft >^=< hBoxF (fileMenuF hasNew >+< gcWarnF) >=^< Left
  where gcWarnF = spacer1F (hvAlignS aRight aCenter) gcWarningF

fileMenuF hasNew =
    spacer1F (noStretchS True True `compS` leftS) (menuBarF menuBar)
  where
    menuBar = [item fileMenu "File"] -- more?
    fileMenu = menu idT $
               (if hasNew then (cmdItem New    "New":) else id)
	       [cmdItem Open   "Open..."     `key` "o",
		cmdItem Save   "Save"        `key` "s",
		cmdItem SaveAs "Save As..."  `key` "a",
		cmdItem Quit   "Quit"        `key` "q" ]

saveF showdoc filename doc errcont cont =
  hIOerrF (WriteFile filename (showdoc doc))
	  (errcont.show)
	  (const cont)

openF parse filename errcont cont =
  hIOerrF (ReadFile filename) (errcont.show) $ \ (Str contents) ->
  either errcont cont (parse contents)

--messageF msg = contDynF $ (startupF [msg] messagePopupF>=^^<nullSP)
-- contDynF doesn't seem to work properly

#endif
