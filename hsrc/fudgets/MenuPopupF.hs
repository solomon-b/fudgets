module MenuPopupF(PopupMenu(..), menuPopupF, menuPopupF') where
import Command
import Cursor
import Shells(unmappedShellF')
import FDefaults() -- synonym Customiser, for hbc
import DShellF(setFocusMgr)
import Event
import Fudget
import FRequest
import Geometry(Point)
import Loops(loopCompThroughRightF,loopThroughRightF)
--import Message(Message(..))
import CompSP(prepostMapSP)
import SerCompF(mapstateF)
import NullF
import Xtypes
import Data.List(union)

data PopupMenu
  = PopupMenu Point XEvent -- Time to pop up the menu.
  | PopupMenuStick	   -- The mouse button has been released, but stay up.
  | PopdownMenu		   -- Time to hide the menu.
  deriving Show
  --deriving (Eq, Ord)

menuPopupF = menuPopupF' False

menuPopupF' delayed menu =
    loopCompThroughRightF (dF $ unmappedShellF' pm startcmds popupShellK menu')
  where
    dF = if delayed then delayF else id
    pm = setFocusMgr False
    menu' = handleButtonMachinesF menu
    startcmds = [XCmd $ ChangeWindowAttributes wattrs,
		 XCmd $ ConfigureWindow [CWBorderWidth 1]]
    wattrs = [CWEventMask [], CWSaveUnder True, CWOverrideRedirect True]

popupShellK =
    setFontCursor 110 downK
  where
    mouse (ButtonEvent {}) = True
    mouse (EnterNotify {}) = True
    mouse _ = False
    samekey (KeyEvent {keySym=ks}) ks' = ks == ks'
    samekey _ _ = False

    toMenu = High . Left . Right
    toBms = High . Left . Left
    out = High . Right

    popdown = map (Low . XCmd) [UnmapWindow]
    popup p = toBms True:map (Low . XCmd) [moveWindow p, MapRaised]

    downK =
      getK $ \msg ->
      case msg of
	High (Right (Left (PopupMenu p ev))) -> putsK (popup p)     $ upK ev
	High (Right (Right x))               -> putK  (toMenu x)    $ downK
	High (Left x)                        -> putK  (out x)       $ downK
	_ -> downK

    upK ev =
      getK $ \msg ->
      case msg of
	High (Right (Left (PopupMenu p ev))) -> putsK (popup p)     $ upK ev
	High (Right (Left PopdownMenu))      -> putsK popdown       $ downK
	High (Right (Left PopupMenuStick))   -> putK  (toBms False) $ upK ev
	High (Right (Right x))               -> putK  (toMenu x)    $ upK ev
	High (Left x) ->
	  if mouse ev
	  then putsK (out x : popdown) $ downK
	  else putK  (out x)           $ upK ev
	Low (XEvt (KeyEvent {type'=Released, keySym=ks})) ->
	  if samekey ev ks
	  then putsK popdown downK
	  else upK ev
	_ -> upK ev

{- handleButtonMachineF fud records the paths of all button machines in fud, so
   that a message can be broadcast to them when the MenuPopup mode changes.
   It also keeps track of the current mode to avoid sending the same mode twice.
-}

handleButtonMachinesF fud =
    loopThroughRightF ctrlF (liftbm fud)
  where
    ctrlF = mapstateF ctrl (False,[])

    ctrl state@(mode,bms) = either fromLoop fromOutside
      where
	fromLoop (Left path) = addbm path
	fromLoop (Right y) = out y

        fromOutside (Left mode') = changeMode mode'
	fromOutside (Right x) = inp x

        addbm path = ((mode,[path] `union` bms),[Left (Left (path,mode))])
	  -- Tell the new button the current mode. (Needed in case it is
	  -- dynamically created inside a menu.)
        out y = (state,[Right y])
	inp x = (state,[Left (Right x)])

	changeMode mode' =
	  if mode'/=mode
	  then ((mode',bms),[Left (Left (path,mode')) | path<-bms])
	      -- !! This will send msgs also to buttons that have been destroyed
	  else (state,[])

    liftbm (F sp) = F $ prepostMapSP pre post sp
      where
	pre (High (Left (path,mode))) = Low (path,XEvt (MenuPopupMode mode))
	pre (High (Right x)) = High x
	pre (Low tevent) = Low tevent

	post (Low (path,XCmd MeButtonMachine)) = High (Left path)
	post (Low tcmd) = Low tcmd
	post (High y) = High (Right y)
