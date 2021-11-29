{-# LANGUAGE CPP #-}
module MenuBarF(
#ifndef __NHC__
	menuF,menuBarF,MenuBar(..),Menu(..),MenuItem'(..),
	Item,item,item',key,itemValue,
	cmdItem,subMenuItem,toggleItem,sepItem,
	radioGroupItem,dynRadioGroupItem,
	delayedSubMenuItem,
	MenuItem(..),menu,Transl(..),idT,compT,
	menuIcon
#endif
  ) where
import Control.Monad((<=<))
import AllFudgets hiding (menuF)
import HbcUtils(mapFst)
--import MonadUtil((@@))
import DynRadioGroupF
import KeyGfx

#ifndef __NHC__
#include "../hsrc/exists.h"

tr x = ctrace "menubar" x x

--- Top level calls, eta expanded because of the monomorphism restriction
menuBarF menu = menuListF Horizontal menu
menuF menu =  menuListF Vertical menu

type MenuBar a = Menu a
type Menu a = [MenuItem' a]
type Keys = [(ModState,KeySym)]

type MenuItem' a = Item (MenuItem a)

data Item a = Item a Gfx Keys
item i = item' [] i -- eta expanded because of the monomorphism restriction
item' k i g = Item i (G g) k
itemValue  (Item a _ _) = a

key (Item a g _) k = Item a (G (keyGfx g k)) [([Mod1],k)]
			-- this creates some unnecessary nested G (G ..)

instance Graphic (Item a) where
  measureGraphicK (Item _ gfx _) = measureGraphicK gfx

instance Eq a => Eq (Item a) where
  Item x _ _ == Item y _ _ = x==y

cmdItem x = item . MenuCommand $ x -- eta expanded because of the monomorphism restriction
toggleItem tr = item . MenuToggle tr
subMenuItem tr = item . SubMenu False tr
delayedSubMenuItem tr = item . SubMenu True tr
radioGroupItem tr items = item . MenuRadioGroup tr items
dynRadioGroupItem tr items = item . MenuDynRadioGroup tr items
sepItem = item MenuLabel (padD 3 $ g $ hFiller 1)

data MenuItem a
  = MenuCommand a
  | MenuToggle (Transl Bool a) Bool
  | EXISTS(b) (Eq EQV(b)) => MenuRadioGroup (Transl EQV(b) a) [Item EQV(b)] EQV(b)
  | EXISTS(b) (Eq EQV(b)) => MenuDynRadioGroup (Transl ([Item EQV(b)],EQV(b)) a) [Item EQV(b)] EQV(b)
  | EXISTS(b) (Eq EQV(b)) => SubMenu Bool (Transl EQV(b) a) (Menu EQV(b))
  | MenuLabel

-- eta expanded because of the monomorphism restriction:
menu t = SubMenu False t

type MMsg a = Either MenuState a
type MF a b = F (MMsg a) (MMsg b)

data Transl l g = Transl (l->g) (g->Maybe l)

--- 
menuItemF :: Eq a => LayoutDir -> MenuItem' a -> MF a a
menuItemF dir (Item item gfx keys) =
  case item of
    MenuCommand a -> translF (click a) (buttonF' (setAlign aLeft . pm) gfx)
    MenuToggle tr init ->
	translF tr (delayItF>==<startupF [init] (toggleButtonF' pm gfx))
    MenuRadioGroup tr items init ->
	translF tr (delayItF>==<gfx `labAboveF` radioGroupF' pm alts init)
      where alts = [(a,g)|Item a g _<-items]
	    pm = setFont menuFont .
		 setPlacer (verticalP' 0) -- (the default is verticalLeftP' 0)
    MenuDynRadioGroup tr items init ->
	translF tr' (delayItF>==<gfx `labAboveF` dynRadioGroupF' pm alts init)
      where alts = [(a,g)|Item a g _<-items]
	    pm = setFont menuFont .
		 setPlacer (verticalP' 0) -- (the default is verticalLeftP' 0)
	    tr' = compT tr dynRadioT
	    dynRadioT = Transl f g
	      where
	        f (alts,alt) = ([Item i g []|(i,g)<-alts],alt)
		g (items,alt) = Just ([(a,g)|Item a g _<-items],alt)

    SubMenu d tr m  -> translMenuF tr (btnMenuF d dir gfx ({-delayF' d $-} subMenuF m))
    MenuLabel     -> graphicsLabelF gfx
--    MenuDelayed item' -> delayF' $ menuItemF dir (Item item' gfx keys)
  where
    --pm = setKeys keys . setFont menuFont
    -- becuase of the mononorphism restriction:
    pm x = setKeys keys . setFont menuFont $ x

{-
    delayF' delayed =
      if delayed
      then delayF''
      else id

    delayF'' fud =
      if argFlag "teemenu" False
      then delayF fud >==< idRightF (teeF show "menuItemF: ")
      else delayF fud
-}

btnMenuF :: Bool -> LayoutDir -> Gfx -> F (MMsg a) a -> MF a a
btnMenuF delayed dir gfx mF =
    buttonMenuF' delayed dir menuFont agfx [] mF >=^< mapEither id Right
  where
    agfx = hboxcD' 3 [g gfx,g menuIcon]

translF (Transl f g) fud =
  Right . f >^=< fud >=^^< mapFilterSP (either (const Nothing) g)

translMenuF (Transl f g) fud =
  mapEither id f >^=< fud >=^^< idLeftSP (mapFilterSP g)

click a = Transl (const a) (\b->if a==b then Just Click else Nothing)
idT = Transl id (const Nothing)
--idT = Transl id Just -- why not this?
compT (Transl f1 g1) (Transl f2 g2) = Transl (f1 . f2) (g2 <=< g1)

-- There should be only one grabberF outside the top level menu.
menuListF :: Eq a => LayoutDir -> Menu a -> F a a
menuListF dir menu = grabberF (menuKeys menu) $ menuListF' dir menu
  where
    menuKeys :: Menu a -> [(a,Keys)]
    menuKeys = concatMap itemKeys
    itemKeys (Item m _ keys) =
	case m of
	  SubMenu _ (Transl f _) menu -> mapFst f (menuKeys menu)
	  MenuRadioGroup (Transl f _) items init ->
	    [(f a,ks)|Item a _ ks<-items]
	  --MenuCommand cmd -> [(cmd,keys)]
	  --MenuToggle (Transl f _) init -> [(f init,keys)] -- hmm
	  _ -> []

subMenuF :: Eq a => Menu a -> F (MMsg a) a
subMenuF menu = filterRightSP >^^=< menuListF' Vertical menu

menuListF' :: Eq a => LayoutDir -> Menu a -> MF a a
menuListF' dir m =
    loopLeftF $
    concatMapSP post >^^=< placerF (linearP dir 0) (listF nms)
    >=^^< concatMapSP pre
  where
    nms = [(i,menuItemF dir e) | (i,e) <- number 0 m]
    ns = map fst nms
    post (i,Right x) = [Right $ Right x]
    post (i,Left b) = [Right $ Left b,Left (i,b)]
    pre (Right (Right x)) = ctrace "menubar" "got input" [(i,Right x) | i<-ns]
    pre (Right (Left b)) =  [(i,Left b) | i<-ns]
    pre (Left (j,b)) = [(i,Left b) | i<-ns, i/=j]

delayItF = idF
{-
delayItF = loopThroughRightF (absF idleSP) timerF
  where
    idleSP = getSP $ either (const idleSP) delaySP
    delaySP x = putSP (Left (Just (0,delay))) $ waitSP x
    waitSP x = getSP $ either doneSP waitSP
      where doneSP _ = putSP (Left Nothing) $ putSP (Right x) idleSP

    delay = argReadKey "delay" 200
-}

--- temporary hack:
{-
--subMenuF gfx mF = menuPopupF mF >==< throughF (buttonF agfx>=^^<nullSP)
menuPopupF mF =
    post >^=<
    inputPopupF "Menu" (inputMsg>^=<mF>=^^<filterRightSP) Nothing
     >=^< pre
  where
    pre cmd = (Nothing,Just cmd)
    post = snd
-}

menuIcon =
  FixD 12 [
    DrawRectangle (rR 1 0 8 10),
    DrawLine (lL 4 3 6 3),
    DrawLine (lL 4 5 6 5),
    DrawLine (lL 4 7 6 7),
    DrawLine (lL 3 11 10 11),
    DrawLine (lL 10 2 10 11)]
    

#endif
