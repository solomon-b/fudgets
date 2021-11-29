{-# LANGUAGE CPP #-}
module GraphicsF(GraphicsF,setCursorSolid,setGfxEventMask,
                 setAdjustSize,setCursor,setDoubleBuffer,
                 graphicsF,graphicsF',
		 graphicsGroupF,graphicsGroupF',
		 graphicsDispGroupF,graphicsDispGroupF',
		 graphicsLabelF,graphicsLabelF',
		 graphicsDispF,graphicsDispF',
		 GfxEventMask(..),GfxChange(..),GfxCommand(..),GfxEvent(..),
                 GfxFCmd,GfxFEvent,
		 replaceGfx,replaceAllGfx,showGfx,highlightGfx) where
import Fudget
import FudgetIO
import Xcommand
import FRequest
import NullF(putK,putsK,getK,nullF)
import Spops(nullSP)
import CompSP(postMapSP)
import SpEither(filterLeftSP)--mapFilterSP
--import SerCompF(stubF)
import Command
import DrawInPixmap(pmFillRectangle,pmDrawPoint)
--import DrawInWindow(wCopyArea)
import Event
import Xtypes
import Geometry
import Gc
import Pixmap
import Cursor
--import Color
--import Font(font_id,string_box_size)
--import LoadFont(safeLoadQueryFont)
import BgF(changeGetBackPixel)
import Defaults(fgColor,bgColor,paperColor,labelFont)
import CmdLineEnv(argFlag,argKeyList,argReadKey)
import LayoutRequest
import Alignment
import Spacers(noStretchS,compS,minSizeS)
import Message
import CompOps
import CompSP(idRightSP)
import Dlayout(groupF)
import Utils(number,pairwith)
import HbcUtils(mapFst,mapSnd)
--import InputMsg
import Graphic
import CompiledGraphics
import MeasuredGraphics(MeasuredGraphics(SpacedM,MarkM),compileMG,DPath(..))--,emptyMG,emptyMG'
import Graphic2Pixmap
import GCtx(GCtx(..),wCreateGCtx,rootGCtx)
import GCAttrs
import MGOps(parentGctx,replaceMGPart,updateMGPart,groupMGParts,ungroupMGParts)
import IdempotSP
import DrawCompiledGraphics
import Rects(intersectRects,overlaps)
import EitherUtils(stripEither)--,mapEither
import Sizing(newSize,Sizing(..))
--import HO(apSnd)
--import Maybe(fromMaybe)
import Xrequest(xrequestK)
import StdIoUtil(echoStderrK)
--import ContinuationIO(stderr)
--import Maptrace(ctrace) -- debugging

import FDefaults
#include "defaults.h"
#include "exists.h"
--  Commands for grapihcsF: ---------------------------------------------------

data GfxChange gfx
  = GfxReplace (Bool,Maybe gfx)
  | GfxGroup Int Int -- position & length
  | GfxUngroup Int -- position
  
data GfxCommand path gfx
  = ChangeGfx [(path,GfxChange gfx)]
  | ChangeGfxBg ColorSpec
  | ChangeGfxBgPixmap PixmapId Bool -- True = free pixmap
#ifdef USE_EXIST_Q
  | EXISTS(bg) TSTHACK((Graphic EQV(bg)) =>) ChangeGfxBgGfx EQV(bg)
#endif
  | ChangeGfxCursor CursorId
  | ChangeGfxFontCursor Int
  | ShowGfx path (Maybe Alignment,Maybe Alignment) -- makes the selected part visible
  | BellGfx Int -- sound the bell
  | GetGfxPlaces [path] -- ask for rectangles of listed paths

replaceAllGfx = replaceGfx []
replaceGfx path gfx = ChangeGfx [(path,GfxReplace (False,Just gfx))]
showGfx path = ShowGfx path (Nothing,Nothing)
highlightGfx path on = ChangeGfx [(path,GfxReplace (on,Nothing))]

instance Functor GfxChange where
  fmap f (GfxReplace r) = GfxReplace (fmap (fmap f) r)
  fmap f (GfxGroup from count) = GfxGroup from count
  fmap f (GfxUngroup at) = GfxUngroup at

instance Functor (GfxCommand path) where
  fmap f cmd =
    case cmd of
      ChangeGfx changes -> ChangeGfx (mapSnd (fmap f) changes)
      -- _ -> cmd -- Operationally, the rest is the same as this line.
      ChangeGfxBg c -> ChangeGfxBg c
      ChangeGfxBgPixmap pm b -> ChangeGfxBgPixmap pm b
#ifdef USE_EXIST_Q
      ChangeGfxBgGfx gfx -> ChangeGfxBgGfx gfx
#endif
      ChangeGfxCursor cursor -> ChangeGfxCursor cursor
      ChangeGfxFontCursor shape -> ChangeGfxFontCursor shape
      ShowGfx path a -> ShowGfx path a
      BellGfx n -> BellGfx n
      GetGfxPlaces paths -> GetGfxPlaces paths

--  Events from graphicsF: ----------------------------------------------------

data GfxEvent path
  = GfxButtonEvent { gfxState :: ModState,
		     gfxType  :: Pressed,
                     gfxButton:: Button,
		     gfxPaths :: [(path,(Point,Rect))] }
  | GfxMotionEvent { gfxState :: ModState,
		     gfxPaths :: [(path,(Point,Rect))] }
  | GfxKeyEvent    { gfxState::ModState,
                     gfxKeySym::KeySym,
		     gfxKeyLookup::KeyLookup }
  | GfxFocusEvent  { gfxHasFocus :: Bool }
  | GfxPlaces [Rect] -- response to GetGfxPlaces
  | GfxResized Size
  deriving (Eq,Show)


--  graphicsF event masks: ----------------------------------------------------

data GfxEventMask = GfxButtonMask | GfxMotionMask | GfxDragMask | GfxKeyMask

allGfxEvents = [GfxButtonMask, GfxMotionMask, GfxDragMask, GfxKeyMask]
gfxMouseMask = [GfxButtonMask, GfxDragMask] -- backward compat

gfxEventMask = concatMap events
  where
    events GfxButtonMask = buttonmask
    events GfxMotionMask = motionmask
    events GfxDragMask   = dragmask
    events GfxKeyMask    = keventmask

    buttonmask = [ButtonPressMask,ButtonReleaseMask]
    motionmask = [PointerMotionMask]
    dragmask = [Button1MotionMask]
    keventmask =
	 [KeyPressMask,
          EnterWindowMask, LeaveWindowMask -- because of CTT implementation
	 ]

--  Customisers for graphicsF: ------------------------------------------------

newtype GraphicsF gfx = Pars [Pars gfx]

data Pars gfx
  -- Standard customisers:
  = BorderWidth Int
  | BgColorSpec ColorSpec
  | FgColorSpec ColorSpec
  | FontSpec FontSpec
  | Sizing Sizing
  | Stretchable (Bool,Bool)
  | InitSize gfx
  | InitDisp gfx
  -- Special customisers:
  | CursorSolid Bool
  | GfxEventMask [GfxEventMask]
  | AdjustSize Bool
  | Cursor Int -- pointer cursor shape for XCreateFontCursor
  | DoubleBuffer Bool
-- Could also support:
--  | Align Alignment
--  | Spacer Spacer
--  | Margin Int

parameter_instance1(BorderWidth,GraphicsF)
parameter_instance1(BgColorSpec,GraphicsF)
parameter_instance1(FgColorSpec,GraphicsF)
parameter_instance1(Sizing,GraphicsF)
parameter_instance1(FontSpec,GraphicsF)
parameter_instance1(Stretchable,GraphicsF)
parameter_instance(InitSize,GraphicsF)
parameter_instance(InitDisp,GraphicsF)
parameter(CursorSolid)
parameter(GfxEventMask)
parameter(AdjustSize)
parameter(Cursor)
parameter(DoubleBuffer)

-------------------------------------------------------------------------------

type GfxFCmd a = GfxCommand DPath a
type GfxFEvent = GfxEvent DPath

graphicsDispF :: Graphic a => F (GfxFCmd a) (GfxFEvent)
graphicsDispF = graphicsDispF' standard

graphicsLabelF lbl = graphicsLabelF' standard lbl

graphicsLabelF' customiser gfx = nullSP >^^=< d >=^^< nullSP'
  where d = graphicsF' (customiser . params)
	params = setInitDisp gfx .setGfxEventMask [] . setSizing Static .
		 setBgColor bgColor . setBorderWidth 0
	nullSP' = nullSP -- :: (SP anything (GfxCommand MeasuredGraphics))
	-- this is a workaround necessary to resolve the overloading

graphicsDispF' :: Graphic gfx => Customiser (GraphicsF gfx) -> F (GfxFCmd gfx) (GfxFEvent)
graphicsDispF' customiser  = graphicsF' (customiser . dispCustomiser)
graphicsDispGroupF fud = graphicsGroupF' dispCustomiser fud
graphicsDispGroupF' customiser fud =
  graphicsGroupF' (customiser . dispCustomiser) fud

dispCustomiser =
  setCursorSolid True . setGfxEventMask gfxMouseMask . setSizing Growing

graphicsF :: Graphic gfx => F (GfxFCmd gfx) (GfxFEvent)
graphicsF = graphicsF' standard

graphicsF' custom = filterLeftSP >^^=< graphicsGroupF' custom nullF >=^< Left

graphicsGroupF :: Graphic gfx => F i o -> F (Either (GfxFCmd gfx) i) (Either (GfxFEvent) o)
graphicsGroupF = graphicsGroupF' standard

--graphicsGroupF' :: (Graphic init,Graphic gfx => Customiser (GraphicsF init) -> F i o -> F (Either (GfxFCmd gfx) i) (Either (GfxFEvent) o)
graphicsGroupF' :: Graphic gfx => Customiser (GraphicsF gfx) -> F i o -> F (Either (GfxFCmd gfx) i) (Either (GfxFEvent) o)
graphicsGroupF' customiser fud = 
  let solid = getCursorSolid params
      mask = getGfxEventMask params
      sizing = getSizing params
      adjsize = getAdjustSize params
      doublebuffer = getDoubleBuffer params
      optcursor = getCursorMaybe params
      font = getFontSpec params
      bw = getBorderWidth params
      bgcol = getBgColorSpec params
      fgcol = getFgColorSpec params
      optx = getInitDispMaybe params
      optstretch = getStretchableMaybe params
      optinitsize = getInitSizeMaybe params
      params = customiser defaults
      defaults = Pars [BorderWidth 1,
                       BgColorSpec (colorSpec paperColor),
                       FgColorSpec (colorSpec fgColor),
		       Sizing Dynamic,
		       CursorSolid False,
		       GfxEventMask allGfxEvents,
		       AdjustSize True,
		       DoubleBuffer defaultdoublebuffer,
		       FontSpec (fontSpec labelFont)]
      eventmask = ExposureMask:
	          gfxEventMask mask
      --grabmask =  [ButtonReleaseMask, PointerMotionMask]
      -- NOTE: some code below assumes that motion events occur ONLY
      --       while Button1 is pressed!
      startcmds = [ChangeWindowAttributes [CWEventMask eventmask,
                                           CWBitGravity NorthWestGravity],
                    ConfigureWindow [CWBorderWidth bw]--,
		    --GrabButton False (Button 1) [] grabmask,
		    --GrabButton False (Button 2) [] [ButtonReleaseMask]
		  ]
  in --compMsgSP layoutOptSP (idRightSP idempotSP) `serCompSP`
     idRightSP (stripEither `postMapSP` idRightSP idempotSP) >^^=<
     groupF (fmap XCmd startcmds)
        (initK doublebuffer font optcursor fgcol bgcol $
	 graphicsK0 solid sizing adjsize optstretch optinitsize optx)
        fud

dbeSwapBuffers cont =
  xrequestK (DbeSwapBuffers swapaction) Just $ \ (DbeBuffersSwapped _) -> cont

optDoubleBufferK False cont = cont Nothing
optDoubleBufferK True cont =
  xrequestK DbeQueryExtension Just $ \ (DbeExtensionQueried status major minor) ->
  let ok=status/=0
  in if not ok
     then echoStderrK "Sorry, double buffering not available." $
	  cont Nothing
     else xrequestK (DbeAllocateBackBufferName swapaction) Just $ \ (DbeBackBufferNameAllocated backbuf) ->
          --xcommandK ClearWindow $
          cont (Just backbuf)

initK doublebuffer font optcursor fgcol bgcol k =
  changeGetBackPixel bgcol $ \ bg ->
  maybe id setFontCursor optcursor $
  convColorK [fgcol,colorSpec "black"] $ \ fg ->
  wCreateGCtx rootGCtx [GCFont [font,fontSpec "fixed"],GCForeground fg,GCBackground bg] $ \ gctx@(GC gc _) ->
  wCreateGC rootGC [GCForeground bg] $ \ cleargc ->
  createCursorGC gc bg fg $ \ higc ->
  optDoubleBufferK doublebuffer $ \ optbackbuf ->
  k optbackbuf gctx bg cleargc higc

optCompileGraphicK gctx optgfx cont =
  case optgfx of
    Nothing -> cont Nothing
    Just gfx ->
      measureGraphicK gfx gctx $ \ mg ->
      cont (Just (mg,compileMG id mg))

graphicsK0 solid sizing adjsize optstretch optinitsize optx optbackbuf gctx bg cleargc higc =
    graphicsK1 
  where
    graphicsK1 =
      optCompileGraphicK gctx optinitsize $ \ optcgsize ->
      optCompileGraphicK gctx optx $ \ optcgx ->
      graphicsK2 optcgsize optcgx
    graphicsK2 optcgsize optcgx =
        graphicsK init
      where
	optSizeS    = fmap (minSizeS . minsize . snd . snd) optcgsize
        optStretchS = fmap stretchS optstretch
          where stretchS (sh,sv) = noStretchS (not sh) (not sv)
        spacerM =
	  case (optStretchS,optSizeS) of
	    (Just stretchS,Just sizeS) -> SpacedM (stretchS `compS` sizeS)
	    (Just stretchS,_         ) -> SpacedM stretchS
	    (_            ,Just sizeS) -> SpacedM sizeS
	    _                          -> MarkM gctx

        -- All incoming and outgoing paths have to be adjusted because of
	-- the extra spacer. The functions pathIn & pathOut handle this.
	init = pairwith (compileMG id) $ spacerM $ maybe (emptyMG 10) fst optcgx
         -- Stretchiness is applied to all drawings as it should be, but
	 -- optinitsize should be applied only to the first drawing!!!
    
    pathIn path = 0:path
    -- pathOut (0:path) = path

    -- locatePointOut p = mapFst pathOut . locatePoint p
    locatePointOut p (CGMark cg) = locatePoint p cg
    -- bug if top node isn't a CGMark !!

    graphicsK (mg,(cg,req)) =
      putLayoutReq req $
      idleK cleargc req mg cg solid []

    idleK cleargc req mg cg active es =
        seq size $ -- prevents a space leak when sizing==Dynamic, TH 980724
	getK $ message lowK highK
      where
	size = minsize req -- == current window size most of the time
	curR = hiR (solid||active)
	same = idleK cleargc req mg cg active es
	newcleargc cleargc' = idleK cleargc' req mg cg active es

	optInsertNew mg cg gctx path optreq optnew k =
	  case optnew of
	    Nothing  -> k mg cg optreq
	    Just new -> measureGraphicK new gctx $ \ newmg ->
			let mg' = replaceMGPart mg path newmg
			    (cg',req) = compileMG (newSize sizing size) mg'
			in k mg' cg' (Just req)

	updGraphicsK mg cg optreq [] c =
	  case optreq of
	    Just req' | not (similar req' req)
		 	     -> --ctrace "updgfx" (show (req,req')) $
				putLayoutReq req' $ c req' mg cg False
	    _                -> c req mg cg True
	updGraphicsK mg cg optreq ((path,change):changes) c =
          case change of
            GfxReplace r -> replace r
            GfxGroup from count -> group from count
            GfxUngroup pos -> ungroup pos
          where
            replace (hi,optnew) =
              optInsertNew mg cg (parentGctx gctx mg path) path optreq optnew $ \ mg' cg' optreq' ->
              let cg'' = case (hi,optnew) of
                           (False,Nothing) -> cgupdate cg' path removecursor
                           (True,_)        -> cgupdate cg' path addcursor
                           _ -> cg'
              in updGraphicsK mg' cg'' optreq' changes c

            group from count = updGraphicsK mg' cg' optreq changes c
              where mg' = updateMGPart mg path (groupMGParts from count)
                    cg' = cgupdate cg path (cgGroup from count)

            ungroup pos = updGraphicsK mg' cg' optreq changes c
              where mg' = updateMGPart mg path (ungroupMGParts pos)
                    cg' = cgupdate cg path (cgUngroup pos)

        bufDrawChangesK = maybe drawChangesK backBufDrawChangesK optbackbuf
	bufDrawK = maybe drawK backBufDrawK optbackbuf

        backBufDrawChangesK backbuf beQuick cur new old changes cont =
            drawChangesK' (Just (DbeBackBuffer backbuf,cleargc)) False cur new old changes $
	    dbeSwapBuffers $
	    cont
	backBufDrawK backbuf cur clip cg cont =
            drawK' (DbeBackBuffer backbuf) cur clip cg $
	    dbeSwapBuffers $
	    --putLow (wCopyArea gc (DbeBackBuffer backbuf) (Rect 0 size) 0) $
	    cont
          where (GC gc _) = gctx

	buttonEvent p state type' button =
	  -- High level output tagged Left is sent through idempotSP
	  putHigh (Left $
	           GfxButtonEvent state type' button (locatePointOut p cg)) $
	  same
	motionEvent p state =
	  -- High level output tagged Left is sent through idempotSP
	  putHigh (Left $ GfxMotionEvent state (locatePointOut p cg)) $
	  same
	key mods sym lookup =
	  putHigh (Right $ GfxKeyEvent mods sym lookup) $ same

	highK (ShowGfx path align) = mkPathVisible cg (pathIn path) align same
	highK (BellGfx n) = xcommandK (Bell n) same

	highK (GetGfxPlaces paths) =
	  putHigh (Right $ GfxPlaces $ fmap (cgrect . cgpart cg . pathIn) paths) $
	  same
	highK (ChangeGfxBg bgspec) =
	  convColorK bgspec $ \ bgcol ->
	  xcommandK (ChangeWindowAttributes [CWBackPixel bgcol]) $
	  xcommandK clearWindowExpose $
	  wCreateGC rootGC [GCForeground bgcol] $ \ cleargc' ->
	  -- FreeGC cleargc
	  newcleargc cleargc'
	highK (ChangeGfxBgPixmap pixmap freeIt) =
	  xcommandK (ChangeWindowAttributes [CWBackPixmap pixmap]) $
	  xcommandK clearWindowExpose $
	  wCreateGC rootGC [GCFillStyle FillTiled,GCTile pixmap] $ \ cleargc' ->
	  -- FreeGC cleargc
	  (if freeIt then xcommandK (FreePixmap pixmap) else id) $
	  newcleargc cleargc'
#ifdef USE_EXIST_Q
        highK (ChangeGfxBgGfx gfx) =
	  graphic2PixmapImage gfx gctx $ \ (PixmapImage size pm) ->
	  highK (ChangeGfxBgPixmap pm True)
#endif
        highK (ChangeGfxCursor cursor) =
          defineCursor cursor $
          xcommandK Flush $
	  same
        highK (ChangeGfxFontCursor shape) =
          setFontCursor shape $
          xcommandK Flush $
	  same
	highK (ChangeGfx changes0) =
	    updGraphicsK mg cg Nothing changes $ \ req' mg' cg' beQuick ->
	    bufDrawChangesK beQuick (higc,curR) cg' cg (fmap fst changes) $
	    --mkChangeVisible cg' changes $
	    idleK cleargc req' mg' cg' active []
	  where changes = mapFst pathIn changes0

	changeActive active' =
	  if active'==active
	  then same
	  else putHigh (Left $ GfxFocusEvent { gfxHasFocus=active' }) $
	       bufDrawChangesK True (higc,hiR (solid||active')) cg cg (cursorPaths cg) $
	       idleK cleargc req mg cg active' es

        lowK (XEvt e) = eventK e
	lowK (LEvt lresp) = layoutK lresp
	lowK _ = same

        layoutK lresp =
	  case lresp of
	    LayoutSize size'
		| adjsize ->
		    if size' == size then same
		    else let cg'' = foldr restorecursor cg' (cgcursors cg)
			       where
			         restorecursor path cg = cgupdate cg path addcursor
				 (cg',_) = compileMG (const size') mg
			 in putHigh (Left $ GfxResized size') $
			    bufDrawChangesK True (higc,curR) cg'' cg [] $
			    idleK cleargc req' mg cg'' active es
		| otherwise -> idleK cleargc req' mg cg active es
	      where req' = mapLayoutSize (const size') req
	    _ -> same

	eventK event =
	  case event of
	    Expose r 0 ->
	      let rs = r:es
	      in bufDrawK (higc,curR) (intersectRects rs) (prune rs cg) $
                 idleK cleargc req mg cg active []
	    Expose r _ -> idleK cleargc req mg cg active (r:es)
	    FocusIn  {} -> changeActive True
	    FocusOut {} -> changeActive False

	    ButtonEvent {pos=pos, type'=type', button=button, state=state} ->
	       buttonEvent pos state type' button
	    MotionNotify {pos=pos,state=state} -> motionEvent pos state
	    KeyEvent _ _ _ mods Pressed _ sym lookup -> key mods sym lookup
	    _ -> same

prune rs (CGMark cg) = CGMark (prune rs cg)
prune rs (CGraphics r cur cmds cgs) =
  if any (overlaps r) rs
  then if null cmds --  || all (null.snd) cmds
       then CGraphics r cur cmds (fmap (prune rs) cgs)
       else CGraphics r cur cmds cgs
             -- cmds may overlap with cgs, so
	     -- if cmds are redrawn then all cgs should be redrawn too.
  else CGraphics r cur [] [] -- subtree rectangles are inside parent rectangles.

{-
locatePoint' p cg = fmap addrect $ locatePoint p cg
  where
    addrect = pairwith (cgrect . cgpart cg)
-}

locatePoint p (CGMark cg) = [(0:path,geom)|(path,geom)<-locatePoint p cg]
 --  ^^ the wrong geometry will be return if CGMark came from a SpacerM !!
locatePoint p (CGraphics r _ _ gs) =
  if p `inRect` r
  then let ps = fmap (locatePoint p) gs
       in case [ (i:path,pr) | (i,paths)<-number 1 ps, (path,pr)<-paths] of
            [] -> [([],(p-rectpos r,r))]
	    ps -> ps
  else []

cursorPaths (CGMark cg) = fmap (0:) (cursorPaths cg)
cursorPaths (CGraphics _ cur _ gs) =
  if cur
  then [[]]
  else [i:p | (i,g)<-number 1 gs, p<-cursorPaths g]

hiR True = solidCursorRects
hiR False = hollowCursorRects

solidCursorRects r = [r]

hollowCursorRects (Rect (Point x y) (Point w h)) =
   [rR x y w lw,rR x y lw h,rR x (y+h-lw) w lw,rR (x+w-lw) y lw h]
 where lw=minimum [2,w,h]

--putSize cg = putLayoutReq (Layout (cgsize cg) False False)

mkChangeVisible cg changes =
  case [ path | (path,(True,_))<-changes] of
    path:_ -> mkPathVisible cg path (Nothing,Nothing)
    _ -> id

mkPathVisible cg path align =
    putLayout (lMkVis (cgrect (cgpart cg path)))
  where
    lMkVis r = LayoutMakeVisible (r `growrect` 5) align
			 -- growrect compensates for a layout bug !!

putLayoutReq = putLayout . LayoutRequest
--putSpacer = putLayout . LayoutSpacer
putLayout = putK . Low . LCmd

createCursorGC gc bg fg cont =
  --allocNamedColorDefPixel defaultColormap cursorcolor "white" $ \ hipix ->
  tryConvColorK cursorcolor $ \ opthipix ->
  let hipix = fromMaybe fg opthipix
  in if hipix/=bg && hipix/=fg && not mono
     then wCreateGC gc [GCForeground hipix] $ \ cursorgc ->
	  cont cursorgc
     else createPixmap (Point 2 2) copyFromParent $ \ pm ->
	  wCreateGC gc [GCForeground bg] $ \ cleargc ->
	  putsK [Low $ pmFillRectangle pm cleargc (rR 0 0 2 2),
		 Low $ pmDrawPoint pm gc 0] $
	  wCreateGC gc [GCFillStyle FillTiled,GCTile pm] $ \ cursorgc ->
	  cont cursorgc

similar l1 l2 =
  minsize l1==minsize l2 &&
  fixedh l1==fixedh l2 &&
  fixedv l1==fixedv l2

cursorcolor = argKeyList "cursor" ["yellow"]
mono = argFlag "mono" False
defaultdoublebuffer = argFlag "doublebuffer" False
swapaction = argReadKey "swapaction" DbeCopied
