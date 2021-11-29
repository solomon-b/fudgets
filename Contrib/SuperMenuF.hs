module SuperMenuF (superMenuF, MenuItem (..)) where
--module SuperMenuF where
import AllFudgets
import Data.Maybe(fromJust) --,fromMaybe
import HbcUtils(breakAt)

data MenuItem a =
       Item a
       | Submenu (String, [MenuItem a])
       deriving (Eq, Ord, Show)

data MenuTag a =
       ItemTag a
       | SubTag String
       deriving (Eq, Ord)

data PopupSubMenu =
       PopupSub Point
       | PopdownSub
          
mainTag = SubTag "Joost Bossuyt"

modstate = []

mousebutton = Button 1

menuButtonF1 gcs optrect text =
  let mask =
        [EnterWindowMask, LeaveWindowMask, ButtonPressMask, ButtonReleaseMask,
         ExposureMask]
      startcmds =
        [XCmd $ ChangeWindowAttributes [CWEventMask mask, CWBackingStore Always]]
      optsize = fmap rectsize optrect
  in swindowF startcmds
             optrect
             (buttonDisplayK gcs optsize text)

lxcmd = Low . XCmd

buttonDisplayK (drawGC,invertGC,fs) opsize text =
    let Rect spos ssize = string_rect fs text
        margin = Point 3 1
        size =case opsize of
                Just s -> s
                Nothing -> padd ssize (padd margin margin)
        invertitif b size' =
          if b then [Low (wFillRectangle invertGC (Rect origin size'))]
               else []
        drawit state size' =
          let textpos = psub margin spos
          in [lxcmd ClearWindow, Low (wDrawImageString drawGC textpos text)]
                ++ invertitif (state == BMInverted) size'
        buttonproc bstate size' =
          let same = buttonproc bstate size'
              cont b = buttonproc b size'
              redraw b s = putsK (drawit b s) (buttonproc b s)
          in getK $ \bmsg ->
               case bmsg of
                 Low (XEvt (Expose _ 0)) -> redraw bstate size'
                 Low (LEvt (LayoutSize size'')) -> redraw bstate size''
                 Low (XEvt (ButtonEvent _ _ _ _ Released _)) ->
                   putsK (invertitif (bstate == BMInverted) size'
                           ++ [High (BMClick, Nothing)])
                        (cont BMNormal)
                 Low (XEvt (EnterNotify {pos=winpos,rootPos=rootpos})) ->
                   let width = Point (xcoord size') (-1) 
                       pos = padd (psub rootpos winpos) width
                   in putsK (invertitif (bstate /= BMInverted) size'
                              ++ [High (BMInverted, Just pos)])
                        (cont BMInverted)
                 Low (XEvt (LeaveNotify {})) ->
                   putsK (invertitif (bstate /= BMNormal) size') (cont BMNormal)
                 _ -> same
    in putsK [Low (layoutRequestCmd (plainLayout size True True))]
            (buttonproc BMNormal size)

menuListF gcs alts show_alt =
  let --show_MenuTag :: MenuTag a -> String
      show_MenuTag x =
        case x of
          ItemTag a -> show_alt a
          SubTag s -> s
      altButton alt = (alt, menuButtonF1 gcs Nothing (show_MenuTag alt))
  in listLF (verticalP' 0) (map altButton alts)

subMenuF gcs optrect alts show_alt =
    let wattrs = [CWEventMask [], CWSaveUnder True, CWOverrideRedirect True]
        startcmds = [lxcmd $ ChangeWindowAttributes wattrs,
	             lxcmd $ ConfigureWindow [CWBorderWidth 1]]
        fudget = menuListF gcs alts show_alt 
    in delayF $
       loopCompThroughRightF $
       shellKF' (setMargin 0.setVisible False) (putsK startcmds subMenuK) fudget

subMenuK =
  let popdown = map lxcmd [UnmapWindow]
      popup p = map lxcmd [moveWindow p, MapRaised]
      downK =
        getK (\msg ->
          case msg of
            High (Right (PopupSub p )) -> putsK (popup p) upK
            _ -> downK)
      upK =
        getK (\msg ->
          case msg of
            High (Right PopdownSub) -> putsK popdown downK
            High (Left (alt, (bm, pos))) ->
              putsK [High (Right (alt, (bm, pos)))] upK
            _ -> upK)
  in setFontCursor 110 downK

controlF list = loopCompThroughRightF (kernelF controlK >+< listF list)

controlK ::
  (Eq a) =>K (Either (MenuTag a,(MenuTag a,(BMevents,Maybe Point))) PopupSubMenu)
             (Either (MenuTag a,PopupSubMenu) a)
controlK =
  let proc active =
        getK (\msg ->
          case msg of
            High (Left (tag, (SubTag s, (bm, opoint)))) ->
              (case bm of
                 BMClick ->
                   let oldlist = map (\x -> High (Left (x, PopdownSub))) active
                   in putsK oldlist (proc [])
                 BMInverted ->
                   let (olist, nlist) = breakAt tag active
                       newlist = [SubTag s, tag] ++ nlist
                       oldlist = map (\x -> High (Left (x, PopdownSub))) olist
                       pos = fromJust opoint
                   in putsK (oldlist ++ [High(Left(SubTag s, PopupSub pos))])
                           (proc newlist)
                 _ -> proc active)
            High (Left (tag, (ItemTag a, (bm, opoint)))) ->
              (case bm of
                 BMClick ->
                   let oldlist = map (\x -> High (Left (x, PopdownSub))) active
                   in putsK (oldlist ++ [High (Right a)]) (proc [])
                 BMInverted ->
                   let (olist, nlist) = breakAt tag active
                       newlist = [tag] ++ nlist
                       oldlist = map (\x -> High (Left (x, PopdownSub))) olist
                   in putsK oldlist (proc newlist)
                 _ -> proc active)
            High (Right (PopupSub pos)) ->
              putsK [High (Left (mainTag, PopupSub pos))] (proc [mainTag])
            High (Right PopdownSub) ->
              let oldlist = map (\x -> High (Left (x, PopdownSub))) active
              in putsK oldlist (proc [])
            _ -> proc active)
  in proc []

clickF1 gcs optrect name =
  let topopup = High . Left
      routeClick = Left
      optsize = fmap rectsize optrect
      proc (Low (XEvt (ButtonEvent _ winpos rootpos [] Pressed (Button 1)))) =
          topopup (True, PopupSub (psub rootpos winpos))
      proc (Low (XEvt (ButtonEvent _ _ _ _ Released (Button 1)))) =
          topopup (False, PopdownSub)
      proc (Low (XEvt (LeaveNotify {mode=NotifyUngrab}))) =
          topopup (False, PopdownSub)
      proc (Low msg) = Low msg
      proc (High hi) = High (Right hi)
      wattrs =
        [CWEventMask [ExposureMask, ButtonPressMask, ButtonReleaseMask,
         OwnerGrabButtonMask, LeaveWindowMask, EnterWindowMask]]
      startcmds = [XCmd $ ChangeWindowAttributes wattrs,
                   XCmd $ ConfigureWindow [CWBorderWidth 1]]
      K cdisp = clickDisplayK gcs optsize name
  in swindowF startcmds
             optrect
             (K $ preMapSP cdisp  proc)

clickDisplayK (drawGC,invertGC,fs) optsize name0 =
  let Rect spos ssize = string_rect fs name0
      strsize = string_box_size fs
      margin = Point 3 1
      size = fromMaybe (padd ssize (padd margin margin)) optsize
      invertitif b size' =
        if b
          then [Low (wFillRectangle invertGC (Rect origin size'))]
          else []
      drawname name hi size =
        let textpos = scalePoint 0.5 (size `psub` strsize name) `psub` spos
        in [lxcmd ClearWindow, Low (wDrawImageString drawGC textpos name)]
               ++ invertitif hi size
      buttonproc highlighted size' name =
        let fixpos (PopupSub p) =
              PopupSub (p `padd` pP (-1) (ycoord size'))
            fixpos msg = msg
            same = buttonproc highlighted size' name
            cont b = buttonproc b size' name
            contn n = buttonproc highlighted size' n
            redraw b s = putsK (drawname name b s) (buttonproc b s name)
            newname name' = putsK (drawname name' highlighted size')
                                 (contn name')
        in getK $ \bmsg ->
             case bmsg of
               Low (XEvt (Expose _ 0)) -> redraw highlighted size'
               Low (LEvt (LayoutSize size'')) -> redraw highlighted size''
               Low (XEvt (LeaveNotify {})) ->
                 putsK (invertitif highlighted size') (cont False)
               Low (XEvt (EnterNotify {})) ->
                 putsK (invertitif (not highlighted) size') (cont True)
               High (Left (hi, msg)) ->
                  putsK (invertitif (hi /= highlighted) size' ++
                        [High (fixpos msg)])
                       (cont hi)
               High (Right name') -> newname name'
               _ -> same
  in putsK [Low (layoutRequestCmd (plainLayout size True True))]
          (buttonproc False size name0)

superMenuF :: (Eq a) => (Maybe Rect) -> FontName -> String -> [MenuItem a]
                        -> (a -> String) -> F String a
superMenuF oplace fname text alts show_alt =
   safeLoadQueryFont fname $ \fs ->
   allocNamedColorPixel defaultColormap "black" $ \ black ->
   allocNamedColorPixel defaultColormap "white" $ \ white ->
   wCreateGC rootGC [GCFunction GXcopy, GCFont (font_id fs)] $ \drawGC ->
   wCreateGC drawGC (invertColorGCattrs black white) $ \invertGC ->
   let gcs = (drawGC,invertGC,fs)
       parse tag source current done =
        if source == []
          then if current == []
                 then done
                 else done ++ [(tag, subMenuF gcs Nothing current show_alt)]
          else let (x : xs) = source
               in case x of
                    Item y -> parse tag
                                    xs 
                                    (current ++ [ItemTag y])
                                    done
                    Submenu (s, z) -> parse tag
                                             xs
                                            (current ++ [SubTag s])
                                            (parse (SubTag s) z [] done)
   in controlF (parse mainTag alts [] []) >==< clickF1 gcs oplace text
