module ShapedButtonsF (radioF1, radioGroupF1, toggleF1, toggleButtonF1, RBBT (..)) where
import AllFudgets
import HbcUtils(lookupWithDefault)

data RBBT = Circle | Square | Triangle
type RadioButtonBorderType = RBBT

radioF1 bbt fname alts startalt =
    radioGroupF1 bbt fname (map fst alts) startalt 
      (lookupWithDefault alts (error "radioF"))

radioGroupF1 :: Eq a => RadioButtonBorderType -> FontName -> [a] -> a -> 
                        (a -> String) -> F a a
radioGroupF1 bbt fname alts startalt show_alt =
    let radioAlts = radioButtonsF1 bbt fname alts show_alt
        buttons = radioAlts >=^< stripEither
    in  loopLeftF (excludeF1 startalt >==< buttons) >=^<
        (\x -> pair x True)

radioButtonsF1 bbt fname alts show_alt =
    let radiobutton alt =
            (
             alt,
             noStretchF False True 
               (toggleButtonF1 bbt fname [] (show_alt alt))
            )
    in  listLF (verticalP' 0) (map radiobutton alts)

excludeF1 start =
    let excl last' =
            let same = excl last'
                cont last'' = excl last''
            in  getSP (\msg ->
                       case msg of
                         (new, False) -> if new == last' then
                                             putsSP [Left (new, True)] (cont new)
                                         else
                                             same
                         (new, True) -> if new == last' then
                                            putsSP [Right new] (cont new)
                                        else
                                            putsSP [Left (last', False), Right new]
                                                  (cont new))
    in  absF (putsSP [Left (start, True)] (excl start))

toggleF1 bbt keys f =
  case bbt of
    Square ->
      let edgew = 3
          dsize = Point 10 10
          innersep = 3
          fudgetsep = 5
          toggleK =
              let cid False = 0
                  cid True = 1
              in  allocNamedColorPixel defaultColormap
                                  onColor1
                                  (\onC ->
                                   allocNamedColorPixel defaultColormap
                                                   offColor1
                                                   (\offC ->
                                                    let toggle s =
                                                            map (Low . XCmd)
                                                                [ChangeWindowAttributes [CWBackPixel (if s then onC else offC)],
                                                                 ClearWindow]
                                                        k (High s) = toggle s
                                                        k _ = []
                                                    in  putsK (Low (layoutRequestCmd (plainLayout dsize True True)) :
                                                              toggle False)
                                                             (K $ concmapSP k)))
          toggleb =
                buttonBorderF1 bbt edgew
                              (marginF innersep
                                    (windowF [{-ConfigureWindow [CWBorderWidth 0]-}] toggleK))
          togglebd =
              let post (Left a) = Left a
                  post (Right b) = stripEither b
              in  stripEither >^=<
                  (marginHVAlignF 0 aCenter aCenter toggleb >+#<
                   (fudgetsep, LeftOf, f))
      in  toggleGroupF keys (marginHVAlignF 0 aLeft aCenter togglebd)
    Triangle ->
      let edgew = 3
          dsize = Point 12 12
          innersep = 6
          fudgetsep = 5
          toggleK =
              let cid False = 0
                  cid True = 1
              in  allocNamedColorPixel defaultColormap
                                  onColor1
                                  (\onC ->
                                   allocNamedColorPixel defaultColormap
                                                   offColor1
                                                   (\offC ->
                                                    let toggle s =
                                                            map (Low . XCmd)
                                                                [ChangeWindowAttributes [CWBackPixel (if s then onC else offC)],
                                                                 ClearWindow]
                                                        k (High s) = toggle s
                                                        k _ = []
                                                    in  putsK (Low (layoutRequestCmd (plainLayout dsize True True)) :
                                                              toggle False)
                                                             (K $ concmapSP k)))
          vormT punt = [FillPolygon Nonconvex CoordModeOrigin [origin, padd origin (Point 0 ((ycoord punt)-1)),
                        padd origin (Point ((xcoord punt)-1) (((ycoord punt)`div`2)-1))]]
          toggleb =
                buttonBorderF1 bbt edgew
                              (marginF innersep
                                    (windowF [{-ConfigureWindow [CWBorderWidth 0]-}]
                                              (shapeK vormT toggleK)))
          togglebd =
              let post (Left a) = Left a
                  post (Right b) = stripEither b
              in  stripEither >^=<
                  (marginHVAlignF 0 aCenter aCenter toggleb >+#<
                   (fudgetsep, LeftOf, f))
      in  toggleGroupF keys (marginHVAlignF 0 aLeft aCenter togglebd)
    Circle ->
      let edgew = 3
          dsize = Point 16 16
          innersep = 2
          fudgetsep = 5
          toggleK =
              let cid False = 0
                  cid True = 1
              in  allocNamedColorPixel defaultColormap
                                  onColor1
                                  (\onC ->
                                   allocNamedColorPixel defaultColormap
                                                   offColor1
                                                   (\offC ->
                                                    let toggle s =
                                                            map (Low . XCmd)
                                                                [ChangeWindowAttributes [CWBackPixel (if s then onC else offC)],
                                                                 ClearWindow]
                                                        k (High s) = toggle s
                                                        k _ = []
                                                    in  putsK (Low (layoutRequestCmd (plainLayout dsize True True)) :
                                                              toggle False)
                                                             (K $ concmapSP k)))
          vormC punt = [FillArc (Rect origin (Point ((xcoord punt)-1) ((ycoord punt)-1))) (0*64) (360*64)]
          toggleb =
                buttonBorderF1 bbt edgew
                              (marginF innersep
                                    (windowF [{-ConfigureWindow [CWBorderWidth 0]-}]
                                             (shapeK vormC toggleK)))
          togglebd =
              let post (Left a) = Left a
                  post (Right b) = stripEither b
              in  stripEither >^=<
                  (marginHVAlignF 0 aCenter aCenter toggleb >+#<
                   (fudgetsep, LeftOf, f))
      in  toggleGroupF keys (marginHVAlignF 0 aLeft aCenter togglebd)

toggleButtonF1 :: RadioButtonBorderType -> String -> [(ModState, KeySym)] -> String -> F Bool Bool
toggleButtonF1 bbt fname keys text =
  stripEither >^=<
  toggleF1 bbt keys (noStretchF True True (labelF' (setFont fname) text))
  >=^< Left

offColor1 = argKey "toggleoff" bgColor

onColor1 = argKey "toggleon" fgColor

buttonBorderF1 :: RadioButtonBorderType -> Int -> (F a b) -> F (Either Bool a) b
buttonBorderF1 = stdButtonBorderF1

stdButtonBorderF1 bbt edgew f =
    let kernel =
          allocNamedColorDefPixel defaultColormap shineColor "white" $ \shine ->
          allocNamedColorDefPixel defaultColormap shadowColor "black" $ \shadow ->
          wCreateGC rootGC [GCFunction GXcopy, GCForeground shadow,
                            GCBackground shine] $ \drawGC ->
          wCreateGC rootGC [GCFunction GXcopy, GCForeground shine, GCBackground shine] $ \extraGC ->
          wCreateGC rootGC (invertColorGCattrs shine shadow) $ \invertGC ->
          let
              dRAWS s =
                    let bpx = edgew
                        bpy = edgew
                        upperLeftCorner = Point bpx bpy
                        size@(Point sx sy) = psub s (Point 1 1)
                        rect = Rect origin size
                        upperRightCorner = Point (sx - bpx) bpy
                        lowerLeftCorner = Point bpx (sy - bpy)
                        lowerRightCorner = psub size upperLeftCorner
                        leftBorder = Line upperLeftCorner lowerLeftCorner
                        upperBorder = Line upperLeftCorner upperRightCorner
                        upperLeftLine = Line origin upperLeftCorner
                        lowerRightLine = Line lowerRightCorner size
                        incx = padd (Point 1 0)
                        incy = padd (Point 0 1)
                        decx = padd (Point (-1) 0)
                        decy = padd (Point 0 (-1))
                        lowerBorderPoints = [lowerLeftCorner, lowerRightCorner,
                                             upperRightCorner, Point sx 0, size, Point 0 sy]
                        borderPoints =
                          [pP 1 1, pP 1 sy, size, pP sx 1, origin, upperLeftCorner,
                           incy lowerLeftCorner, (incx . incy) lowerRightCorner,
                           incx upperRightCorner, upperLeftCorner]
                        rectPoints = [origin, padd origin (Point (sx-1) 0), size, padd origin (Point 0 (sy-1))]
                    in  (map Low [
                                  wFillPolygon extraGC Convex CoordModeOrigin rectPoints,
                                  wFillPolygon drawGC Nonconvex CoordModeOrigin lowerBorderPoints,
                                  wDrawLine drawGC leftBorder,
                                  wDrawLine drawGC upperBorder,
                                  wDrawLine drawGC upperLeftLine,
                                  wDrawLine invertGC lowerRightLine,
                                  wDrawRectangle drawGC rect
                                 ],
                                 [Low (wFillPolygon invertGC Nonconvex CoordModeOrigin borderPoints)])
              dRAWT s =
                    let bpx = edgew
                        bpy = edgew+2
                        upperLeftCorner = Point bpx bpy
                        size@(Point sx sy) = psub s (Point 1 1)
                        ap = padd origin (Point 5 2)
                        bp = padd origin (Point 5 (sy-3))
                        cp = Point (sx) (((sy - bpy)`div`2)+2)
                        dp = padd ap (Point bpx bpy)
                        ep = padd bp (Point bpx (-bpy))
                        fp = psub cp (Point (bpx+4) 0)
                        l1 = Line ap bp
                        l2 = Line bp cp
                        l3 = Line cp ap
                        l4 = Line dp ep
                        l5 = Line ep fp
                        l6 = Line fp dp
                        l7 = Line ap dp
                        l8 = Line bp ep
                        l9 = Line cp fp
                        incx = padd (Point 1 0)
                        incy = padd (Point 0 1)
                        decx = padd (Point (-1) 0)
                        decy = padd (Point 0 (-1))
                        tBorderPoints = [(incx . incy) ap, decy bp, decx cp, (incx . incy) ap, dp, fp, ep, dp]
                        tLowerBorderPoints = [ep,bp,cp,fp]
                        trianglePoints = [ap,bp,cp]
                    in  (map Low [
                                  wFillPolygon extraGC Nonconvex CoordModeOrigin trianglePoints,
                                  wFillPolygon drawGC Nonconvex CoordModeOrigin tLowerBorderPoints,
                                  wDrawLine drawGC l1,
                                  wDrawLine drawGC l2,
                                  wDrawLine drawGC l3,
                                  wDrawLine drawGC l4,
                                  wDrawLine drawGC l5,
                                  wDrawLine drawGC l6,
                                  wDrawLine drawGC l7,
                                  wDrawLine drawGC l8,
                                  wDrawLine drawGC l9
                                 ],
                                 [
                                  Low (wFillPolygon invertGC Nonconvex
                                  CoordModeOrigin tBorderPoints)
                                 ])
              dRAWC s =
                    let bpx = edgew
                        bpy = edgew
                        upperLeftCorner = Point bpx bpy
                        size@(Point sx sy) = psub s (Point 1 1)
                        groteRechthoek = Rect origin size
                        groteRechthoek2 = Rect (psub origin (Point 1 1)) size
                        kleineRechthoek = Rect (padd origin (Point edgew edgew)) (Point (sx-(2*edgew)) (sy-(2*edgew)))
                    in  (map Low [
                                  wFillArc extraGC groteRechthoek (0*64) (360*64),
                                  wFillArc drawGC groteRechthoek (-135*64) (180*64),
                                  wDrawArc drawGC groteRechthoek (0*64) (360*64),
                                  wFillArc extraGC kleineRechthoek (0*64) (360*64),
                                  wDrawArc drawGC kleineRechthoek (0*64) (360*64)
                                 ],
                                 [Low (wFillArc invertGC groteRechthoek2 (0*64) (360*64))])
              proc pressed size =
                  getK $ \bmsg ->
                  let same = proc pressed size
                      (drawit_size, pressit_size) = case bbt of
                                                      Square -> dRAWS size
                                                      Triangle -> dRAWT size
                                                      Circle -> dRAWC size
                      redraw b = if (b == pressed) then [] else pressit_size
                  in  case bmsg of
                        Low (XEvt (Expose _ 0)) -> putsK (drawit_size ++
                            (if pressed then pressit_size else [])) same
                        Low (LEvt (LayoutSize newsize)) -> proc pressed newsize
                        High change -> putsK (redraw change) (proc change size)
                        _ -> same
              proc0 pressed =
                  getK $ \msg ->
                  case msg of
                    Low (LEvt (LayoutSize size)) -> proc pressed size
                    High change -> proc0 change
                    _ -> proc0 pressed
          in  proc0 False

        startcmds =
          [XCmd $ ConfigureWindow [CWBorderWidth 0],
           XCmd $ ChangeWindowAttributes [CWEventMask [ExposureMask]]]
    in  stripEither >^=< (((groupF startcmds (changeBg bgColor kernel)) . marginF (edgew + 1)) f)

