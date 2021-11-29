module BitmapF (bitmapButtonF, bitmapDispF, bitmapDispBorderF) where
import AllFudgets

windowKernel filename =
  allocNamedColorPixel defaultColormap "black" $ \fg ->
  allocNamedColorPixel defaultColormap "white" $ \bg ->
  changeBackPixel "white" $
  wCreateGC rootGC [GCFunction GXcopy,
                    GCForeground fg,
                    GCBackground bg,
                    GCGraphicsExposures False] $ \drawGC ->
  let displayImage size bitmapid =
        createPixmap size copyFromParent (\pixmapid ->
          putsK [Low (pmCopyPlane pixmapid drawGC (Pixmap bitmapid) 
                    (Rect (Point 0 0) size) (Point 0 0) 0),
                lxcmd (ChangeWindowAttributes [CWBackPixmap pixmapid]),
                lxcmd (FreePixmap bitmapid),
                lxcmd (FreePixmap pixmapid),
                Low (layoutRequestCmd (plainLayout size True True)),
                lxcmd ClearWindow] 
               displayproc)
      lxcmd = Low . XCmd
      displayproc =
        getK (\msg ->
          case msg of
            Low (XEvt (Expose _ 0)) -> xcommandK ClearWindow displayproc 
            High BitmapBad -> error ("Invalid bitmap file")
            High (BitmapReturn size _ bitmapid) -> displayImage size bitmapid
            _ -> displayproc)
  in readBitmapFile filename (\bmr ->
       case bmr of
         BitmapBad -> error ("Invalid bitmap file " ++ filename)
         BitmapReturn size _ bitmapid -> displayImage size bitmapid)

bitmapDispF :: FilePath -> F BitmapReturn a
bitmapDispF filename =
  let wattrs = [CWBackingStore WhenMapped, CWEventMask [ExposureMask]]
      kernelF =  windowF ([XCmd $ ChangeWindowAttributes wattrs])
                         (windowKernel filename)
  in marginHVAlignF 0 aCenter aCenter kernelF

bitmapDispBorderF :: Int -> FilePath -> F BitmapReturn a
bitmapDispBorderF width filename =
  let wattrs = [CWBackingStore WhenMapped, CWEventMask [ExposureMask]]
      kernelF = windowF [XCmd (ChangeWindowAttributes wattrs),
                         XCmd (ConfigureWindow [CWBorderWidth width])]
                         (windowKernel filename)
  in marginHVAlignF 0 aCenter aCenter kernelF

bitmapButtonF keys filename =
  let kernelF = bitmapDispBorderF 0 filename 
  in fromRight >^=< (pushButtonF keys kernelF)
