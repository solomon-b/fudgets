module HandleF(hHandleF,vHandleF) where
import AllFudgets

hHandleF = handleF' Horizontal 108
vHandleF = handleF' Vertical 116

sepSize = max 2 defaultSep -- !! see note about layoutreq below
sepD dir =
    -- the size of the drawing must be sepSize (in the dir direction)
    placedD (margS d (d2-d) `spacerP` linearP dir 0) $
    boxD [fgD [shadowColor,"black"] l,fgD [shineColor,"white"] l]
  where l = g line
        d2 = sepSize-2
	d = d2 `div` 2

	margS = colinear dir hMarginS vMarginS
	line = colinear dir vFiller hFiller 1

handleF' dir cur alignment =
    --showCommandF "handleF" $
    nullSP >^^=<
    postMapLow post (groupF startcmds handleK0 (labelF (sepD dir)))
    >=^< Left
    --windowF startcmds handleK0
  where
    -- Hack: all layout requests must come from the same address, otherwise
    -- two boxes will be placed instead of one...
    post ([L],cmd@(LCmd _)) = ([R,R],cmd)
    post tcmd = tcmd

    startcmds =
      [--layoutRequestCmd layoutreq,
       XCmd $ ChangeWindowAttributes [CWEventMask eventmask,
                                      CWBackPixmap parentRelative]]

    eventmask = [ButtonPressMask,ButtonReleaseMask,ButtonMotionMask]

    -- It's important that the size of sepD and this request agree!!
    layoutreq = plainLayout (diag sepSize) isHoriz (not isHoriz)
    isHoriz = dir==Horizontal
    wantposreq size p = layoutreq{wantedPos=Just(p,size,alignment)}

    handleK0 = 
      setFontCursor cur $
      handleK

    handleK = idleK 0 0
      where
        idleK parentp size = getK $ message low high
	  where
	    same = idleK parentp size
	    high size' = idleK parentp size'
	    low event =
	      case event of
		XEvt ButtonEvent {type'=Pressed,rootPos=pabs} ->
		  dragK parentp size (pabs-parentp) pabs
		LEvt (LayoutPos parentp') -> idleK parentp' size
		_ -> same

	dragK parentp size refp curp = getK $ message low high
	  where
	    moveto = dragK parentp size refp
	    same = moveto curp
	    putpos = putK . Low . layoutRequestCmd . wantposreq size

            high size' = dragK parentp size' refp curp

	    low event =
	      case event of
		LEvt (LayoutPos parentp') -> dragK parentp' size refp curp
		XEvt ButtonEvent {type'=Released,rootPos=curp0'} ->
		    (if curp'==curp
		    then id
		    else putpos (curp'-refp)) $
		    idleK parentp size
		  where curp' = constrain size (curp0'-refp)+refp
		XEvt MotionNotify {rootPos=curp0',state=mods} ->
		    if curp'==curp || Shift `elem` mods
		    then same
		    else putpos (curp'-refp) $
		         moveto curp'
		  where curp' = constrain size (curp0'-refp)+refp
		_ -> same

--constrain _ = id
--{-
-- Try to limit split position to reasonable values:
constrain size =
  if size>defaultSep
  then pmin (size-defaultSep) . pmax defaultSep
  else id
--}
