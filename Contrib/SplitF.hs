module SplitF where
import AllFudgets
import HandleF(vHandleF,hHandleF)
import LinearSplitP(linearSplitP)

hSplitF = hSplitF' aCenter
vSplitF = vSplitF' aCenter

hSplitF' = splitF' Horizontal
vSplitF' = splitF' Vertical

splitF' dir alignment fud1 fud2 =
    loopCompThroughRightF $
    groupF startcmds sizeK $
    placerF (linearSplitP dir defaultSep) $
    fud1>+<hF>+<fud2
  where
    startcmds = [XCmd $ ChangeWindowAttributes [CWBackPixmap parentRelative]]

    hF = colinear dir hHandleF vHandleF alignment

    toLoop = Just . High . Left
    out = Just . High . Right
    toHandle = toLoop . Left . Right
    toFud1 = toLoop . Left . Left
    toFud2 = toLoop . Right

    sizeK = K (mapFilterSP route)
    route = message low high

    low (LEvt (LayoutSize size)) = toHandle size
    low event = ignore event
    
    high = either fromLoop (either toFud1 toFud2)
    fromLoop = either (either fromFud1 ignore) fromFud2
    fromFud1 = out . Left
    fromFud2 = out . Right

    ignore _ = Nothing
