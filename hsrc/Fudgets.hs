{-# LANGUAGE CPP #-}
#include "exists.h"
-- | Programmers' index. There is very little documentation here. See the
-- <http://www.altocumulus.org/Fudgets/Manual/ Fudget Library Reference Manual>
-- instead.
module Fudgets(
 -- * GUI
 buttonF, border3dF, buttonBorderF, pushButtonF,
 BMevents(..),
 popupMenuF, Click(..),
 radioGroupF, radioGroupF', intF, passwdF, stringF,
 intInputF, stringInputF, passwdInputF,
 intInputF', stringInputF', passwdInputF',
 toggleButtonF,
 -- ** Popups
 inputPopupOptF, inputPopupF,
 passwdPopupOptF, passwdPopupF, stringPopupOptF, stringPopupF,
 confirmPopupF, ConfirmMsg(..), oldConfirmPopupF, oldMessagePopupF,
 messagePopupF, intDispF, displayF, labelF,
 -- ** Text editor
 EditStop(..), editF,
 EditEvt(..), EditCmd(..), editorF, editorF', selectall, loadEditor,
 newline, EDirection(..), inputEditorF, inputEditorF', EditorF,
 EditStopFn(..), EditStopChoice(..), IsSelect(..),setEditorCursorPos,
 -- ** List and text
 oldFilePickF, PickListRequest(..),
 textF, textF',-- textF'',
 TextRequest(..), TextF, HasInitText(..), HasSizing(..),Sizing(..),
 ListRequest(..),
 replaceAll, replaceAllFrom, deleteItems, insertItems, appendItems,
 changeItems, replaceItems, highlightItems, pickItem, applyListRequest, 
 smallPickListF, labRightOfF, labLeftOfF, labBelowF, labAboveF,
 tieLabelF, menuF, PopupMenu(..), menuPopupF,
 pickListF, moreShellF,pickListF', moreShellF',
 moreFileShellF, moreFileF, moreF, moreF',
 terminalF, cmdTerminalF, TerminalCmd(..),
 -- ** Graphics
 hyperGraphicsF, hyperGraphicsF',GraphicsF,setAdjustSize,
 -- * Fudgets and combinators
 contDynF,
 Fudget(..), F, --F(..),
 listF,
 untaggedListF, loopCompF, loopCompSP, loopF, loopLeftF, loopRightF, loopOnlyF,
 loopThroughRightF, loopCompThroughLeftF, loopCompThroughRightF,
 loopThroughBothF,
 --Message,
 delayF, getF, putF, putsF, startupF, appendStartF, 

 nullF, parF, prodF, absF, bypassF, concatMapF, idF, idLeftF,
 idRightF, mapF, mapstateF, serCompLeftToRightF, serCompRightToLeftF, stubF,
 throughF, toBothF, (>*<), (>+<), (>=^<), (>=^^<),
 (>#+<), (>#==<), (>==<), (>^=<), (>^^=<),
 prepostMapHigh,

 quitIdF, quitF, quitButtonF,

 DynFMsg(..), dynF, dynListF, DynMsg(..), --Either(..),
 FudgetIO(..),
 --Fa(..),Direction, TCommand(..),TEvent(..),

 {- FCommand(..), FEvent(..), K(..), KCommand(..), KEvent(..),
 TCommand(..), TEvent(..), -}
 -- ** Input
 InF(..), InputMsg(..), inputDoneSP, inputLeaveDoneSP,
 inputListSP, inputPairSP, inputThroughF, inputPairF, inputListF, inputChange,
 inputListLF, inputPairLF, -- obsolete
 stripInputSP,
 inputButtonKey, inputLeaveKey, inputMsg, mapInp, stripInputMsg,
 inputDone, inputLeaveDone,
 tstInp,

 -- * Layout
 Orientation(..),
 alignF, marginHVAlignF, layoutModifierF, noStretchF,
 marginF, sepF,
 autoP, flipP, permuteP, revP, idP,
 Alignment(..), aBottom, aCenter, aLeft, aRight, aTop,
 -- barP, rightBelowP,
 dynListLF, LayoutDir(..),
 listLF, nullLF, holeF,
 --rbLayoutF,
 untaggedListLF, LayoutRequest,
 Placer, center, center', fixedh, fixedv, flipPoint,
 flipRect, flipReq,

 NameLayout, LName(..),hvAlignNL, marginHVAlignNL, hBoxNL, hBoxNL',
 nullNL, leafNL, spaceNL, placeNL, 
 listNF, modNL, nameF, nameLayoutF, sepNL, marginNL, vBoxNL, vBoxNL',

 hBoxF, matrixF, placerF, spacerF, spacer1F, revHBoxF, revVBoxF, spacerP,
 tableF, vBoxF, horizontalP, horizontalP', matrixP, matrixP',
 verticalP, verticalP',paragraphP,paragraphP',paragraphP'',
 dynPlacerF, dynSpacerF, -- There are pitfalls with using these...

 Distance(..), Spacer, bottomS, centerS, compS, flipS,
 hAlignS, sizeS, maxSizeS, minSizeS, hCenterS, hMarginS, marginHVAlignS,
 hvAlignS, hvMarginS, idS, leftS, marginS, sepS, noStretchS,
 rightS, topS, vAlignS, vCenterS, vMarginS, tableP, tableP', bubbleF,
 bubblePopupF, bubbleRootPopupF, shellF, PotRequest(..), PotState(..),
 containerGroupF, hPotF, vPotF, popupShellF, popupShellF', PopupMsg(..),
 posPopupShellF, hScrollF, scrollF, scrollShellF, vScrollF,
 ESelCmd(..), ESelEvt(..), SelCmd(..), SelEvt(..), eselectionF,
 selectionF, allcacheF,
 {-
 bitmapdatacacheF, bitmapfilecacheF,
 colorcacheF, fontcacheF, fontcursorcacheF, fstructcacheF, gCcacheF,
 -}
 doubleClickF, Time(..),
 -- * Stream processors
 -- ** Combining stream processors
 (-+-),(-*-),(-==-),
 compEitherSP, idLeftSP, idRightSP, postMapSP, preMapSP,
 prepostMapSP, serCompSP, loopLeftSP, loopSP, loopOnlySP, loopThroughRightSP,
 loopThroughBothSP,
 parSP, seqSP,
 -- ** Stream processor primitives
 SP,nullSP,putSP,putsSP,getSP,
 StreamProcIO(..),runSP, walkSP, pullSP,

 {-SPm(..), bindSPm, getSPm, monadSP, nullSPm, putsSPm, thenSPm, toSPm, unitSPm,-}

 -- ** Convenient stream processors
 idSP, filterSP, filterJustSP, filterLeftSP, filterRightSP, mapFilterSP,
 splitSP, toBothSP,
 concatSP, concSP,
 mapSP, concatMapSP, concmapSP,
 concatMapAccumlSP, mapstateSP, mapAccumlSP,
 zipSP,
 -- ** Stream processor behaviour
 Cont(..),
 appendStartSP, chopSP, delaySP, feedSP,
 splitAtElemSP, startupSP, stepSP,
 cmdContSP, conts, getLeftSP, getRightSP, waitForSP, waitForF, dropSP, contMap,
 -- * System (stdio, files, network, subprocesses)
 -- ** Dialogue IO
 hIOF,
 hIOSuccF, hIOerrF, haskellIOF,
 inputLinesSP, linesSP,
 -- ** Stdio
 outputF, stderrF, stdinF,
 stdioF, stdoutF,
 -- ** Subprocesses
 subProcessF,
 -- ** Files and directories
 appStorageF,
 readDirF, readFileF, writeFileF,
 -- ** Sockets
 Host(..), LSocket(..), Peer(..), Port(..), Socket(..),
 openLSocketF,
 openSocketF, receiverF, transceiverF, transmitterF, asyncTransmitterF,
 asyncTransceiverF,
 -- ** Timer
 Tick(..), timerF,
 --dropF,
 -- ** Running a fudget
 fudlogue, fudlogue', Fudlogue,
 -- * Command line, environment and defaults
 argFlag, argKey, argReadKey, argKeyList, args, progName,
 bgColor, buttonFont, defaultFont, defaultSize,
 defaultPosition, defaultSep, edgeWidth, fgColor, labelFont, look3d,
 menuFont, options, paperColor, shadowColor, shineColor,
 -- * Utilities for the Either type
 filterLeft, filterRight, isLeft, isRight, mapEither,
 fromLeft, fromRight, plookup, splitEitherList, stripEither, stripLeft,
 --  mapfilter, isM, stripMaybe, stripMaybeDef,
     -- use Maybe.mapMaybe,isJust,fromJust,fromMaybe!
 stripRight, swapEither,
 -- * Geometry
 (=.>),
 Line(..), Point(..), Rect(..), Size(..), Move(..), confine, diag, freedom,
 growrect, inRect, lL, line2rect, moveline, moverect, origin, pMax,
 pMin, pP, padd, plim, pmax, pmin, posrect, psub, rR, rect2line,
 rectMiddle,
 rmax, rsub, scale, scalePoint, sizerect,
 --xcoord, ycoord, rectpos, rectsize,
 -- * Utilities
 aboth, anth, gmap, -- afst, asnd, dropto, 
 issubset, lhead, loop, lsplit, ltail, mapPair, number, oo, pair,
 pairwith, part, remove, replace, swap, unionmap, module FudVersion,

 -- * Xlib types
 XCommand, XEvent, Path(..),
 Button(..), ColorName(..), FontName(..), KeySym(..), FontStruct, RGB(..),
 WindowAttributes, 
 ModState(..), Modifiers(..), 
  -- * Graphics and drawings
 CoordMode(..),Shape(..),
 DrawCommand(..),fillCircle,drawCircle,
 Graphic(..),
 Drawing(..),atomicD,labelD,up,boxD,hboxD,hboxD',vboxD,vboxD',tableD,tableD',
 hboxcD,hboxcD',vboxlD,vboxlD',matrixD,matrixD',
 attribD,softAttribD,hardAttribD,fontD,fgD,stackD,spacedD,placedD,
 blankD,filledRectD,rectD,
 DPath(..),
#ifdef USE_EXIST_Q
 Gfx,
#endif
 g,
 FixedDrawing(..),FixedColorDrawing(..),gctx2gc,
 FlexibleDrawing(..),flex,flex',
 filler,hFiller,vFiller,frame,frame',ellipse,ellipse',arc,arc',
 filledEllipse,filledEllipse',filledarc,filledarc',
 lpar,rpar,lbrack,rbrack,lbrace,rbrace,
 triangleUp,triangleDown,filledTriangleUp,filledTriangleDown,
 BitmapFile(..),
 ColorGen(..),FontGen(..),FontSpec,ColorSpec,--Name(..),--ColorFallback(..),
#ifdef USE_EXIST_Q
 colorSpec,fontSpec,
#endif
 GCtx,rootGCtx,wCreateGCtx,createGCtx,gcFgA,gcBgA,gcFontA,
 GCAttributes(..),GCFillStyle(..),GCCapStyle(..),GCLineStyle(..),GCFunction(..),
 Width(..),

 -- * Customisation
 Customiser(..), PF(..), standard,

 HasClickToType(..), HasVisible(..), HasFontSpec(..), setFont,
 HasKeys(..), HasWinAttr(..), --HasTitle(..),
 HasBorderWidth(..), HasBgColorSpec(..), HasFgColorSpec(..), HasMargin(..),
 setBgColor,setFgColor,
 HasAlign(..),
 {- HasAllowedChar(..), HasShowString(..), -}
 setAllowedChar, setShowString, setCursorPos,
 HasCache(..),
 setDeleteQuit,setDeleteWindowAction,DeleteWindowAction(..),--HasDeleteQuit(..),
 HasInitSize(..),
 HasInitDisp(..),
 --setInitDisp, getInitDisp,
 setSpacer,
 HasStretchable(..),
 HasLabelInside(..),setPlacer,

 ShellF, shellF', setInitPos,
 unmappedSimpleShellF, unmappedSimpleShellF',
 ButtonF, buttonF', buttonF'', setLabel,
 DisplayF, displayF',-- displayF'',
 labelF', --labelF'',
 StringF, stringF', stringF'', setInitString, setInitStringSize,
 passwdF', passwdF'',
 intF', intF'',
 intDispF', --intDispF'',
 -- * Miscellaneous
 gcWarningF,bellF,
 --D_IOError,
 -- * Time
#ifdef VERSION_old_time
 getTime,getLocalTime,
#endif
#ifdef VERSION_time
 getCurrentTime,getZonedTime,
#endif
 -- * Debugging
 spyF,teeF, ctrace,showCommandF) where

import AllFudgets -- hiding (Cont,PCont,Fa)
import FudVersion

-- I hate this file /TH
-- I still hate this file /TH
