module FreeGroupF(freeGroupF) where
--import Fudget
import EitherUtils
import Path(here)
import LayoutRequest
import Geometry
--import Command
--import Event
--import Xtypes

import Dlayout(groupF)
import UserLayoutF(userLayoutF)
import Spops
--import SpEither(mapFilterSP)
import NullF(nullK)
import SerCompF(absF)
import LoopCompF(loopCompF)
import CompOps
import Defaults(bgColor)
import GreyBgF(changeBg)

freeGroupF f =
    loopCompF (absF placeSP0>+<innerF)
  where
    innerF = userLayoutF $
             stripEither >^=< groupF [] (bgK nullK) f >=^< Right

    placeSP0 = placeSP False here (rR 0 0 0 0) {- dummy initial path & place -}

    placeSP placed path place =
      let same = placeSP placed path place
      in 
      getSP $ \ msg ->
      case msg of
        Left (path',layoutmsg) ->
	  case layoutmsg of
	    LayoutRequest req ->
	      let place' = sizerect place (minsize req)
	      in putSP (Right layoutmsg) $ placeSP False path' place'
	    LayoutMakeVisible _ _ -> putSP (Right layoutmsg) same
	    LayoutScrollStep _ -> putSP (Right layoutmsg) same
            _ -> same -- ignore other msgs for now
	Right (Right pos) ->
	  let place' = posrect place pos
	  in putSP (Left (path,place')) $
	     placeSP True path place'
	Right (Left newtotsize) ->
	  let place' = sizerect place newtotsize
	  in ifSP (not placed || place'/=place)
                  (putSP (Left (path,place'))) $
	    --  ^^ dangerous optimization?
	     placeSP True path place'

ifSP b th = if b then th else id

bgK = changeBg bgColor
