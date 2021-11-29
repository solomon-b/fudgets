module MoreF(
  moreF,moreF',
  pickListF,pickListF',PickListRequest(..)
) where

import Fudget
--import Xtypes
import ResourceIds() -- synonym ColorName, for hbc
import Spops
import Geometry
import ScrollF(oldVscrollF,grabScrollKeys)
import TextF
import Spacer(marginF)
import SerCompF(absF)
import Loops(loopThroughRightF)
import CompOps((>=^<))
import StringUtils(rmBS,expandTabs)
import Defaults(labelFont,paperColor)
import GCAttrs() -- instances

import FDefaults
--import Alignment
import InputMsg(InputMsg,mapInp)
import ListRequest(ListRequest(..),replaceAll,replaceItems,applyListRequest)

txtF' pmod = marginF 5 $
             --alignSepF 5 aLeft aTop $
	     textF' pmod

--stringListF :: Size -> FontName -> F TextRequest (InputMsg (Int,String))
stringListF' size pmod = oldVscrollF grabScrollKeys (size,size) (txtF' pmod)

moreF = moreF' standard

moreF' :: Customiser TextF -> F [String] (InputMsg (Int,String))
moreF' pmod =
    stringListF' (pP 480 260) pmod' >=^<(replaceAll.map (rmBS.expandTabs 8))
  where
    pmod' = pmod.setBgColor paperColor

type PickListRequest a = ListRequest a

pickListF = pickListF' standard

pickListF' :: Customiser TextF -> (a->String) -> F (PickListRequest a) (InputMsg (Int,a))
pickListF' pmod show =
    loopThroughRightF (absF (pickSP [])) altListF
  where
    pmod' = pmod.setFont labelFont
    altListF = oldVscrollF grabScrollKeys (Point 240 260,Point 480 390) (txtF' pmod')
    pickSP alts =
      getSP $ \msg ->
      case msg of
        Right plreq@(ReplaceItems from cnt newalts') ->
	  let alts' = applyListRequest plreq alts
	      newalts = map show newalts'
	  in putSP (Left (replaceItems from cnt newalts)) $
	     evalSpine alts' $ -- prevents a space leak
	     pickSP alts'
        Right (HighlightItems ns) -> putSP (Left (HighlightItems ns)) $ pickSP alts
	Right (PickItem n) ->  putSP (Left (PickItem n)) $ pickSP alts
	Left msg -> putSP (Right (mapInp (\(n,_)->(n,alts!!n)) msg)) (pickSP alts)

evalSpine [] = id
evalSpine (x:xs) = evalSpine xs
