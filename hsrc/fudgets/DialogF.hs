{-# LANGUAGE CPP #-}
module DialogF(inputPopupOptF, inputPopupF, passwdPopupOptF,
               passwdPopupF, stringPopupOptF, stringPopupF,
	       confirmPopupF, ConfirmMsg(..),
	       oldConfirmPopupF, oldMessagePopupF,
               messagePopupF) where
import Spacer(marginHVAlignF,marginF)
import Alignment
import PushButtonF(Click(..))
import DButtonF
import FDefaults
import CompOps
import CompSP(preMapSP)
import Defaults(labelFont,bgColor,defaultSep)--buttonFont,fgColor,
import CmdLineEnv(argFlag)
import DDisplayF
import PopupF(popupShellF)
import Fudget
import Geometry(pP)
import Spops
import SpEither(filterJustSP,filterRightSP)
import StringF
import InputF(InF(..))
import InputMsg(ConfirmMsg(..),toConfirm,fromConfirm,InputMsg(..),inputLeaveKey)
--import EitherUtils(isM)
import Data.Maybe(isJust,maybeToList)
--import TextF(textF')
--import ListRequest(replaceAll)
--import NullF(startupF)
import Placer(vBoxF,hBoxF)
import AutoPlacer(autoP')
import Sizing
import Xtypes() -- synonyms, for hbc
import Graphic -- instances (+ class Graphic, because of the monomorphism restr)
import Drawing
import GCAttrs() -- instances

default(Int)

oldMessagePopupF = popupShellF "Message" Nothing (labelabove 50 ok >=^< Left)
oldConfirmPopupF = popupShellF "Confirm" Nothing (labelabove 50 confirm >=^< Left)

-- Grr! Type signatures required because of the mononorphism restriction
confirmPopupF :: Graphic msg => F msg (msg,ConfirmMsg)
messagePopupF :: Graphic msg => F msg (msg,Click)
confirmPopupF = msgPopupF confirm
messagePopupF = msgPopupF ok

msgPopupF buttons =
    popupShellF "Confirm" Nothing
      (filterRightSP>^^=< vBoxF (msgF>+<buttons)>=^<Left . layoutfix)
  where
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
    msgF :: Graphic a => F (Drawing lbl a) b
#endif
    msgF = marginHVAlignF 5 aCenter aCenter $ displayF' pm
     where pm = setBgColor [bgColor,"white"] . setSizing Dynamic .
		setFont labelFont . setBorderWidth 0
    layoutfix = PlacedD (autoP' (pP defaultSep 0)) . AtomicD

genStringPopupOptF title inp default' =
    inputPopupOptF title (inp default') (Just default')

genStringPopupF title inp default' =
    filterMaybePairF (genStringPopupOptF title inp default')

stringPopupOptF = genStringPopupOptF "String Entry" oldStringF
stringPopupF default' = filterMaybePairF (stringPopupOptF default')

passwdPopupOptF = genStringPopupOptF "Password Entry" oldPasswdF
passwdPopupF default' = filterMaybePairF (passwdPopupOptF default')

inputPopupOptF :: String -> InF a b -> Maybe b -> F (Maybe String, Maybe a) ((Maybe String, Maybe a), Maybe b)
inputPopupOptF title f default' =
    let stringconfirm =
            (filterDoneSP default' >^^=< vBoxF (f >+< confirm')) >=^< Left
    in  popupShellF title
		   Nothing
		   (marginF 5 ((labelabove 50 stringconfirm >=^^< distPairSP)))

inputPopupF title f def = filterMaybePairF (inputPopupOptF title f def)

button k s = buttonF' (setKeys [([],k)]) s

button' k s =
  if argFlag "okkey" False
  then button k s
  else buttonF s
       -- This is a fix for the problem that when you press return in a
       -- stringPopupF, the next time the popup appears the string in it
       -- isn't selected.

#ifdef __HUGS__
label :: F String a -- for Hugs
#endif
label = displayF' pm
  where pm = setBorderWidth 0 . setBgColor [bgColor,"white"].setFont labelFont

ok = marginHVAlignF 0 aLeft aBottom (button "Return" "OK")
ok' = marginHVAlignF 0 aLeft aBottom (button' "Return" "OK")
cancel = marginHVAlignF 0 aRight aBottom (button "Escape" "Cancel")

confirm = toConfirm >^=< hBoxF (ok >+< cancel) >=^< fromConfirm
confirm' = toConfirm >^=< hBoxF (ok' >+< cancel) >=^< fromConfirm

labelabove len f = filterRightSP >^^=< vBoxF (label >+< f)

filterMaybePairF :: (F a (b, Maybe c)) -> F a (b, c)
filterMaybePairF f = preMapSP filterJustSP liftOpt >^^=< f

liftOpt (x, Nothing) = Nothing
liftOpt (x, Just y) = Just (x, y)

distPairSP = concmapSP (\(x, y) -> otol Left x ++ otol Right y)
  where otol f = maybeToList . fmap f

filterDoneSP =
    let fd s =
            getSP $ \msg ->
            let same = fd s
            in case msg of
	         Right Confirm -> if isJust s then putSP s same else same
		 Right Cancel -> putSP Nothing same
		 Left (InputChange s') -> fd (Just s')
		 Left (InputDone k s') | k /= inputLeaveKey -> putSP (Just s') $
                                                              fd (Just s')
                 Left _ -> same
    in  fd

#ifdef __NHC__
-- nhc bug workaround
blaha=undefined::DisplayF
#endif
