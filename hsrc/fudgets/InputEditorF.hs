{-# LANGUAGE CPP #-}
module InputEditorF(
    EditorF,
    inputEditorF,inputEditorF',
    editorF,editorF')
  where
import Editor(oldEditorF,loadEditor)
import Edit(EditEvt(..))
--import InputMsg(InputMsg(..))
import CompOps
import SpEither(mapFilterSP)
import Spops(concatMapSP)
import ResourceIds() -- synonym FontName, for hbc
import GCAttrs --(FontSpec,fontSpec)
import Defaults(defaultFont)
import FDefaults

#include "defaults.h"

inputEditorF = inputEditorF' standard

inputEditorF' pm =
    mapFilterSP change >^^=< editorF' pm >=^^< concatMapSP loadEditor
  where
    change (EditChange inputmsg) = Just inputmsg
    change _ = Nothing

editorF = editorF' standard

editorF' customiser = oldEditorF font
  where
    font = fromMaybe (fontSpec defaultFont) $ getFontSpecMaybe ps
    ps = (customiser::(Customiser EditorF))  (Pars [])

newtype EditorF = Pars [Pars]

data Pars
  = FontSpec FontSpec

parameter_instance(FontSpec,EditorF)
