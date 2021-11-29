-- replaces modules AlignF and SepF
module Spacer(module Spacer,Distance(..)) where
--import LayoutRequest
import Spacers
import Placer(spacer1F)

marginHVAlignF m h v = spacer1F $ marginHVAlignS m h v

alignF uladd bradd halign valign =
  spacer1F (hvMarginS uladd bradd `compS` hvAlignS halign valign)

layoutModifierF = spacer1F . layoutModifierS

noStretchF fh fv = spacer1F (noStretchS fh fv)

sepF = spacer1F . sepS
marginF = spacer1F . marginS
