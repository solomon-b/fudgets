module FontProperty (fontProperty) where

import Font(FontProp(..))
import InternAtom(internAtom,atomName)
import Xtypes(Atom(..))

-- The font_prop element of the FontStruct contains a list of FontProp,
-- each containing a pair of Int's: the first is the property name atom number,
-- the secind is the property value atom number. To get a property of a font
-- (which may or may not exist) it is necessary to try to get the atom
-- number for the property name, and, if successful, find out the element
-- of the list containing that number. Then, using the second number 
-- in the element found, retrieve the value. If more than one property 
-- with the same name is found, only the first will be returned.

fontProperty fsprops propn cont = 
  let match an' (FontProp an vn) = an' == an
      valatom (FontProp an vn) = vn
  in  internAtom propn True $ \pna ->
      case pna of
        Atom 0 -> cont Nothing
        a ->
          case (map valatom (filter (match a) fsprops)) of
            [] ->   cont Nothing
            vn:_ -> atomName (Atom (fromIntegral vn)) $ \vna -> cont vna


