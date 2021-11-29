{-# LANGUAGE CPP #-}
module GCAttrs(module GCAttrs,Cont(..)) where
--import Fudget
--import NullF(F,K)
import FudgetIO
import Xtypes
import EitherUtils(Cont(..))
import Font(FontStruct,font_id,font_range, font_prop, update_font_id)
import Color(tryAllocNamedColor,tryAllocColor)
import LoadFont(listFontsWithInfo,loadFont,loadQueryFont)
import FontProperty(fontProperty)
import CmdLineEnv(argKey)
import Utils(aboth,segments)
--import ListUtil(chopList,breakAt)

#include "exists.h"

data FontData
   = FID FontStruct
   | FS FontStruct

fdFontId (FID fs) = font_id fs
fdFontId (FS fs) = font_id fs

fontdata2struct (FS fs) k = k fs
fontdata2struct (FID fs) k = k fs

--newtype Name = Name String deriving (Eq,Show)
  -- The type Name is used instead of String since String is a type synonym
  -- and therefore can't be made an instance of a class.

#ifdef USE_EXIST_Q
data ColorSpec = EXISTS(a) TSTHACK((Show EQV(a),ColorGen EQV(a)) =>) ColorSpec EQV(a)
 -- deriving Show -- doesn't work because of a HBC bug
instance Show ColorSpec where showsPrec n (ColorSpec c) = showsPrec n c

data FontSpec = EXISTS(a) TSTHACK((Show EQV(a),FontGen EQV(a)) =>) FontSpeci EQV(a)
 -- deriving Show -- doesn't work because of a HBC bug
instance Show FontSpec where showsPrec n (FontSpeci f) = showsPrec n f

colorSpec x = ColorSpec x
fontSpec x = FontSpeci x
#else
data ColorSpec = StringCS ColorName | RGBCS RGB | PixelCS Pixel | ListCS [ColorSpec] deriving (Show)
data FontSpec = StringFS FontName | FontIdFS FontId | FontStructFS FontStruct | ListFS [FontSpec] deriving (Show)
#endif

--data ColorFallback = CF ColorName ColorName

--type GCAttrsSpec = GCAttributes ColorSpec FontSpec

class ColorGen a where
  IFNOEXIST(colorSpec :: a -> ColorSpec)
  tryConvColorK :: FudgetIO f => a -> Cont (f i o) (Maybe Pixel)

  -- Methods with defaults, to be overidden only in the Char instance:
  IFNOEXIST(colorSpecList :: [a] -> ColorSpec)
  convColorListK :: FudgetIO f => [a] -> Cont (f i o) (Maybe Pixel)

  convColorListK = convList tryConvColorK
  IFNOEXIST(colorSpecList = ListCS . map colorSpec)

convColorK c = tryConvColorK c . maybe err
  where err = error ("Can't allocate color: "++show c)

class FontGen a where
  IFNOEXIST(fontSpec :: a -> FontSpec)
  tryConvFontK :: FudgetIO f => a -> Cont (f i o) (Maybe FontData)

  -- Methods with defaults, to be overidden only in the Char instance:
  IFNOEXIST(fontSpecList :: [a] -> FontSpec)
  convFontListK :: FudgetIO f => [a] -> Cont (f i o) (Maybe FontData)

  IFNOEXIST(fontSpecList = ListFS . map fontSpec)
  convFontListK  = convList tryConvFontK

convFontK f k = tryConvFontK f $ maybe (tryConvFontK "fixed" $ maybe err k) k
  where err = error ("Can't load font: "++show f)

convList try xs cont = conv xs
  where conv [] = cont Nothing
        conv (x:xs) = try x $ maybe (conv xs) (cont . Just)


#ifdef USE_EXIST_Q
instance ColorGen ColorSpec where tryConvColorK (ColorSpec c) = tryConvColorK c
instance FontGen FontSpec where tryConvFontK (FontSpeci c) = tryConvFontK c
#else
instance ColorGen ColorSpec where
  colorSpec = id
  tryConvColorK cs =
    case cs of
      StringCS s -> tryConvColorK s
--      NameCS name -> tryConvColorK name
      PixelCS pixel -> tryConvColorK pixel
      RGBCS rgb -> tryConvColorK rgb
--      FallbackCS fb -> tryColorColorK fb
      ListCS cs -> tryConvColorK cs

instance FontGen FontSpec where
  fontSpec = id
  tryConvFontK fs =
    case fs of
--      NameFS (Name name) -> tryConvFontK name k
      StringFS name -> tryConvFontK name
      FontStructFS fstr -> tryConvFontK fstr
      ListFS fs -> tryConvFontK fs
#endif

--instance ColorGen ColorFallback where
--  IFNOEXIST(colorSpec = FallbackCS)
--  convColorK = convColorFallbackK

--convColorFallbackK (CF c1 c2) = allocNamedColorDefPixel defaultColormap c1 c2

instance ColorGen c => ColorGen [c] where
  IFNOEXIST(colorSpec = colorSpecList)
  tryConvColorK = convColorListK

instance FontGen a => FontGen [a] where
  IFNOEXIST(fontSpec = fontSpecList)
  tryConvFontK = convFontListK

-- To be able to allow Strings as color names, we have to make an instance for
-- Char. We actually don't want single Chars to be allowed as color names, but
-- to avoid run-time errors they are allowed (and treated as one-char Strings).
instance ColorGen Char where
  IFNOEXIST(colorSpec c = StringCS [c])
  IFNOEXIST(colorSpecList s = StringCS s)
  tryConvColorK c = convColorListK [c]
  convColorListK s k = tryAllocNamedColor defaultColormap s (k . fmap colorPixel)

-- In the "auto mode":

-- if a font is less than 256 chars, load it as is
-- if a font is monospaced, reuse the FontStruct, inserting
--   a font ID obtained for the font via loadFont (as the font name was returned
--   by listFontsWithInfo, we are safe assuming that it exists)
-- if a font is proportional and large then keep its FID in order
--   to query the server for characters metrics.

getFontData :: FudgetIO f => [Char] -> Cont (f i o) (Maybe FontData)
getFontData =
    case usefontstructs of
      "yes" -> qf
      "no" -> lf
      _ -> autof
  where
    qf fname k = loadQueryFont fname (k . fmap FS)
    lf fname k = 
      listFontsWithInfo fname 1 $ \ fis ->
      case fis of
        [] -> k Nothing
        (fn,fs):_ -> loadFont fname $ \fid -> k $ Just $ FID $ update_font_id fs fid
    autof fname k =
      listFontsWithInfo fname 1 $ \ fis ->
      case fis of
        [] -> k Nothing
	(fn,fs):_ -> let fprops = font_prop fs
                     in  fontProperty fprops "SPACING" $ \spacing ->
                         fontProperty fprops "FONT" $ \font ->
                         if char_count<=256
	                   then qf fn k
                           else let fscons = if (fixed_width font spacing)
                                               then FS
                                               else FID
                                in  loadFont fname $ \fid ->
                                    k $ Just $ fscons $ update_font_id fs fid
	   where
	     char_count = hi-lo
	     (lo,hi) = aboth fromEnum (font_range fs)

             fixed_width fnt spcng = 
               let spc = segments (/='-') fn
                   spct = segments (/= '-') fnt'
                   monosp = ["m", "c", "M", "C"]
                   [fnt', spcng'] = map (\s -> case s of
                                                 Just c -> c
                                                 _      -> "\xFF") [fnt, spcng]
                   lspc = length spc
                   lspct = length spct
               in  spcng' `elem` monosp ||                     -- font property tells "monospaced"
                   lspct > 11 && (spct !! 11) `elem` monosp || -- from font property if XLFD
                   lspc > 11 && (spc !! 11) `elem` monosp ||   -- from font name if XLFD
                   lspc == 1 && (head spc) == "fixed"          -- no XLFD, guess by font alias

{--
	     fixed_width = spc !! 11 `elem` ["m","c"]
--}
{--
	     fixed_width
               | (spacing == (Just c)) = c `elem` monosp
               | ((length spct) > 11) = (spct !! 11) `elem` monosp
               | ((length spc) > 11) = (spc !! 11) `elem` monosp
               | ((length spc) == 1) && ((head spc) == "fixed") = True
               | otherwise = False

	     fixed_width = if length spc>11
			   then spc !! 11 `elem` ["m","c"]
			   else --XFLD is missing, assume proportional, unless
			        --the name of the font is "fixed"
                                fn=="fixed"

	     spc = chopList (breakAt '-') fn
             spct = chopList (breakAt '-') font
             monosp = ["m", "c", "M", "C"]
--}

instance FontGen Char where
  IFNOEXIST(fontSpec c = StringFS [c])
  IFNOEXIST(fontSpecList s = StringFS s)
  tryConvFontK f = convFontListK [f]
  convFontListK = getFontData

--instance ColorGen Name where
--  IFNOEXIST(colorSpec = NameCS)
--  tryConvColorK (Name s) = tryConvColorK s

tryConvColorRGBK rgb k = tryAllocColor defaultColormap rgb (k . fmap colorPixel)

instance ColorGen RGB where
  IFNOEXIST(colorSpec = RGBCS)
  tryConvColorK = tryConvColorRGBK

--instance FontGen Name where
--  IFNOEXIST(fontSpec = NameFS)
--  convFontK (Name s) = safeLoadQueryFont s
  
instance ColorGen Pixel where
  IFNOEXIST(colorSpec = PixelCS)
  tryConvColorK p k = k (Just p)

instance FontGen FontStruct where
  IFNOEXIST(fontSpec = FontStructFS)
  tryConvFontK fs k = k (Just (FS fs))

{--

instance FontGen FontId where
  IFNOEXIST(fontSpec = FontIdFS)
  tryConvFontK fs k = k (Just (FID fs))

--}

--convGCSpecK :: GCSpec -> (GCAttributeList->FontStruct->K i o) -> K i o
convGCSpecK fs attrs = gcattrsK fs attrs []
  where
    gcattrsK fs [] outattrs dr = dr (reverse outattrs) fs
    gcattrsK fs (attr : attrs) outattrs dr =
      let cp attr' = gcattrsK fs attrs (attr' : outattrs) dr
      in case attr of
	   GCForeground colspec ->
	     convColorK colspec $ \fg ->
	     gcattrsK fs attrs (GCForeground fg : outattrs) dr
	   GCBackground colspec ->
	     convColorK colspec $ \fg ->
	     gcattrsK fs attrs (GCBackground fg : outattrs) dr
	   GCFont fspec ->
	     convFontK fspec $ \fs' ->
	     gcattrsK fs' attrs (GCFont (fdFontId fs') : outattrs) dr
	   GCFunction f -> cp (GCFunction f)
	   GCLineWidth w -> cp (GCLineWidth w)
	   GCLineStyle s -> cp (GCLineStyle s)
	   GCCapStyle s -> cp (GCCapStyle s)
	   GCJoinStyle s -> cp (GCJoinStyle s)
	   GCSubwindowMode m -> cp (GCSubwindowMode m)
	   GCGraphicsExposures b -> cp (GCGraphicsExposures b)

gcFgA,gcBgA :: c -> [GCAttributes c FontSpec]
gcBgA c = [GCBackground c]
gcFgA c = [GCForeground c]

gcFontA :: f -> [GCAttributes ColorSpec f]
gcFontA f = [GCFont f]


usefontstructs = argKey "fontstructs" "auto"
