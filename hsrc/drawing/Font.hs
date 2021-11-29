module Font(CharStruct(..), FontDirection(..), FontStruct, FontStructList(..),
            FontProp(..), split_string, string_len, update_font_id, -- string_rect_mono,
            string_rect, font_id, font_descent, font_ascent, font_range, font_prop,
	    char_width, char_rbearing,
	    string_box_size, string_bounds,
            next_pos, poslist, linespace,fsl2fs) where
--import Sort(sortLe)
import Geometry(Point(..), Rect(..), pP, rR, rectsize, xcoord)
import Xtypes(Atom,FontId)
--import Utils(aboth)
--import EitherUtils(mapMaybe)
--import HbcUtils(mapFst)
import Data.List(mapAccumL)
import Maptrace(ctrace) -- debugging
import Data.Array
--import qualified Data.Array as LA
--import qualified LA -- GHC bug workaround, can't use LA.!
--import Data.Ix

default(Int)

data CharStruct = CharStruct Int -- lbearing
                             Int -- rbearing
			     Int -- width
			     Int -- ascent
			     Int -- descent
		  deriving (Eq, Ord, Show, Read)

char_lbearing (CharStruct lb rb w a d) = lb
char_rbearing (CharStruct lb rb w a d) = rb
char_width    (CharStruct lb rb w a d) = w
char_ascent   (CharStruct lb rb w a d) = a
char_descent  (CharStruct lb rb w a d) = d

data FontDirection = FontLeftToRight |
                     FontRightToLeft 
                     deriving (Eq, Ord, Show, Read, Enum)

data FontProp = FontProp Atom Int deriving (Eq, Ord, Show, Read)

-- Only 8-bit characters and 2-byte matrixes. See fsl2fs too!
data FontStruct = FontStruct FontId
                             FontDirection
			     Char -- first character
			     Char -- last character
			     Bool -- all chars exist
			     Char -- default char
			     [FontProp]
			     CharStruct -- min bounds
			     CharStruct -- max bounds
			     (Maybe (Array Char CharStruct))
			     Int -- logical extent above baseline for spacing
			     Int -- logical extent below baseline for spacing 
                  deriving (Eq, Ord, Show, Read)

font_id      (FontStruct fid fd fc lc all' dc fps minb maxb ca asc de) = fid
font_ascent  (FontStruct fid fd fc lc all' dc fps minb maxb ca asc de) = asc
font_descent (FontStruct fid fd fc lc all' dc fps minb maxb ca asc de) = de
per_char     (FontStruct fid fd fc lc all' dc fps minb maxb ca asc de) = ca
max_bounds   (FontStruct fid fd fc lc all' dc fps minb maxb ca asc de) = maxb
min_bounds   (FontStruct fid fd fc lc all' dc fps minb maxb ca asc de) = minb
font_range   (FontStruct fid fd fc lc all' dc fps minb maxb ca asc de) = (fc,lc)
default_char (FontStruct fid fd fc lc all' dc fps minb maxb ca asc de) = dc
font_prop    (FontStruct fid fd fc lc all' dc fps minb maxb ca asc de) = fps

update_font_id (FontStruct _ fd fc lc all' dc fps minb maxb ca asc de) fid = 
  FontStruct fid fd fc lc all' dc fps minb maxb ca asc de

linespace fs = font_ascent fs + font_descent fs

char_struct default' fs c =
    case per_char fs of
      Nothing -> default' fs
      Just ca -> --ca ! c
                 ca ! (if inRange (font_range fs) c   -- or: bounds ca
                       then -- debugging
		            if inRange (bounds ca) c
			    then c
			    else ctrace "fontrange" (font_range fs,bounds ca,c) (default_char fs)
		       else let c' = default_char fs
		            in if inRange (bounds ca) c'
			       then c'
			       else ctrace "fontrange" (font_range fs,"default char",c') ' ')

lbearing fs = char_lbearing . char_struct min_bounds fs
rbearing fs = char_rbearing . char_struct max_bounds fs
poslist fs = map (char_width . char_struct max_bounds fs)
next_pos fs = sum . poslist fs

-- string_bounds gives enclosing rect with respect to first character's origin
string_bounds fs [] = Rect (Point 0 0) (Point 0 0)
string_bounds fs s =
    let cs = char_struct max_bounds fs
        x = lbearing fs (head s)
        y = -(maximum . map (char_ascent . cs)) s
        width = next_pos fs (take (length s - 1) s) + rbearing fs (last s)
        height = (maximum . map (char_descent . cs)) s - y
    in  Rect (Point x y) (Point width height)

string_len fs s = (xcoord . rectsize . string_bounds fs) s

string_rect fs s =
    rR 0 (-font_ascent fs) (string_len fs s) (linespace fs)

string_box_size fs s = pP (next_pos fs s) (linespace fs)

split_string:: FontStruct -> String -> Int -> (String,String,Int)
split_string fs s x =
   -- find the first char that ends to the right of the wanted x position
   case dropWhile (\(_,_,xr)->xr<x) nxs of
     (n,xl,xr):_ ->
	-- xl<=x<=xr, wanted x position is inside the nth character
	if x-xl<xr-x
	then split n -- left edge of nth char is closer
	else split (n+1) -- right edge of nth char is closer
     [] -> (s,[],n) -- x position is after the last char of the string
  where
    split n = case splitAt n s of (s1,s2) -> (s1,s2,n)

    --n=length s, nxs= string & screen positions of all characters in the string
    ((n,_),nxs) = mapAccumL (\(n,x) w -> ((n+1,x+w),(n,x,x+w))) ((0,0)::(Int,Int)) ws

    -- Width of all characters:
    ws = poslist fs s
    

{- old:
    let dist s' = abs (next_pos font s' - x)
        nearer (pre1, _, _) (pre2, _, _) = dist pre1 <= dist pre2
	better x y = if nearer x y then x else y
    in foldr1 better (allsplits s)

--allsplits s = [(take n s, drop n s, n) | n <- [0 .. length s]])
allsplits [] = [([],[],0)]
allsplits xxs@(x:xs) = ([],xxs,0): map (\(xs,ys,n)->(x:xs,ys,n+1)) (allsplits xs)
-}

--------

-- This is a temporary fix until we know how to construct Haskell arrays from C
data FontStructList = FontStructList
                             FontId
                             FontDirection
			     Char -- first character
			     Char -- last character
			     Bool -- all chars exist
			     Char -- default char
			     [FontProp]
			     CharStruct -- min bounds
			     CharStruct -- max bounds
			     (Maybe [CharStruct])
			     Int -- logical extent above baseline for spacing
			     Int -- logical extent below baseline for spacing 
                  deriving (Eq, Ord, Show, Read)

fontl_prop (FontStructList fid fd fc lc all' dc fps minb maxb ca asc de) = fps

fsl2fs (FontStructList fid fd fc lc all' dc fps minb maxb optclist asc de) =
    FontStruct fid fd fc lc all' dc fps minb maxb optca asc de
  where optca = fmap l2a optclist
        l2a clist = array (fc, lc) (zip ixs clist)
	-- !! This assumes single byte font, or 2 byte matrix font.
	-- !! Linear 16-bit fonts will not work.
	-- ! Using a linear array for a 2 byte matrix font wastes space!
	ixs = [toEnum (256*byte1+byte2) | byte1<-range (min_byte1,max_byte1),
	                         byte2<-range (min_byte2,max_byte2)]
	(min_byte1,min_byte2) = fromEnum fc `divMod` 256
	(max_byte1,max_byte2) = fromEnum lc `divMod` 256
{-
-- hack to circumvent limitation in generational garbage collector

data Array a b = Arr (a,a) (LA.Array Int (LA.Array Int b))
	deriving (Eq,Ord,Show,Read)

array :: (Ix a, Enum a) => (a,a) -> [(a,b)] -> Array a b
array bds l = Arr bds (LA.listArray (0,dix b2i-dix b1i)
                       [LA.array rng (filter (inRange rng.fst) (mapFst fromEnum l))
                        | offs <- [b1i,b1i+maxsize..b2i],
			  rng <- [(offs,(offs+maxsize-1) `min` b2i)]])
  where (b1i,b2i) = aboth fromEnum bds

(!) :: (Ix a, Enum a) => Array a b -> a -> b
(!) (Arr (b1,b2) a) i = (a `LA.sub` dix (ii-fromEnum b1)) `LA.sub` ii where ii = fromEnum i

bounds :: (Ix a, Enum a) => Array a b -> (a,a)
bounds (Arr bds a) = bds
-}
maxsize = 255
dix i = i `div` maxsize
