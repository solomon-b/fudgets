#define FI foreign import ccall unsafe "cfuns.h"

#define SETI(ctype,fhtype,p1,i1,field,v1) \
  FI seti_/**/ctype/**/_/**/field :: HTA(ctype) -> Int -> (fhtype) -> IO ()

/*  (_casm_ ``((ctype *)%0)[%1].field=%2;'' ((p)::HT(ctype)) (i) (v) :: IO ()) */
#define SINDEX(ctype,p1,i1,v1) \
  FI sindex_/**/ctype :: Addr -> Int -> HT(ctype)  -> IO ()

/*(_casm_ ``((ctype *)%0)[%1]=%2;'' (p) (i) (v) :: IO ())*/

#define SET(ctype,fhtype,p1,field,v1) \
  FI set_/**/ctype/**/_/**/field :: HT(ctype) -> fhtype -> IO ()

/* (_casm_ ``((ctype *)%0)->field=%1;'' (addrOf (p)) (v) :: IO ())*/

#define SETWa(swa,field,v) SET(XSetWindowAttributes,Int,swa,field,v)
#define SETWaXID(swa,field,v) SET(XSetWindowAttributes,XID,swa,field,v)


#define GET(ctype,rhtype,p1,field) \
  FI get_/**/ctype/**/_/**/field :: HT(ctype) -> IO rhtype

#define GETC(ctype,rctype,rhtype,p1,field) GET(ctype,rhtype,p1,field)

/* (_casm_ `` %r = ((ctype *)%0)->field; '' (addrOf (p))) */

#define AGET(ctype,rhtype,p1,field) FI aget_/**/ctype/**/_/**/field :: HT(ctype) -> IO (rhtype)

/* (_casm_ `` %r = &(((ctype *)%0)->field); '' ((p)::HT(ctype))) */

#define INDEX(ctype)  FI index_/**/ctype :: HT(ctype) -> Int -> IO HT(ctype)

/* (\pP iI -> _casm_ ``%r = (ctype *)%0 + (int)%1;'' ((pP) :: HT(ctype)) iI) */

#define CINDEX(t) FI cindex_/**/t :: Addr -> Int -> IO (HT(t))

/* (\pP iI -> _casm_ ``%r = ((t *)%0)[(int)%1];'' pP iI) */

#define CCONST(c) FI const_/**/c :: Int
#define CWORD32(c) FI const_/**/c :: Word32
