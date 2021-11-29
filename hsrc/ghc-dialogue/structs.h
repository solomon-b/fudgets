--- New stuff for GHC 4.08.1, 2000-11-25 /TH

#define IORC(p,t,c) instance PrimResult (IO (p)) (IO (t)) where{unmarshall=(c)}
#define CA(t,p,c) instance PrimArg (t) (p) r where {marshall f = f .(c)}

#define IDR(t) instance PrimResult (IO (t)) (IO (t)) where {unmarshall=id}
#define IDA(t) CA(t,t,id)
#define IDAR(t) IDR(t);IDA(t);INSTCCALL(t)
#define IDAR0(t) IDR(t);IDA(t)

#define ENUMR(t) IORC(Int,t,fmap toEnum)
#define ENUMA(t) CA(t,Int,fromEnum)
#define ENUMAR(t) ENUMR(t);ENUMA(t)

--- Old stuff, modified to fit with the new stuff, 2000-11-25 /TH

#define HT(ctype) C/**/ctype
#define HTA(ctype) HT(ctype)Array

#define NEWTYPE(t) newtype t = t Addr deriving (Eq,Show)
#define HASADDR(t) instance HasAddr t where {addrOf (t addr) = addr}
#if 0
#define INSTCCALL(t) instance CCallable t;instance CReturnable t
#else
#define INSTCCALL(t)
#endif

#define ISTORE(t) \
  instance Storable t where { \
    sizeOf (t a) = sizeOf a; \
    alignment (t a) = alignment a; \
    peek p = t <$> peek p; \
    poke p (t a) = poke p a }

#define SIZEOF(ctype) (fudsizeof_/**/ctype)

#define IPTR(ctype) \
  new/**/ctype/**/Array n = HT(ctype) `fmap` malloc (n*SIZEOF(ctype)); \
  new/**/ctype = new/**/ctype/**/Array 1; \
  HASADDR(HT(ctype)); \
  instance IsPtr HT(ctype) where { \
    nullPtr = HT(ctype) nullAddr;\
    newPtr = new/**/ctype ;\
    newArray = new/**/ctype/**/Array}

-- freePtr (HT(ctype) addr) = free addr

#define H_STRUCTTYPE(ctype) \
  NEWTYPE(HT(ctype));IPTR(ctype);IDAR(HT(ctype))

#define H_ARRAY(ctype) H_STRUCTTYPE(ctype); ISTORE(HT(ctype)) ; type HTA(ctype) = HT(ctype)

#define C_STRUCTTYPE(ctype) \
  NEWTYPE(HT(ctype)); \
  HASADDR(HT(ctype)); \
  ISTORE(HT(ctype)); \
  INSTCCALL(HT(ctype))
