#include "structs.h"

#define SETI(ctype,fhtype,p,i,field,v) (seti_/**/ctype/**/_/**/field (p) (i) (v))
#define SINDEX(ctype,p,i,v) (sindex_/**/ctype (p) (i) (v))
#define SET(ctype,fhtype,p,field,v) (set_/**/ctype/**/_/**/field (p) (v))
#define GET(ctype,rhtype,p,field) (get_/**/ctype/**/_/**/field (p))
#define GETC(ctype,rctype,rhtype,p,field) GET(ctype,rhtype,p,field)
#define INDEX(ctype) index_/**/ctype
#define CINDEX(t) cindex_/**/t

#define SETWa(swa,field,v) SET(XSetWindowAttributes,Int,swa,field,v)
#define SETWaXID(swa,field,v) SET(XSetWindowAttributes,XID,swa,field,v)

#define AGET(ctype,rhtype,p,field) (aget_/**/ctype/**/_/**/field (p))

#define CCONST(c) const_/**/c
#define CWORD32(c) CCONST(c)
