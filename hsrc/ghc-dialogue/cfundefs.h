
#define SETI(ctype,fhtype,p1,i1,field,v1) \
  void seti_##ctype##_##field(ctype p[],int i,typeof(p[i].field) v) \
    BODY({ p[i].field=v; })

#define SINDEX(ctype,p1,i1,v1) \
  void sindex_##ctype(ctype p[],int i,typeof(p[i]) v) \
    BODY({ p[i]=v; })

/*(_casm_ ``((ctype *)%0)[%1]=%2;'' (p) (i) (v) :: IO ())*/

#define SET(ctype,fhtype,p1,field,v1) \
  void set_##ctype##_##field(ctype *p,typeof(p->field) v) \
    BODY({ p->field=v; })

#define SETWa(swa,field,v) SET(XSetWindowAttributes,Int,swa,field,v)
#define SETWaXID(swa,field,v) SET(XSetWindowAttributes,XID,swa,field,v)

/* (_casm_ ``((ctype *)%0)->field=%1;'' (addrOf (p)) (v) :: IO ())*/

#define GET(ctype,rhtype,p1,field) \
  typeof(((ctype *)0)->field) get_##ctype##_##field(ctype *p) \
    BODY({ return p->field; })

#define GETC(ctype,crtype,rhtype,p1,field) \
  crtype get_##ctype##_##field(ctype *p) \
    BODY({ return p->field; })

/* (_casm_ `` %r = ((ctype *)%0)->field; '' (addrOf (p))) */

#define AGET(ctype,rhtype,p1,field) \
  typeof(&(((ctype *)0)->field)) aget_##ctype##_##field(ctype *p) \
    BODY({ return &(((ctype *)p)->field); })

/* (_casm_ `` %r = &(((ctype *)%0)->field); '' ((p)::HT(ctype))) */

#define INDEX(ctype) \
  ctype *index_##ctype(ctype *p,int i) \
    BODY({ return p+i; })

/* (\pP iI -> _casm_ ``%r = (ctype *)%0 + (int)%1;'' ((pP) :: HT(ctype)) iI) */

#define CINDEX(t) \
  t cindex_##t(t p[],int i) \
    BODY({ return p[i]; })

/* (\pP iI -> _casm_ ``%r = ((t *)%0)[(int)%1];'' pP iI) */

#define CCONST(c)  int const_##c(void) BODY({ return (c);})
#define CWORD32(c) int const_##c(void) BODY({ return (c);})
