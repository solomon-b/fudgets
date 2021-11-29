#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#ifdef __MACH__
#include <sys/time.h>
#endif

/* For use in ghc-dialogue/AsyncInput.hs since
   calling FD_ZERO from _casm_ doesn't work with ghc on Linux-386. gcc
   compilains about a spilled register. /TH 2000-03-17

   Used for more functions after getting rid of _casm_/_ccall_ for GHC 6.2... 
   /TH 2004-03-25
*/

void fdzero(fd_set *s) { FD_ZERO(s); }
void fdset(int fd,fd_set *s) { FD_SET(fd,s); }
int fdisset(int fd,fd_set *s) { return FD_ISSET(fd,s); }
/* void bcopy_fdset(fd_set *src,fd_set *dst) { bcopy(src,dst,sizeof(fd_set)); } */
void bcopy_fdset(fd_set *src,fd_set *dst) { *dst=*src; }

int get_errno(void) { return errno; }
long get_stdin(void) { return (long)stdin; }
int get_fileno(long f) { return fileno((FILE *)f); }

void xDestroyImage(XImage *ximage) { XDestroyImage(ximage); }
void setXImage_data(XImage *ximage,char *data) { ximage->data=data; }

int default_bpp(Display *d,int depth) {
  int i,cnt,bpp;
  XPixmapFormatValues *ps=XListPixmapFormats(d,&cnt);
  bpp=depth; /* Hmm. Something is wrong if depth isn't found. */
  for(i=0;i<cnt;i++)
    if(ps[i].depth==depth) {
      bpp=ps[i].bits_per_pixel;
      break;
    }
  XFree(ps);
  /*fprintf(stderr,"default_bpp(%d,%d)=%d\n",(int)d,depth,bpp);*/
  return bpp;
}

void disable_timers(void) {
#ifdef __MACH__
  /* Disable timers set up by the GHC RTS. */
  struct itimerval zero;
  timerclear(&zero.it_interval);
  timerclear(&zero.it_value);
  setitimer(ITIMER_VIRTUAL,&zero,NULL);
#endif
}
