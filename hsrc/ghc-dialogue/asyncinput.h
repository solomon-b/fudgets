/*#ifndef ASYNCINPUT_H*/
/* This file seems to be incldued twice for some strange reason... */
#define ASYNCINPUT_H

#undef _POSIX_SOURCE

#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>

#include <sys/socket.h>
#define XLIB_ILLEGAL_ACCESS
#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef struct timeval timeVal;

typedef struct sockaddr sockAddr;

/* Things from socketlib, doesn't really belong here: */
int in_connect(char *host,short port,int type);
int in_bind(short port,	int type) ;
void fdzero(fd_set *s);
void fdset(int fd,fd_set *s);
int fdisset(int fd,fd_set *s);
void bcopy_fdset(fd_set *,fd_set *);
int get_errno(void);
long get_stdin(void);
int get_fileno(long); /* The true argument type is FILE * */

void xDestoryImage(XImage *ximage);
int default_bpp(Display *,int depth);
void setXImage_data(XImage *ximage,char *data);

void disable_timers(void);

/* Quick hack to work around typing problem: */
/* #define GETSOCKNAME(s,a,len) getsockname(s,(struct sockaddr *)a,len) */
/* #define ACCEPT(s,a,len) accept(s,(struct sockaddr *)a,len) */

/*#endif*/
