#include "asyncinput.h"
#include <X11/extensions/Xdbe.h>

#ifndef BODY
#define BODY(b) ;
#endif
#include "cfundefs.h"
#include "ccalls.h"

#define CSIZE(ctype) int fudsizeof_##ctype(void) BODY({ return sizeof(ctype); })
#include "csizes.h"
