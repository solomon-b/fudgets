/*
 * (c) Copyright 1991 by Panagiotis Tsirigotis
 * Read the file COPYRIGHT for more details.
 * 
 */
/*
 * Internet support library.
 *
 * INTERFACE:
 *
 *		in_address			: form an internet address
 *		in_bind				: bind a socket to an internet address
 *		in_bind_opt			: bind a socket to an internet address and
 *								  set (for now, binary) options
 *		in_connect			: connect to an internet address
 *		in_connect_opt		: connect to an internet address and
 *								  set (for now, binary) options
 *
 */


#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
/* #include <stdarg.h> */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>    /* on some systems, errno is not a variable */
/* extern int errno ; */

#include <sys/param.h>
#ifdef MAXHOSTNAMELEN
#define HOST_NAME_LEN		MAXHOSTNAMELEN
#else
#define HOST_NAME_LEN		32
#endif

#include "defs.h"

#if defined(__NetBSD__) || defined(_HPUX_SOURCE)
extern int h_errno;
#endif

/*
 * $Header: /home/magnus/cvs/Fudgets/xlib/socketlib/inet.c,v 1.7 2013-07-28 12:09:13 hallgren Exp $
 */

#define PRIVATE				static

static char *version = VERSION ;

enum status_e { OK, ERROR } ;


/*
 * Close a file descriptor without destroying errno
 */
#define CLOSE( sd, t )				t = errno ;						\
											(void) close( sd ) ;			\
											errno = t
#ifndef FALSE
#define FALSE			0
#define TRUE			1
#endif


/*
 * Returns TRUE if the host argument has the form:
 *		<num>.<num>
 *		<num>.<num>.<num>
 *		<num>.<num>.<num>.<num>
 */
PRIVATE int is_number_address( host )
	char *host ;
{
	register int dot_count = 0 ;
	register char *p ;

	if ( host == NULL )
		return( FALSE ) ;
	
	for ( p = host ; *p ; p++ )
		if ( isascii( *p ) && isdigit( *p ) )		/* skip digits */
			continue ;
		else if ( *p == '.' )							/* count dots */
		{
			dot_count++ ;
			if ( dot_count > 3 )
				return( FALSE ) ;
		}
		else													/* reject everything else */
			return( FALSE ) ;
	return( dot_count > 0 ) ;
}



/*
 * Form an Internet address.
 * If host is NULL, the current host is assumed (but this does not
 * guarantee the use of the loopback address)
 *
 * Return value:
 *		0					: if successful
 *		RESOLVER_ERROR	: if an error occurs
 *
 * In case of error, errno is set to the value of h_errno, if the OS
 * supports h_errno; if not, the value of errno is undefined.
 */
int in_address( host, port, address )
	char *host ;
	short port ;
	struct sockaddr_in *address ;
{
	char host_name[ HOST_NAME_LEN ] ;
	char *the_host ;
	struct hostent *hp ;

	bzero( (char *) address, sizeof( *address ) ) ;

	/*
	 * Determine the host name
	 */
	if ( host == NULL )
	{
		(void) gethostname( host_name, HOST_NAME_LEN ) ;
		the_host = host_name ;
	}
	else
		the_host = host ;
	
	/*
	 * Get host address
	 */
	if ( is_number_address( host ) )
		address->sin_addr.s_addr = inet_addr( host ) ;
	else
	{

#ifndef HAS_H_ERRNO
		if ( ( hp = gethostbyname( the_host ) ) == NULL )
			return( RESOLVER_ERROR ) ;
#else
		int tries = 0 ;

		while ( ( hp = gethostbyname( the_host ) ) == NULL &&
				  (h_errno == TRY_AGAIN
#if __FreeBSD__ == 2
   /* Needed because of FreeBSD 2.2-961014-SNAP bug? */
				   || h_errno == NETDB_INTERNAL
#endif
				   ) &&
				  tries < MAX_TRIES )
			tries++ ;
		if ( hp == NULL )
		{
			errno = h_errno ;
			return( RESOLVER_ERROR ) ;
		}
#endif	/* HAS_H_ERRNO */

		bcopy( hp->h_addr, (char *) &address->sin_addr, hp->h_length ) ;
	}

	address->sin_family = AF_INET ;
	address->sin_port = htons( port ) ;

	return( 0 ) ;
}



/*
 * Bind an internet address. Port is the port that will be used; the
 * address is that of the machine the program runs on.
 * Type is the type of socket; this also defines the protocol
 * that will be used (i.e. for SOCK_STREAM use TCP etc)
 *
 * Return value:
 *		a file descriptor	:  if the operation is successful
 *		SYSCALL_ERROR 		: if there is a system call error
 *		RESOLVER_ERROR 	: if there is a resolver error
 * In case of error, errno contains a description of the error
 */
PRIVATE	int bind_address() ;

int in_bind( port, type )
	short port ;
	int type ;
{
	int sd ;

	if ( ( sd = socket( AF_INET, type, 0 ) ) == -1 )
		return( SYSCALL_ERROR ) ;
	
	return( bind_address( sd, port ) ) ;
}

#if 0
PRIVATE	va_list apply_options() ;
#endif

/*
 * Same as in_bind but also sets socket-level options.
 * The option list is a list of pairs (option, option_value) terminated
 * by NULL.
 */
#if 0
int in_bind_opt( port, type, va_alist )
	short port ;
	int type ;
	va_dcl
{
	int sd ;
	enum status_e status ;
	va_list ap ;
	
	if ( ( sd = socket( AF_INET, type, 0 ) ) == -1 )
		return( SYSCALL_ERROR ) ;
	
	va_start( ap ) ;
	ap = apply_options( sd, ap, &status );
	va_end( ap ) ;
	if ( status == ERROR )
		return( OPTION_ERROR ) ;

	return( bind_address( sd, port ) ) ;
}
#endif



/*
 * bind_address binds the address <INADDR_ANY,port> to a socket.
 * Return value:
 *		socket			: if successful
 *		SYSCALL_ERROR	: if bind fails
 *		RESOLVER_ERROR	: if it can't get the address of the local host
 */
PRIVATE int bind_address( sd, port )
	int sd ;
	short port ;
{
	struct sockaddr_in sin ;
	int errno_save ;

	sin.sin_family = AF_INET ;
	sin.sin_addr.s_addr = INADDR_ANY ;
	sin.sin_port = htons( port ) ;

	if ( bind( sd, (struct sockaddr *) &sin, sizeof( sin ) ) == -1 )
	{
		CLOSE( sd, errno_save ) ;
		return( SYSCALL_ERROR ) ;
	}
	return( sd ) ;
}


/*
 * Apply the options in the option_list.
 * Return value:
 *		The remaining part of the argument list
 *
 * The status argument shows if there has been an error.
 * In case of error, the socket is closed.
 */
#if 0
PRIVATE va_list apply_options( sd, option_list, status )
	int sd ;
	va_list option_list ;
	enum status_e *status ;
{
	register int option ;
	int errno_save ;

	/*
	 * Apply the options
	 */
	while ( ( option = va_arg( option_list, int ) ) != NULL )
	{
		int option_value ;

		/*
		 * For the moment accept only boolean options
		 */
		switch ( option )
		{
			case SO_DEBUG:
			case SO_REUSEADDR:
			case SO_KEEPALIVE:
			case SO_DONTROUTE:
			case SO_BROADCAST:
			case SO_OOBINLINE:
				option_value = va_arg( option_list, int ) ;
				if ( setsockopt( sd, SOL_SOCKET, option,
							&option_value, sizeof( option_value ) ) == -1 )
				{
					CLOSE( sd, errno_save ) ;
					*status = ERROR ;
					return( option_list ) ;
				}
				break ;

			default:
				break ;
		}
	}
	*status = OK ;
	return( option_list ) ;
}
#endif

PRIVATE	int connect_to_host() ;

/*
 * Connect to an internet address. The address is specified by the
 * host, port arguments. Type is the type of the socket which
 * defines the type of the protocol to be used.
 * If host is NULL, then the local host is implied.
 *
 * Return value:
 *		a file descriptor	:  if the operation is successful
 *		SYSCALL_ERROR 		: if there is a system call error
 *		RESOLVER_ERROR 	: if there is a resolver error
 * In case of error errno contains a description of the error
 */
int in_connect( host, port, type )
	char *host ;
	short port ;
	int type ;
{
	int sd ;

	if ( ( sd = socket( AF_INET, type, 0 ) ) == -1 )
		return( SYSCALL_ERROR ) ;
	
	return( connect_to_host( sd, host, port ) ) ;
}



/*
 * Same as in_connect but also sets socket-level options
 */
#if 0
int in_connect_opt( host, port, type, va_alist )
	char *host ;
	short port ;
	int type ;
	va_dcl
{
	int sd ;
	enum status_e status ;
	va_list ap ;
	int connect_to_host() ;
	va_list apply_options() ;

	if ( ( sd = socket( AF_INET, type, 0 ) ) == -1 )
		return( SYSCALL_ERROR ) ;
	
	va_start( ap ) ;
	ap = apply_options( sd, ap, &status ) ;
	va_end( ap ) ;
	if ( status == ERROR )
		return( OPTION_ERROR ) ;
	
	return( connect_to_host( sd, host, port ) ) ;
}
#endif

/*
 * connect_to_host tries to connect socket sd to the address <host,port>
 * Return value:
 *		socket				: if the operation is successful
 *		RESOLVER_ERROR		: if there is a resolver error
 *		SYSCALL_ERROR		: if connect(2) fails
 */
PRIVATE int connect_to_host( sd, host, port )
	int sd ;
	char *host ;
	short port ;
{
	struct sockaddr_in sin ;
	int errno_save ;

	if ( in_address( host, port, &sin ) == RESOLVER_ERROR )
	{
		CLOSE( sd, errno_save ) ;
		return( RESOLVER_ERROR ) ;
	}

	if ( connect( sd, (struct sockaddr *) &sin, sizeof( sin ) ) == -1 )
	{
		CLOSE( sd, errno_save ) ;
		return( SYSCALL_ERROR ) ;
	}
	return( sd ) ;
}

