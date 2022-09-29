/* --------------------------------------------------- */
/* | Copyright (c) 1989 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: limits.h,v 1.9.1.4 90/05/09 14:59:01 wje Exp $ */

#ifndef	_POSIX_LIMITS_
#define	_POSIX_LIMITS_	1

/* <sysv/sys/limits.h> has duplicate definitions of everything in
 * <sysv/limits.h> so don't include the latter if the former has
 * already been included
 */
#ifndef	_SYS_LIMITS_
#include <sysv/limits.h>
#endif	/* _SYS_LIMITS_ */

/* Run-time increaseable values -- actual values provided by sysconf() */
#ifndef	NGROUPS_MAX
#define	NGROUPS_MAX	16
#endif	/* NGROUPS_MAX */

/* Run-time invariant values (possibly indeterminant) -- 
 * actual values provided by sysconf()
 */
/* There is the potential for these values listed below
 * to be configurable, so we will undefine them, forcing the
 * program to find out at run time.
 */
/* #ifndef	ARG_MAX	
 * #define	ARG_MAX		20480
 * #endif
 * 
 * #ifndef	CHILD_MAX
 * #define	CHILD_MAX 	100
 * #endif
 * 
 * #ifndef	OPEN_MAX 
 * #define	OPEN_MAX	100
 * #endif
 * 
 * #ifndef	STREAM_MAX 
 * #define	STREAM_MAX	100
 * #endif
 */

#ifndef	TZNAME_MAX 
#define	TZNAME_MAX	3
#endif
 
/* Pathname variable values -- values should be queried with pathconf() */
#ifndef	LINK_MAX
#define	LINK_MAX	1000
#endif

#ifndef	MAX_CANON
#define	MAX_CANON	256
#endif

/* this value is indeterminant because streams buffers are
 * dynamically allocated -- programs must use pathconf() to obtain a value.
 */
/* #ifndef	MAX_INPUT
 * #define	MAX_INPUT	256
 * #endif
 */ 
#ifndef	NAME_MAX
#define	NAME_MAX	255
#endif

#ifndef	PATH_MAX
#define	PATH_MAX	1023
#endif	

#ifndef	PIPE_BUF
#define	PIPE_BUF	5120
#endif

#ifndef	MB_LEN_MAX
#define MB_LEN_MAX	1
#endif

/* Invariant Values */
#define	SSIZE_MAX	2147483647

/* POSIX Minimum Values */
#define _POSIX_ARG_MAX          4096
#define _POSIX_CHILD_MAX        6
#define _POSIX_LINK_MAX         8
#define _POSIX_MAX_CANON        255
#define _POSIX_MAX_INPUT        255
#define _POSIX_NAME_MAX         14
#define _POSIX_NGROUPS_MAX      0
#define _POSIX_OPEN_MAX         16
#define _POSIX_PATH_MAX         255
#define _POSIX_PIPE_BUF         512
#define _POSIX_SSIZE_MAX        32767
#define _POSIX_STREAM_MAX	8
#define _POSIX_TZNAME_MAX	3

#endif	_POSIX_LIMITS_
