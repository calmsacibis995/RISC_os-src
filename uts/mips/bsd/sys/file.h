/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: file.h,v 1.13.4.2 90/05/10 04:36:34 wje Exp $ */

#ifndef	_BSD_SYS_FILE_
#define	_BSD_SYS_FILE_	1


/*
|| BSD compatibility header
||
|| Under the SGI scheme, these things live in <sys/fcntl.h>,
|| but in the BSD scheme of things <sys/file.h> is the place.
*/

#ifdef mips
#ifdef KERNEL
#include "bsd/sys/types.h"
#include "../../sys/file.h"
#include "sys/fcntl.h"
#else
#include <bsd/sys/types.h>
#include "../../sys/file.h"
#include <sys/fcntl.h>
#endif

#else /* mips */
#include </usr/include/sys/types.h>
#include </usr/include/sys/file.h>
#include <sys/fcntl.h>		/* can be relative since BSD doesn't have one */
#endif /* mips */

/*
 * Access call.
 */
#define	F_OK		0	/* does file exist */
#define	X_OK		1	/* is it executable by caller */
#define	W_OK		2	/* writable by caller */
#define	R_OK		4	/* readable by caller */

/*
 * Lseek call.
 */
#define	L_SET		0	/* absolute offset */
#define	L_INCR		1	/* relative to current offset */
#define	L_XTND		2	/* relative to end of file */

#endif	_BSD_SYS_FILE_
