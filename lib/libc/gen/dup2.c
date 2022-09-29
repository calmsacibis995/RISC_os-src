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
#ident	"$Header: dup2.c,v 1.8.2.2 90/05/10 01:30:32 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifdef RISCOS
#include 	<bsd43/sys/syscall.h>
#define getdtablesize() ((int) syscall(BSD43_SYS_getdtablesize))
#define OPEN_MAX getdtablesize()
#else RISCOS
#include	<sys/limits.h>	/* Get definition for OPEN_MAX */
#endif RISCOS
#include	<fcntl.h>
#include	<sys/errno.h>	/* Get definition for EBADF */

int
dup2(fildes, fildes2)
int	fildes,		/* file descriptor to be duplicated */
	fildes2;	/* desired file descriptor */
{
	int	tmperrno;	/* local work area */
	extern	int	errno;	/* system error indicator */

	/* Be sure fildes is valid and open */
	if (fcntl(fildes, F_GETFL, 0) == -1) {
		errno = EBADF;
		return (-1);
	}

	/* Be sure fildes2 is in valid range */
	if (fildes2 < 0 || fildes2 >= OPEN_MAX) {
		errno = EBADF;
		return (-1);
	}

	/* Check if file descriptors are equal */
	if (fildes == fildes2) {
		/* open and equal so no dup necessary */
		return (fildes2);
	}

	/* Close in case it was open for another file */
	/* Must save and restore errno in case file was not open */
	tmperrno = errno;
	close(fildes2);
	errno = tmperrno;

	/* Do the dup */
	return (fcntl(fildes, F_DUPFD, fildes2));
}

