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
#ident	"$Header: xcreat.c,v 1.6.2.2 90/05/10 01:11:48 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"sys/types.h"
# include	"macros.h"

/*
	"Sensible" creat: write permission in directory is required in
	all cases, and created file is guaranteed to have specified mode
	and be owned by effective user.
	(It does this by first unlinking the file to be created.)
	Returns file descriptor on success,
	fatal() on failure.
*/

xcreat(name,mode)
char *name;
int mode;
{
	register int fd;
	register char *d;

#ifdef mips
	d = (char *)malloc(size(name));
#else
	d = (char *)alloca(size(name));
#endif mips
	copy(name,d);
	if (!exists(dname(d))) {
		sprintf(Error,"directory `%s' nonexistent (ut1)",d);
		fatal(Error);
	}
#ifdef mips
	free(d);
#endif mips
	unlink(name);
	if ((fd = creat(name,mode)) >= 0)
		return(fd);
	return(xmsg(name,"xcreat"));
}
