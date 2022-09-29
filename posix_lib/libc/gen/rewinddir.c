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
#ident	"$Header: rewinddir.c,v 1.2.1.2 90/05/10 04:13:49 wje Exp $"

/*
	rewinddir -- C library extension routine

*/

#include	<sys/types.h>
#include	<dirent.h>

#undef	rewinddir

void
rewinddir(dirp)
register DIR	*dirp;		/* stream from opendir() */
{
	(void) seekdir(dirp, 0);
	return;
}
