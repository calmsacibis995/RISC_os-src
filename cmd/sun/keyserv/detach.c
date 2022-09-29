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
#ident	"$Header: detach.c,v 1.2.1.2 90/05/09 19:12:25 wje Exp $"

#ifndef lint
static char sccsid[] = 	"@(#)detach.c	1.2 88/05/19 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/*
 * Copyright (C) 1986, Sun Microsystems, Inc.
 */

#include <sys/ioctl.h>
#include <fcntl.h>

/*
 * detach from tty
 */
detachfromtty()
{
	int tt;

	close(0);
	close(1);
	close(2);
	switch (fork()) {
	case -1:
		perror("fork");
		break;
	case 0:
		break;
	default:
		exit(0);
	}
	tt = open("/dev/tty", O_RDWR);
	if (tt > 0) {
		ioctl(tt, TIOCNOTTY, 0);
		close(tt);
	}
	(void)open("/dev/null", O_RDWR, 0);
	dup(0);
	dup(0);
}
 

