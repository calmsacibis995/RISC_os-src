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
#ident	"$Header: tcsetpgrp.c,v 1.1.1.2 90/05/10 04:15:44 wje Exp $"

#include <sys/types.h>
#include <termios.h>

pid_t
tcsetpgrp(fd, pgrp)
	int fd;
	pid_t pgrp;
{
	if (ioctl(fd, TIOCSPGRP, &pgrp) < 0)
		return(-1);
	return(0);
}
