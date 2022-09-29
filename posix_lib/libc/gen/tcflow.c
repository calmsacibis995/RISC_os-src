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
#ident	"$Header: tcflow.c,v 1.1.1.2 90/05/10 04:15:09 wje Exp $"

#include <termios.h>
#include <errno.h>

int
tcflow(fd, action)
	int fd, action;
{
	switch(action) {
	case TCOOFF:
		return(ioctl(fd, TCXONC, 0));
	case TCOON:
		return(ioctl(fd, TCXONC, 1));
	case TCIOFF:
		return(ioctl(fd, TCXONC, 2));
	case TCION:
		return(ioctl(fd, TCXONC, 3));
	default:
		errno = EINVAL;
		return(-1);
	}
}
