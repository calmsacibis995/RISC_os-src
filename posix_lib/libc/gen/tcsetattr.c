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
#ident	"$Header: tcsetattr.c,v 1.1.1.2 90/05/10 04:15:38 wje Exp $"

#include <termios.h>
#include <errno.h>

int
tcsetattr(fd, action, tp)
	int fd, action;
	struct termios *tp;
{
	switch(action) {
	case TCSANOW:
		return(ioctl(fd, TCSETA, tp));
	case TCSADRAIN:
		return(ioctl(fd, TCSETAW, tp));
	case TCSAFLUSH:
		return(ioctl(fd, TCSETAF, tp));
	default:
		errno = EINVAL;
		return(-1);
	}
}
