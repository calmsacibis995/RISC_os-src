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
#ident	"$Header: tcflush.c,v 1.1.1.2 90/05/10 04:15:15 wje Exp $"

#include <termios.h>
#include <errno.h>

int
tcflush(fd, queue_selector)
	int fd, queue_selector;
{
	switch(queue_selector) {
	case TCIFLUSH:
		return(ioctl(fd, TCFLSH, 0));
	case TCOFLUSH:
		return(ioctl(fd, TCFLSH, 1));
	case TCIOFLUSH:
		return(ioctl(fd, TCFLSH, 2));
	default:
		errno = EINVAL;
		return(-1);
	}
}
