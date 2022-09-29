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
#ident	"$Header: cfsetispeed.c,v 1.3.1.3 90/05/10 04:12:41 wje Exp $"

#include <termios.h>
#include <errno.h>

int
cfsetispeed(tp, speed)
	struct termios *tp;
	speed_t		speed;
{
	/* invalid value for speed */
	if (speed < 0 || speed > EXTB) {
		errno = EINVAL;
		return(-1);
	}
	/* set input speed equal to output speed */
	if (speed == 0)
		speed = tp->c_cflag & CBAUD;

	/* set speed */
	tp->c_cflag &= ~CIBAUD;
	tp->c_cflag |= ((speed << CIBDSHFT) & CIBAUD);
	return(0);
}
