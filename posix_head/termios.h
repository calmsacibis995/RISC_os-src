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
/* $Header: termios.h,v 1.2.1.2 90/05/10 04:09:54 wje Exp $ */

#ifndef	_POSIX_TERMIOS_
#define	_POSIX_TERMIOS_	1

#include <sys/termios.h>

extern speed_t	cfgetospeed();
extern speed_t	cfgetispeed();
extern int	cfsetospeed();
extern int	cfsetispeed();
extern int	tcsetattr();
extern int	tcgetattr();
extern int	tcsendbreak();
extern int	tcdrain();
extern int	tcflush();
extern int	tcflow();

#endif	_POSIX_TERMIOS_
