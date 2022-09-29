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
/* $Header: termios.h,v 1.6.1.3 90/05/10 06:03:09 wje Exp $ */

#ifndef	_POSIX_SYS_TERMIOS_
#define	_POSIX_SYS_TERMIOS_	1

#ifndef	KERNEL
#include <sysv/sys/termio.h>
#endif	/* not KERNEL */

#define	NCCS		NCC

/* optional actions for tc{get,set}attr() */
#define TCSANOW         0
#define TCSADRAIN       1
#define TCSAFLUSH	2

/* options for tcflow() */
#define TCOOFF          0
#define TCOON           1
#define TCIOFF          2
#define TCION           3

/* queue selectors for tcflush() */
#define TCIFLUSH        0
#define TCOFLUSH        1
#define TCIOFLUSH       2

typedef unsigned long  	tcflag_t;
typedef unsigned char   cc_t;
typedef unsigned long  	speed_t;

struct 	termios {
	tcflag_t 	c_iflag;	/* input modes */
	tcflag_t 	c_oflag;	/* output modes */
	tcflag_t 	c_cflag;	/* control modes */
	tcflag_t 	c_lflag;	/* local modes */
	cc_t 		c_cc[NCCS];	/* control chars */
};

/*
 * control mode flags
 */
#define	CIBAUD		0x000f0000	/* input baud rate mask */
#define	CIBDSHFT	16		/* shift to get to CIBAUD field */

/* 
 * local mode flags (over and above those defined in <sysv/sys/termio.h>
 */
#define	IEXTEN	0000400

#endif	_POSIX_SYS_TERMIOS_
