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
/* $Header: wait.h,v 1.1.4.2 90/05/10 06:45:13 wje Exp $ */

#ifndef	_SYS_WAIT_
#define	_SYS_WAIT_	1

#define WNOHANG		0x01	/* Don't hang in wait. */
#define WUNTRACED	0x02	/* Also report stopped (untraced) children. */

#endif	_SYS_WAIT_
