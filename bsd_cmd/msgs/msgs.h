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
/* $Header: msgs.h,v 1.3.1.2 90/05/07 18:53:11 wje Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)msgs.h	5.1 (Berkeley) 6/4/85
 */

/*
 * Local definitions for msgs.
 */

#define USRMSGS	"/usr/msgs"		/* msgs directory */
#define MAIL	"/usr/bin/mailx -f %s"	/* could set destination also */
#define PAGE	"/bin/more -%d" 	/* crt screen paging program */
