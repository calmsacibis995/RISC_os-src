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
#ident	"$Header: pathname.c,v 1.1.2.2 90/05/10 03:10:11 wje Exp $"

/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 *
 *  Copyright (c) 1985 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 */

/*
 * There is no particular significance to the numbers assigned
 * to Test_port and Sock_port.  They're just random numbers greater
 * than then range reserved for privileged sockets.
 */

# ifdef DEBUG

char	*Driver =	"/va/conrad/games/src/hunt/hunt.driver.dbg";
# ifdef INTERNET
int	Test_port =	('h' << 8) | 't';
int	Sock_port =	('h' << 8) | 's';
# else INTERNET
char	*Sock_name =	"/tmp/hunt";
# endif INTERNET

# else DEBUG

char	*Driver =	"/usr/games/lib/hunt.driver";
# ifdef INTERNET
int	Test_port =	('h' << 8) | 't';
int	Sock_port =	('h' << 8) | 's';
# else INTERNET
char	*Sock_name =	"/tmp/hunt";
# endif INTERNET

# endif DEBUG
