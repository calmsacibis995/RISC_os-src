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
/* $Header: nsaddr.h,v 1.7.3.2 90/05/10 01:02:05 wje Exp $ */

#ifndef	_NSADDR_
#define	_NSADDR_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	stoa - convert string to address

	atos - convert address to string

	header file
*/


#define	OCT	0	/* octal type */
#define	HEX	1	/* hexadecimal type */
#define	RAW	2	/* string type */
#define KEEP	8	/* keep protocol field	*/

struct address {
	char		*protocol;
	struct netbuf	addbuf;
};

struct netbuf	*stoa(/* str, netbuf */);
char		*atos(/* str, netbuf, type */);
struct address	*astoa(/* str, addr */);
char		*aatos(/* str, addr, type */);

#endif	_NSADDR_
