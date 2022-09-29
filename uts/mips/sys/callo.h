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
/* $Header: callo.h,v 1.9.1.2 90/05/10 06:07:12 wje Exp $ */

#ifndef	_SYS_CALLO_
#define	_SYS_CALLO_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/


/*
 * The callout structure is for a routine arranging
 * to be called by the clock interrupt
 * (clock.c) with a specified argument,
 * in a specified amount of time.
 * Used, for example, to time tab delays on typewriters.
 */

struct	callout {
	int	c_time;			/* incremental time */
	caddr_t	c_arg;			/* argument to routine */
	int	(*c_func)();		/* routine */
	struct	callout *c_next;	/* link to next entry */
	int	c_id;			/* ID for its removal */
	int	(*c_spl)();		/* spl routine (if any) */
};

#endif	_SYS_CALLO_
