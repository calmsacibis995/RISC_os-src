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
/* $Header: map.h,v 1.7.4.2 90/05/10 06:27:56 wje Exp $ */

#ifndef	_SYS_MAP_
#define	_SYS_MAP_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *		struct map	X[]	.m_size		.m_addr
 *				---	------------	-----------
 *				[0]	mapsize(X)	mapwant(X)
 *					# X[] unused	sleep value
 *
 *		  mapstart(X)->	[1]	# units		unit number
 *				 :	    :		  :
 *				[ ]	    0
 */

struct map
{
	unsigned long m_size;	/* number of units available */
	unsigned long m_addr;	/* address of first available unit */
};

extern struct map sptmap[];	/* Map for system virtual space.   */

#define	mapstart(X)	&X[1]		/* start of map array */
#define	mapwant(X)	X[0].m_addr
#define	mapsize(X)	X[0].m_size	/* number of empty slots \
					   remaining in map array */
#define	mapdata(X) {(X)-2, 0} , {0, 0}
#define	mapinit(X, Y)	X[0].m_size = (Y)-2
#define	mapdefine(mapsize, nfree, start) \
	{ (mapsize)-2, 0 } , { (nfree), (start) }, { 0, 0 }

uint malloc();

#endif	_SYS_MAP_
