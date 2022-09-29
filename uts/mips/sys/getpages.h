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
/* $Header: getpages.h,v 1.8.1.2 90/05/10 06:20:56 wje Exp $ */

#ifndef	_SYS_GETPAGES_
#define	_SYS_GETPAGES_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifdef KERNEL
#include <sys/buf.h>
#endif KERNEL


/*	The following define sets the maximum size chunk which
 *	getpages can swap out at once.  The tuneable parameter
 *	tune.t_maxsc can never be made larger than this value.
 */

#define MAXSPGLST	100


/*
 *	Structure used for list entry in argument memfree()
 */

typedef struct {
	pfd_t	*pl_pfdat;	/* Ptr to pfdat entry */
	pde_t	*pl_pde;	/* ptr to pde, if any */
} pglst_t;

#ifdef KERNEL
struct	buf	*bclnlist;	/* head of cleaned page list */
extern	int	klout;		/* target kluster size for pageout */
extern	int	klin;		/* target kluster size for pagein */
#endif KERNEL

#endif	_SYS_GETPAGES_
