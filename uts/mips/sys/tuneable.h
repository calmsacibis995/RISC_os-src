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
/* $Header: tuneable.h,v 1.10.4.2 90/05/10 06:42:28 wje Exp $ */

#ifndef	_SYS_TUNEABLE_
#define	_SYS_TUNEABLE_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


typedef struct tune {
	int	t_gpgslo;	/* If freemem < t_getpgslow, then start	*/
				/* to steal pages from processes.	*/
	int	t_gpgshi;	/* Once we start to steal pages, don't	*/
				/* stop until freemem > t_getpgshi.	*/
	int	t_gpgslmsk;	/* Mask used by getpages to determine	*/
				/* whether a page is stealable.  The	*/
				/* page is stealable if the nr field is	*/
				/* greater than t_gpgsmsk.  Possible	*/
				/* values for this mask are:		*/
				/* >7	- steal any valid page.		*/
				/* 0-7	- steal page if not referenced	*/
				/*		  in (8 - mask) * 	*/
				/*		  t_vhandr seconds.	*/
	int	t_gpgshmsk;	/* If stealing pages at low mask	*/
				/* (gpgslmsk) didn't get enough, then	*/
				/* keep trying until high mask.		*/
				/* must be >= t_gpgslmsk		*/
	int	t_vhandr;	/* Run vhand once every t_vhandr seconds*/
				/* if freemem < t_vhandl.		*/
	int	t_vhandl;	/* Run vhand once every t_vhandr seconds*/
				/* if freemem < t_vhandl.		*/
	int	t_maxsc;	/* The maximum number of pages which	*/
				/* will be swapped out in a single	*/
				/* operation.  Cannot be larger than	*/
				/* MAXSPGLST in getpages.h.		*/
	int	t_maxfc;	/* The maximum number of pages which	*/
				/* will be saved up and freed at once.	*/
				/* Cannot be larger than MAXFPGLST in	*/
				/* getpages.h.				*/
	int	t_maxumem;	/* The maximum size of a user's virtual	*/
				/* address space in pages.		*/
	int	t_bdflushr;	/* The rate at which bdflush is run in	*/
				/* seconds.				*/
	int	t_minarmem;	/* The minimum available resident (not	*/
				/* swapable) memory to maintain in 	*/
				/* order to avoid deadlock.  In pages.	*/
	int	t_minasmem;	/* The minimum available swapable	*/
				/* memory to maintain in order to avoid	*/
				/* deadlock.  In pages.			*/
} tune_t;

extern tune_t	tune;

/*	The following is the default value for t_gpgsmsk.  It cannot be
 *	defined in /etc/master or /etc/system due to limitations of the
 *	config program.
 */

#define MAXGPGSLMSK	(PG_NDREF-1)	/* Max value for t_gpgslmsk */

#endif	_SYS_TUNEABLE_
