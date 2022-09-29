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
/* $Header: dnlc.h,v 1.2.1.2 90/05/10 06:11:10 wje Exp $ */


#ifndef _SYS_DNLC_
#define _SYS_DNLC_	1

/*	@(#)dnlc.h	2.1 88/05/18 4.0NFSSRC SMI;	*/
/*	@(#)dnlc.h 2.6 86/07/16 SMI	*/

/*
 * Copyright (c) 1984 Sun Microsystems Inc.
 */

/*
 * This structure describes the elements in the cache of recent
 * names looked up.
 */

#define	NC_NAMLEN	15	/* maximum name segment length we bother with*/

struct	ncache {
	struct	ncache	*hash_next, *hash_prev;	/* hash chain, MUST BE FIRST */
	struct 	ncache	*lru_next, *lru_prev;	/* LRU chain */
	struct	vnode	*vp;			/* vnode the name refers to */
	struct	vnode	*dp;			/* vno of parent of name */
	char		namlen;			/* length of name */
	char		name[NC_NAMLEN];	/* segment name */
	struct	ucred	*cred;			/* credentials */
};

#define	ANYCRED	((struct ucred *) -1)
#define	NOCRED	((struct ucred *) 0)

int	ncsize;
struct	ncache *ncache;

#endif	_SYS_DNLC_
