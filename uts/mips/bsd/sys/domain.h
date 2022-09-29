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
/* $Header: domain.h,v 1.5.4.2 90/05/10 04:36:28 wje Exp $ */

#ifndef	_BSD_SYS_DOMAIN_
#define	_BSD_SYS_DOMAIN_	1


/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)domain.h	7.1 (Berkeley) 6/4/86
 */

/*
 * Structure per communications domain.
 */
struct	domain {
	int	dom_family;		/* AF_xxx */
	char	*dom_name;
	int	(*dom_init)();		/* initialize domain data structures */
	int	(*dom_externalize)();	/* externalize access rights */
	int	(*dom_dispose)();	/* dispose of internalized rights */
	struct	protosw *dom_protosw, *dom_protoswNPROTOSW;
	struct	domain *dom_next;
};

#ifdef KERNEL
struct	domain *domains;
#endif

#endif	_BSD_SYS_DOMAIN_
