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
/* $Header: idp_var.h,v 1.5.4.2 90/05/10 04:32:25 wje Exp $ */

#ifndef	_BSD_NETNS_IDP_VAR_
#define	_BSD_NETNS_IDP_VAR_	1


/*
 * Copyright (c) 1984, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)idp_var.h	7.1 (Berkeley) 6/5/86
 */

/*
 * IDP Kernel Structures and Variables
 */
struct	idpstat {
	int	idps_badsum;		/* checksum bad */
	int	idps_tooshort;		/* packet too short */
	int	idps_toosmall;		/* not enough data */
	int	idps_badhlen;		/* ip header length < data size */
	int	idps_badlen;		/* ip length < ip header length */
};

#ifdef KERNEL
struct	idpstat	idpstat;
#endif

#endif	_BSD_NETNS_IDP_VAR_
