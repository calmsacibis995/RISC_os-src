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
/* $Header: ucred.h,v 1.2.1.2 90/05/10 06:43:28 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ucred.h 2.1 88/05/18 4.0NFSSRC SMI
*/
/*----- COPYRIGHT (END) -----------------------------------------------------*/

#ifndef _SYS_UCRED_
#define _SYS_UCRED_

/*
 * User credential structure
 */
struct ucred {
 	unsigned short	cr_ref;			/* reference count */
 	uid_t 	 	cr_uid;			/* effective user id */
 	gid_t  		cr_gid;			/* effective group id */
 	uid_t  		cr_ruid;		/* real user id */
 	gid_t		cr_rgid;		/* real group id */
 	gid_t  		cr_groups[NGROUPS];	/* groups, 0 terminated */
};

#ifdef KERNEL
#define	crhold(cr)	(cr)->cr_ref++
void crfree();
struct ucred *crget();
struct ucred *crcopy();
struct ucred *crdup();
struct ucred *crgetcred();
#endif KERNEL

#endif !_SYS_UCRED_
