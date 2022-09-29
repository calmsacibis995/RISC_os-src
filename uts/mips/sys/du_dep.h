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
/* $Header: du_dep.h,v 1.6.4.2 90/05/10 06:11:22 wje Exp $ */

#ifndef	_SYS_DU_DEP_
#define	_SYS_DU_DEP_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * PORTS CIO "dumb" Definitions
 *
 ***** WARNING ***** WARNING ***** WARNING ***** WARNING
 *	This header file is shared by both sw and fw
 ***** WARNING ***** WARNING ***** WARNING ***** WARNING
 */

typedef long	RAPP;	/* FW request codes */
typedef long	CAPP;	/* FW completion codes */

#define CQSIZE		10		/* size one completion queue  */
#define RQSIZE		5		/* size all requeust queues   */
#define NUM_QUEUES	1		/* single request queue */

#define RAM_START	0
#define RAM_END		32768

#endif	_SYS_DU_DEP_
