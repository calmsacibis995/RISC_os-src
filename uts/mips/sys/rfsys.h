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
/* $Header: rfsys.h,v 1.6.4.2 90/05/10 06:34:46 wje Exp $ */

#ifndef	_SYS_RFSYS_
#define	_SYS_RFSYS_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#ifdef INKERNEL
extern	char	Domain[];
#endif

/*  opcodes for rfsys system call */

#define RF_FUMOUNT	1	/* forced unmount */
#define RF_SENDUMSG	2	/* send buffer to remote user-level */
#define RF_GETUMSG	3	/* wait for buffer from remote user-level */
#define RF_LASTUMSG	4	/* wakeup from GETUMSG */
#define RF_SETDNAME	5	/* set domain name */
#define RF_GETDNAME	6	/* get domain name */
#define RF_SETIDMAP	7
#define RF_FWFD		8
#define RF_VFLAG	9
#define RF_DISCONN	10	/* return value for link down */
#define RF_VERSION	11


/* defines for VFLAG option	*/
#define V_CLEAR 0
#define V_SET	1
#define V_GET	2

/* defines for the VERSION option	*/
#define VER_CHECK	1
#define VER_GET		2

#endif	_SYS_RFSYS_
