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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: prot_time.h,v 1.1.1.2.1.1.1.2 90/11/17 11:57:53 beacker Exp $ */

/* @(#)nfs.cmds:nfs/lockd/prot_time.h 1.1 */
/*
 * This file consists of all timeout definition used by rpc.lockd
 */

#define MAX_LM_TIMEOUT_COUNT	1
#define OLDMSG			30	/* counter to throw away old msg */
/*
#define LM_TIMEOUT_DEFAULT 	15
*/
#define LM_TIMEOUT_DEFAULT      45
/*
#define LM_GRACE_DEFAULT 	3
*/
#define LM_GRACE_DEFAULT       	1
int 	LM_TIMEOUT;
int 	LM_GRACE;
int	grace_period;
