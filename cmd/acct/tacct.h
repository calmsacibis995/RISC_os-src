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
/* $Header: tacct.h,v 1.1.2.2 90/05/09 15:10:32 wje Exp $ */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	total accounting (for acct period), also for day
 */

struct	tacct	{
	uid_t		ta_uid;		/* userid */
	char		ta_name[8];	/* login name */
	float		ta_cpu[2];	/* cum. cpu time, p/np (mins) */
	float		ta_kcore[2];	/* cum kcore-minutes, p/np */
	float		ta_con[2];	/* cum. connect time, p/np, mins */
	float		ta_du;		/* cum. disk usage */
	long		ta_pc;		/* count of processes */
	unsigned short	ta_sc;		/* count of login sessions */
	unsigned short	ta_dc;		/* count of disk samples */
	unsigned short	ta_fee;		/* fee for special services */
};
