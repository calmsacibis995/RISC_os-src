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
/* $Header: elog.h,v 1.6.4.2 90/05/10 06:11:50 wje Exp $ */

#ifndef	_SYS_ELOG_
#define	_SYS_ELOG_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * "True" major device numbers. These correspond
 * to standard positions in the configuration
 * table, but are used for error logging
 * purposes only.
 */

#define CNTL	1
#define SYS	2
#define CAC	3
#define PF	4

/*
 * IO statistics are kept for each physical unit of each
 * block device (within the driver). Primary purpose is
 * to establish a guesstimate of error rates during
 * error logging.
 */

struct iostat {
	long	io_ops;		/* number of read/writes */
	long	io_misc;	/* number of "other" operations */
	long	io_qcnt;	/* number of jobs assigned to drive */
	ushort io_unlog;	/* number of unlogged errors */
};

/*
 * structure for system accounting
 */
struct iotime {
	struct iostat ios;
	long	io_bcnt;	/* total blocks transferred */
	time_t	io_resp;	/* total block response time */
	time_t	io_act;		/* total drive active time (cumulative utilization) */
};
#define	io_cnt	ios.io_ops
#define io_qc ios.io_qcnt
/* drive utilization times can be calculated by system software as follows */

/* Average drive utilization = (io_cact/io_elapt) */
/* Average drive utilization for last interval = (io_liact/io_intv) */

#endif	_SYS_ELOG_
