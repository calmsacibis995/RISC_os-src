#ident "$Header: iobuf.h,v 1.1 90/03/29 15:06:49 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

#ifndef	_SYS_IOBUF_
#define	_SYS_IOBUF_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Each IDFC controller has an iobuf, which contains private state data
 * and 2 list heads: the b_forw/b_back list, which is doubly linked
 * and has all the buffers currently associated with that major
 * device; and the d_actf/d_actl list, which is private to the
 * controller but in fact is always used for the head and tail
 * of the I/O queue for the device.
 * Various routines in bio.c look at b_forw/b_back
 * (notice they are the same as in the buf structure)
 * but the rest is private to each device controller.
 */
typedef struct iobuf
{
	int	b_flags;		/* see buf.h */
	struct	buf *b_forw;		/* first buffer for this dev */
	struct	buf *b_back;		/* last buffer for this dev */
	struct	buf *b_actf;		/* head of I/O queue (b_forw)*/
	struct 	buf *b_actl;		/* tail of I/O queue (b_back)*/
	dev_t	b_dev;			/* major+minor device name */
	char	b_active;		/* busy flag */
	char	b_errcnt;		/* error count (for recovery) */
	int	jrqsleep;		/* process sleep counter on jrq full */
	struct eblock	*io_erec;	/* error record */
	int	io_nreg;		/* number of registers to log on errors */
	paddr_t	io_addr;		/* local bus address */
	physadr	io_mba;			/* mba address */
	struct	iostat	*io_stp;	/* unit I/O statistics */
	time_t	io_start;
	int	sgreq;			/* SYSGEN required flag */
	int	qcnt;			/* outstanding job request counter */
	int	io_s1;			/* space for drivers to leave things */
	int	io_s2;			/* space for drivers to leave things */
} iobuf_t;

#define tabinit(dv,stat) {0,0,0,0,0,makedev(dv,0),0,00,0,0,0,0,0,stat,0,0,0,0,0}
#define NDEVREG	(sizeof(struct device)/sizeof(int))

#define	B_ONCE	01	/* flag for once only driver operations */
#define	B_TIME	04	/* for timeout use */

#endif	_SYS_IOBUF_
