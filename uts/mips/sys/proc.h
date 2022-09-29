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
/* $Header: proc.h,v 1.22.1.5.1.1.1.3 90/11/20 17:18:08 beacker Exp $ */

#ifndef	_SYS_PROC_
#define	_SYS_PROC_	1

#include <bsd/sys/time.h>
#include <bsd43/sys/time.h>
#include <bsd43/sys/resource.h>


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	One structure allocated per active process. It contains all
**	data needed about the process while the process may be swapped
**	out.  Other per process data (user.h) may swapped with the
**	process but in fact it is not.
*/

typedef struct	proc	{
	char	p_stat;			/* Status of process.	     */
	char	p_pri;			/* priority, negative is high */
	char	p_cpu;			/* cpu usage for scheduling */
	char	p_nice;			/* nice for cpu usage */
	uint	p_flag;			/* Flags defined below.	     */
	ushort	p_uid;			/* real user id */
	ushort	p_suid;			/* saved (effective) user id */
					/* from exec.		     */
	short	p_pgrp;			/* name of process group     */
					/* leader		     */
	short	p_pid;			/* unique process id */
	short	p_ppid;			/* process id of parent */
	ushort	p_sgid;			/* saved (effective) group   */
					/* id from exec.	     */
	int	p_sig;			/* signals pending to this   */
					/* process		     */
	struct	proc	*p_flink;	/* linked list of processes */
	struct	proc	*p_blink;	/* linked list of processes */
	caddr_t	p_wchan;		/* Wait addr for sleeping   */
					/* processes.		    */
	struct	proc	*p_parent;	/* ptr to parent process    */
	struct	proc	*p_child;	/* ptr to first child process */
	struct	proc	*p_sibling;	/* ptr to next sibling	    */
					/* process on chain	    */

	int	p_mpgneed;		/* number of memory pages    */
					/* needed in memwant.	     */
	uint	p_size;			/* size of swappable image  */
					/* in pages.		    */
	time_t	p_utime;		/* user time, this proc */
	time_t	p_stime;		/* system time, this proc */

	pde_t	p_ubptbl[NWIREDENTRIES];/* Page table for the u-block, and */
					/* other regions to avoid double */
					/* misses */
	caddr_t p_tlbhi_tbl[NWIREDENTRIES]; /* virtual addresses to go with */
					/* above page table		    */
	int	p_nexttlb;		/* index for next available entry in
					/* above tables */

	struct  proc *p_mlink;		/* link list of processes    */
					/* sleeping on memwant or    */
					/* swapwant.	  	     */
	preg_t	*p_region;		/* process regions */
	int	p_tlbpid;		/* unique tlbpid */
	short	p_jcpgrp;		/* job control process group */
	char	p_time;			/* resident time for scheduling */
	char	p_cursig;		/* signal currently being processed */
	short	p_epid;			/* effective pid             */
					/* normally - same as p_pid  */
					/* if server - p_pid that sent msg */
	sysid_t p_sysid;		/* normally - same as sysid */
					/* if server - system that sent msg */
	struct	rcvd  *p_minwd;		/* server msg arrived on this queue */
	struct	proc  *p_rlink;		/* linked list for server */
	int	p_trlock;
	struct vnode *p_trace;		/* pointer to /proc vnode */
	long	p_sigmask;		/* tracing signal mask for /proc */
	int	p_hold;			/* hold signal bit mask */
	int	p_chold;		/* defer signal bit mask */
					/* sigset turns on this bit */
					/* signal does not turn on this bit */
	long	p_fp;			/* generate SIGFPE on fp interrupts */
	short	p_xstat;		/* exit status for wait */

	struct itimerval p_realtimer;

	int	p_sigignore;		/* signals being ignored */
	int	p_sigcatch;		/* signals being caught by user */

	struct	bsd43_rusage *p_ru;	/* pointer to bsd43_rusage block */

	struct	vnode *p_ttyvp;		/* pointer to (iuse'd) vnode for */

	size_t	p_rssize;		/* current resident set size in pages */
	size_t	p_maxrss;	/* copy of u.u_limit[MAXRSS] */
	size_t	p_swrss;	/* resident set size before last swap */
	int	p_pctcpu;		/* %cpu for this process during p_time */
	pde_t	p_tpde;			/* temporary pde		*/
	short	p_cpticks;		/* ticks of cpu time */
	char	p_slptime;		/* time since last block */
	char	p_pause_cnt;		/* used by sysmips(MIPS_PAUSE) */
	int	p_sig_flag;		/* partial copy of u_sig_flag */
					/* currently only for bsd43 signals */
	struct file *p_ttyfp;		/* controlling terminal file pointer */
	int p_filler[7];		/* Filler -- I hate recompiling ps. */

} proc_t;

#define	p_link	p_flink

extern struct proc proc[];		/* the proc table itself */

/* stat codes */

#define	SSLEEP	1		/* Awaiting an event.		*/
#define	SRUN	2		/* Running.			*/
#define	SZOMB	3		/* Process terminated but not	*/
				/* waited for.			*/
#define	SSTOP	4		/* Process stopped by signal	*/
				/* since it is being traced by	*/
				/* its parent.			*/
#define	SIDL	5		/* Intermediate state in	*/
				/* process creation.		*/
#define	SONPROC	6		/* Process is being run on a	*/
				/* processor.			*/
#define SXBRK	7		/* process being xswapped       */

/* flag codes */

#define	SSYS	0x0001		/* System (resident) process.	*/
#define	STRC	0x0002		/* Process is being traced.	*/
#define	SWTED	0x0004		/* Stopped process has been	*/
				/* given to parent by wait	*/
				/* system call.  Don't return	*/
				/* this process to parent again	*/
				/* until it runs first.		*/
#define SNWAKE	0x0008		/* Process cannot wakeup by	*/
				/* a signal.			*/
#define SLOAD	0x0010		/* in core                      */
#define SLOCK   0x0020		/* Process cannot be swapped.	*/
#define	SRSIG	0x0040		/* Set when signal goes remote	*/
#define SPOLL	0x0080		/* Process in stream poll	*/
#define SSEL	SPOLL		/* 4.3 select(1) hack		*/
#define SPRSTOP	0x0100		/* process is being stopped via /proc */
#define SPROCTR	0x0200		/* signal tracing via /proc */
#define SPROCIO	0x0400		/* doing I/O via /proc, so don't swap */
#define SSEXEC	0x0800		/* stop on exec */
#define SPROPEN	0x1000		/* process is open via /proc */
#define SLKDONE 0x2000		/* record-locking has been done */
#define SNOCLDSTOP 0x4000	/* don't send SIGCLD when child stops */
#define SEXECED	0x8000		/* process has exec'ed after fork	   */
#define	SSEQL	0x0010000	/* user warned of sequential vm behavior */
#define	SUANOM	0x0020000	/* user warned of random vm behavior */
#define	SWEXIT	0x0040000	/* working on exiting */
#define	SVFORK	0x0080000	/* process resulted from vfork() */
#define	SOWEUPC	0x0200000	/* owe process an addupc() */
#define SFIXADE 0x4000000       /* fixup unalligned address errors */

#define PTRACED(p)	((p)->p_flag&(STRC|SPROCTR|SSEXEC|SPROPEN))

#define ubptbl(PP) ((PP)->p_ubptbl)

/* flags for p_fp */
#define	P_FP_SIGINTR1	1
#define	P_FP_SIGINTR2	2

#endif	_SYS_PROC_
