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
/* $Header: proc.h,v 1.7.1.2 90/05/10 04:53:50 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)proc.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * One structure allocated per active
 * process. It contains all data needed
 * about the process while the
 * process may be swapped out.
 * Other per process data (user.h)
 * is swapped with the process.
 */
#if defined(mips) && defined (LANGUAGE_C)
struct	bsd43_(proc) {
	struct	bsd43_(proc) *p_link;	/* linked list of running processes */
	struct	bsd43_(proc) *p_rlink;
	struct	bsd43_(proc) *p_nxt;	/* linked list of allocated proc slots */
	struct	bsd43_(proc) **p_prev;	/* also zombies, and free proc's */
	struct	bsd43_(pte) *p_addr;	/* u-area kernel map address */
#ifdef mips
	int	p_tlbpid;	/* tlb context of this proc */
	struct	bsd43_(tlbinfo) *p_tlbinfo;	/* per process tlb mappings */
	int	p_tlbindx;	/* index of next wired tlb entry to use */
#endif mips
	signed	char p_usrpri;	/* user-priority based on p_cpu and p_nice */
	signed	char p_pri;	/* priority, negative is high */
	char	p_cpu;		/* cpu usage for scheduling */
	char	p_stat;
	char	p_time;		/* resident time for scheduling */
	signed	char p_nice;	/* nice for cpu usage */
	char	p_slptime;	/* time since last block */
	char	p_cursig;
	int	p_sig;		/* signals pending to this process */
	int	p_sigmask;	/* current signal mask */
	int	p_sigignore;	/* signals being ignored */
	int	p_sigcatch;	/* signals being caught by user */
	int	p_flag;
	uid_t	p_uid;		/* user id, used to direct tty signals */
	short	p_pgrp;		/* name of process group leader */
	short	p_pid;		/* unique process id */
	short	p_ppid;		/* process id of parent */
	u_short	p_xstat;	/* Exit status for wait */
	struct	bsd43_(rusage) *p_ru;	/* mbuf holding exit information */
	short	p_poip;		/* page outs in progress */
#ifdef vax
	short	p_szpt;		/* copy of page table size */
#endif
#ifdef mips
	long	p_textstart;	/* starting address of user text */
	long	p_datastart;	/* starting address of user data */
	short	p_textpt;	/* copy of text  page table size */
	short	p_datapt;	/* copy of data  page table size */
	short	p_stakpt;	/* copy of stack page table size */
#endif
	bsd43_(size_t)	p_tsize;	/* copy size of text (clicks) */
	bsd43_(size_t)	p_dsize;	/* copy size of data space (clicks) */
	bsd43_(size_t)	p_ssize;	/* copy of stack size (clicks) */
	bsd43_(size_t) 	p_rssize; 	/* current resident set size in clicks */
	bsd43_(size_t)	p_maxrss;	/* copy of u.u_limit[MAXRSS] */
	bsd43_(size_t)	p_swrss;	/* resident set size before last swap */
	swblk_t	p_swaddr;	/* disk address of u area when swapped */
	caddr_t p_wchan;	/* event process is awaiting */
	struct	bsd43_(text) *p_textp;	/* pointer to text structure */
#ifdef vax
	struct	bsd43_(pte) *p_p0br;	/* page table base P0BR */
#endif
#ifdef mips
	struct	bsd43_(pte) *p_textbr;	/* text  page table base */
	struct	bsd43_(pte) *p_databr;	/* data  page table base */
	struct	bsd43_(pte) *p_stakbr;	/* stack page table base */
#endif
	struct	bsd43_(proc) *p_xlink;	/* linked list of procs sharing same text */
	short	p_cpticks;	/* ticks of cpu time */
#ifdef vax
	float	p_pctcpu;	/* %cpu for this process during p_time */
#endif
#ifdef mips
	int	p_pctcpu;	/* %cpu for this process during p_time */
	long	p_fp;		/* generate SIGFPE on all fp interrupts */
#endif
	short	p_ndx;		/* proc index for memall (because of vfork) */
	short	p_idhash;	/* hashed based on p_pid for kill+exit+... */
	struct	bsd43_(proc) *p_pptr;	/* pointer to process structure of parent */
	struct	bsd43_(proc) *p_cptr;	/* pointer to youngest living child */
	struct	bsd43_(proc) *p_osptr;	/* pointer to older sibling processes */
	struct	bsd43_(proc) *p_ysptr;	/* pointer to younger siblings */
	struct	bsd43_(itimerval) p_realtimer;
	struct	bsd43_(quota) *p_quota;	/* quotas for this process */
};
#endif mips && LANGUAGE_C

#define	BSD43_PIDHSZ		64
#define	BSD43_PIDHASH(pid)	((pid) & (BSD43_PIDHSZ - 1))

#if defined(mips) && defined (LANGUAGE_C)
#ifdef KERNEL
short	pidhash[BSD43_PIDHSZ];
struct	bsd43_(proc) *bsd43_(pfind)();
struct	bsd43_(proc) *bsd43_(proc), *bsd43_(procNPROC);	/* the proc table itself */
struct	bsd43_(proc) *bsd43_(freeproc), *bsd43_(zombproc), *bsd43_(allproc);
			/* lists of procs in various states */
int	bsd43_(nproc);

#ifdef vax
#define	BSD43_NQS	32		/* 32 run queues */
struct	bsd43_(prochd) {
	struct	bsd43_(proc) *bsd43_(ph_link);	/* linked list of running processes */
	struct	bsd43_(proc) *bsd43_(ph_rlink);
} qs[BSD43_NQS];
#endif vax

#ifdef mips
/*
 * Use single run queue. There is no ffs instruction and run queue's
 * are not typically very long.
 */
struct	bsd43_(prochd) {
	struct	bsd43_(proc) *bsd43_(ph_link);	/* linked list of running processes */
	struct	bsd43_(proc) *bsd43_(ph_rlink);
} qs;
#endif mips

int	bsd43_(whichqs);		/* bit mask summarizing non-empty qs's */

#endif KERNEL
#endif mips && LANGUAGE_C

/* stat codes */
#define	BSD43_SSLEEP	1		/* awaiting an event */
#define	BSD43_SWAIT	2		/* (abandoned state) */
#define	BSD43_SRUN	3		/* running */
#define	BSD43_SIDL	4		/* intermediate state in process creation */
#define	BSD43_SZOMB	5		/* intermediate state in process termination */
#define	BSD43_SSTOP	6		/* process being traced */

/* flag codes */
#define	BSD43_SLOAD	0x0000001	/* in core */
#define	BSD43_SSYS	0x0000002	/* swapper or pager process */
#define	BSD43_SLOCK	0x0000004	/* process being swapped out */
#define	BSD43_SSWAP	0x0000008	/* save area flag */
#define	BSD43_STRC	0x0000010	/* process is being traced */
#define	BSD43_SWTED	0x0000020	/* stpd proc not been waited for by parent */
#define	BSD43_SULOCK	0x0000040	/* user settable lock in core */
#define	BSD43_SPAGE	0x0000080	/* process in page wait state */
#define	BSD43_SKEEP	0x0000100	/* another flag to prevent swap out */
#define	BSD43_SOMASK	0x0000200	/* restore old mask after taking signal */
#define	BSD43_SWEXIT	0x0000400	/* working on exiting */
#define	BSD43_SPHYSIO	0x0000800	/* doing physical i/o (bio.c) */
#define	BSD43_SVFORK	0x0001000	/* process resulted from vfork() */
#define	BSD43_SVFDONE	0x0002000	/* another vfork flag */
#define	BSD43_SNOVM	0x0004000	/* no vm, parent in a vfork() */
#define	BSD43_SPAGI	0x0008000	/* init data space on demand, from inode */
#define	BSD43_SSEQL	0x0010000	/* user warned of sequential vm behavior */
#define	BSD43_SUANOM	0x0020000	/* user warned of random vm behavior */
#define	BSD43_STIMO	0x0040000	/* timing out during sleep */
/* was SDETACH */
#define	BSD43_SOUSIG	0x0100000	/* using old signal mechanism */
#define	BSD43_SOWEUPC	0x0200000	/* owe process an addupc() call at next ast */
#define	BSD43_SSEL	0x0400000	/* selecting; wakeup/waiting danger */
#define	BSD43_SLOGIN	0x0800000	/* a login process (legit child of init) */
#define	BSD43_SPTECHG	0x1000000	/* pte's for process have changed */
#define BSD43_SLKDONE 0x2000000       /* record-locking has been done */
#define BSD43_SFIXADE 0x4000000       /* fixup unalligned address errors */

/* flags for p_fp */
#define	BSD43_P_FP_SIGINTR1	1
#define	BSD43_P_FP_SIGINTR2	2

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define NQS BSD43_NQS
#   define PIDHASH BSD43_PIDHASH
#   define PIDHSZ BSD43_PIDHSZ
#   define P_FP_SIGINTR1 BSD43_P_FP_SIGINTR1
#   define P_FP_SIGINTR2 BSD43_P_FP_SIGINTR2
#   define SFIXADE BSD43_SFIXADE
#   define SIDL BSD43_SIDL
#   define SKEEP BSD43_SKEEP
#   define SLKDONE BSD43_SLKDONE
#   define SLOAD BSD43_SLOAD
#   define SLOCK BSD43_SLOCK
#   define SLOGIN BSD43_SLOGIN
#   define SNOVM BSD43_SNOVM
#   define SOMASK BSD43_SOMASK
#   define SOUSIG BSD43_SOUSIG
#   define SOWEUPC BSD43_SOWEUPC
#   define SPAGE BSD43_SPAGE
#   define SPAGI BSD43_SPAGI
#   define SPHYSIO BSD43_SPHYSIO
#   define SPTECHG BSD43_SPTECHG
#   define SRUN BSD43_SRUN
#   define SSEL BSD43_SSEL
#   define SSEQL BSD43_SSEQL
#   define SSLEEP BSD43_SSLEEP
#   define SSTOP BSD43_SSTOP
#   define SSWAP BSD43_SSWAP
#   define SSYS BSD43_SSYS
#   define STIMO BSD43_STIMO
#   define STRC BSD43_STRC
#   define SUANOM BSD43_SUANOM
#   define SULOCK BSD43_SULOCK
#   define SVFDONE BSD43_SVFDONE
#   define SVFORK BSD43_SVFORK
#   define SWAIT BSD43_SWAIT
#   define SWEXIT BSD43_SWEXIT
#   define SWTED BSD43_SWTED
#   define SZOMB BSD43_SZOMB
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


