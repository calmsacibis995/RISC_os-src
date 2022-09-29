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
/* $Header: vmmeter.h,v 1.6.3.3 90/05/10 05:00:24 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vmmeter.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Virtual memory related instrumentation
 */
struct bsd43_(vmmeter)
{
#define	bsd43_v_first	v_swtch
	unsigned v_swtch;	/* context switches */
#ifdef mips
	unsigned v_tlbpid;	/* calls to new_tlbpid */
#endif
	unsigned v_trap;	/* calls to trap */
	unsigned bsd43_(v_syscall);	/* calls to syscall() */
	unsigned v_intr;	/* device interrupts */
	unsigned v_soft;	/* software interrupts */
	unsigned v_pdma;	/* pseudo-dma interrupts */
	unsigned v_pswpin;	/* pages swapped in */
	unsigned v_pswpout;	/* pages swapped out */
	unsigned v_pgin;	/* pageins */
	unsigned v_pgout;	/* pageouts */
	unsigned v_pgpgin;	/* pages paged in */
	unsigned v_pgpgout;	/* pages paged out */
	unsigned v_intrans;	/* intransit blocking page faults */
	unsigned v_pgrec;	/* total page reclaims */
	unsigned v_xsfrec;	/* found in free list rather than on swapdev */
	unsigned v_xifrec;	/* found in free list rather than in filsys */
	unsigned v_exfod;	/* pages filled on demand from executables */
	unsigned v_zfod;	/* pages zero filled on demand */
	unsigned bsd43_(v_vrfod);	/* fills of pages mapped by vread() */
	unsigned v_nexfod;	/* number of exfod's created */
	unsigned v_nzfod;	/* number of zfod's created */
	unsigned v_nvrfod;	/* number of vrfod's created */
	unsigned v_pgfrec;	/* page reclaims from free list */
	unsigned v_faults;	/* total faults taken */
	unsigned v_scan;	/* scans in page out daemon */
	unsigned v_rev;		/* revolutions of the hand */
	unsigned v_seqfree;	/* pages taken from sequential programs */
	unsigned v_dfree;	/* pages freed by daemon */
	unsigned v_fastpgrec;	/* fast reclaims in locore */
#define	bsd43_v_last v_fastpgrec
	unsigned v_swpin;	/* swapins */
	unsigned v_swpout;	/* swapouts */
	unsigned v_zeros;	/* pages zero'ed in idle loop */
	unsigned v_zero_hits;	/* clearseg's saved by idle clearing */
};
#ifdef KERNEL
struct	bsd43_(vmmeter) cnt, rate, sum;
#endif

/* systemwide totals computed every five seconds */
struct bsd43_(vmtotal)
{
	short	t_rq;		/* length of the run queue */
	short	t_dw;		/* jobs in ``disk wait'' (neg priority) */
	short	t_pw;		/* jobs in page wait */
	short	t_sl;		/* jobs sleeping in core */
	short	t_sw;		/* swapped out runnable/short block jobs */
	long	t_vm;		/* total virtual memory */
	long	t_avm;		/* active virtual memory */
	long	t_rm;		/* total real memory in use */
	long	t_arm;		/* active real memory */
	long	t_vmtxt;	/* virtual memory used by text */
	long	t_avmtxt;	/* active virtual memory used by text */
	long	t_rmtxt;	/* real memory used by text */
	long	t_armtxt;	/* active real memory used by text */
	long	t_free;		/* free memory pages */
};
#ifdef KERNEL
struct	bsd43_(vmtotal) total;
#endif

/*
 * Optional instrumentation.
 */
#ifdef PGINPROF

#define	BSD43_NDMON	128
#define	BSD43_NSMON	128

#define	BSD43_DRES	20
#define	BSD43_SRES	5

#define	BSD43_PMONMIN	20
#define	BSD43_PRES	50
#define	BSD43_NPMON	64

#define	BSD43_RMONMIN	130
#define	BSD43_RRES	5
#define	BSD43_NRMON	64

/* data and stack size distribution counters */
unsigned int	dmon[BSD43_NDMON+1];
unsigned int	smon[BSD43_NSMON+1];

/* page in time distribution counters */
unsigned int	pmon[BSD43_NPMON+2];

/* reclaim time distribution counters */
unsigned int	rmon[BSD43_NRMON+2];

int	bsd43_(pmonmin);
int	bsd43_(pres);
int	bsd43_(rmonmin);
int	bsd43_(rres);

unsigned bsd43_(rectime);		/* accumulator for reclaim times */
unsigned bsd43_(pgintime);		/* accumulator for page in times */
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DRES BSD43_DRES
#   define NDMON BSD43_NDMON
#   define NPMON BSD43_NPMON
#   define NRMON BSD43_NRMON
#   define NSMON BSD43_NSMON
#   define PMONMIN BSD43_PMONMIN
#   define PRES BSD43_PRES
#   define RMONMIN BSD43_RMONMIN
#   define RRES BSD43_RRES
#   define SRES BSD43_SRES
#   define v_first bsd43_v_first
#   define v_last bsd43_v_last
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


