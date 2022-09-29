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
/* $Header: boot.h,v 1.9.3.2 90/05/10 06:05:53 wje Exp $ */

#ifndef	_SYS_BOOT_
#define	_SYS_BOOT_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 * Restart block -- monitor support for "warm" starts
 *
 * prom will perform "warm start" if restart_blk is properly set-up:
 *	rb_magic == RESTART_MAGIC
 *	rb_occurred == 0
 *	rb_checksum == 2's complement, 32-bit sum of first 32, 32-bit words 
 */
#define	RESTART_MAGIC	0xfeedface
#define	RESTART_CSUMCNT	32		/* chksum 32 words of restart routine */
#define	RESTART_ADDR	0xa0000400	/* prom looks for restart block here */
#define	RB_BPADDR	(RESTART_ADDR+24)/* address of rb_bpaddr */

#ifdef LANGUAGE_C
struct restart_blk {
	int	rb_magic;		/* magic pattern */
	int	(*rb_restart)();	/* restart routine */
	int	rb_occurred;		/* to avoid loops on restart failure */
	int	rb_checksum;		/* checksum of 1st 32 wrds of restrt */
	char	*rb_fbss;		/* start of prom bss and stack area */
	char	*rb_ebss;		/* end of prom bss and stack area */
	/*
	 * These entries are for communication between the debug monitor
	 * and the client process being debugged
	 * NOTE: a return value of -1 from (*rb_vtop)() is distinguished
	 * to indicate that a translation could not be made.
	 */
	int	(*rb_bpaddr)();		/* breakpoint handler */
	int	(*rb_vtop)();		/* virtual to physical conversion rtn */
	/*
	 * config table goes here
	 */
};

/*
 * args to promexec -- monitor support for loading new programs
 *
 * bootfiles should be specified as DEV(UNIT)FILE
 * (e.g. bfs(0)bootmips_le)
 */
struct promexec_args {
	char	*pa_bootfile;		/* file to boot (only some devices) */
	int	pa_argc;		/* arg count */
	char	**pa_argv;		/* arg vector */
	char	**pa_environ;		/* environment vector */
	int	pa_flags;		/* flags, (see below) */
};
#endif /* LANGUAGE_C */

/*
 * promexec flags
 */
#define	EXEC_NOGO	1	/* just load, don't transfer control */

#endif	_SYS_BOOT_
