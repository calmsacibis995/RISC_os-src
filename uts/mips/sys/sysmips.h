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
/* $Header: sysmips.h,v 1.16.1.2.1.3.1.2 90/10/23 13:48:33 beacker Exp $ */

#ifndef	_SYS_SYSMIPS_
#define	_SYS_SYSMIPS_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifdef LANGUAGE_C
#include <sys/types.h>
#endif

/*
 * Sysmips() system call commands.
 */

#define SETNAME		1	/* rename the system */
#define STIME		2	/* set time */
#define FLUSH_CACHE	3	/* flush caches */
#define SMIPSSWPI	4	/* swap add, del, list funcs -- see swap.h */
#define	MIPS_FPSIGINTR	5	/* generate a SIGFPE on every fp interrupt */
#define	MIPS_FPU	6	/* turn on/off the fpu */
#define MIPS_FIXADE	7	/* fix address error (unaligned) */
#define MIPS_KEYLOCKED	8	/* check for locked key-switch. */
#define MIPS_PRIVILEGED_CHOWN 9 /* make chown privileged or not */
#define MIPS_RDNVRAM	10	/* read nvram */
#define MIPS_EXECMAP	11	/* map a file into memory like exec does */
#define MIPS_EXECUNMAP  12      /* unmap a file from memory */
#define MIPS_SBREAK     13      /* grow an aribitrary region */
#define MIPS_PAUSE	14	/* pause until awakened */
#define MIPS_WAKEUP	15	/* wakeup paused process */
#define MIPS_SWAP	16	/* swap/manipulate future syscall args */

#define MIPS_ATOMIC_SET 2001	/* atomic_set(&i,j) sets i to j and returns */
				/* j					    */
#define MIPS_UTLBHNDLR	2002	/* get/set utlbmiss handler as follows: */
#define	GET_UTLBHNDLR	0	/* - get current utlbmiss handler */
#define	UTLBHNDLR_3BUG	1	/* - install 3.0bug utlbmiss handler */
#define	UTLBHNDLR_3BUGP	2	/* - install 3.0bug profiling utlb handler */
#define	UTLBHNDLR_FAST	3	/* - install fast utlbmiss handler */
#define	UTLBHNDLR_FASTP	4	/* - install fast profiling utlbmiss handler */

#define MIPS_KMEMLOG	2003	/* kernel memory usage profiling */
#define KMEMLOG_GETP	0	/* - get kmemlog info (see kmemlog structure) */
#define KMEMLOG_SETP	1	/* - set kmemlog info (see kmemlog structure) */
#define KMEMLOG_EN	2	/* - enable kmemlog logging */
#define KMEMLOG_DIS	3	/* - disable kmemlog logging */
#define KMEMLOG_DATA	4	/* - get kmemlog data */
#define KMEMLOG_STAT	5	/* - get kmemlog state */

#ifdef	LANGUAGE_C
struct kmemlog {
	int	kmem_min;	/* minimum request size to log */
	int	kmem_incr;	/* Each bucket size */
	int	kmem_sz;	/* total number of entries */
}; 
#endif

#define MIPS_GET_MIB	2004	/* get kernel MIB values */
#define MIPS_GET_NEXT_MIB 2005	/* get kernel MIB values */

#define	MIPS_TEST_KMEM	2006	/* Test kernel memory allocator */

#define MIPS_DEVICE_ERROR 2007	/* Control injection of device errors */

#ifdef	LANGUAGE_C
struct device_error {
	u_int version;	/* for future enhancements */
	u_int index;	/* index into device error array */
	dev_t dev;	/* external major device number */
	long type;	/* types of operations which trigger this error */
	long number;	/* count of I/Os of specified type */
	long start;	/* IO when to start injecting errors */
	long count;	/* number of errors to inject this time */
	long repeat;	/* number of times to repeat injection */
	daddr_t base;	/* block number of start of error area */
	u_int extent;	/* extent of area which results in error injection */
	u_int ioctl;	/* Ioctl to match for an error */
	short error;	/* error to return */
	};
#endif

#define DEVICE_ERROR_VERSION_1	1

#define DEVICE_ERROR_ADD_ENTRY		0x1
#define DEVICE_ERROR_DELETE_ENTRY	0x2
#define DEVICE_ERROR_GET_ENTRY		0x4
#define DEVICE_ERROR_GET_NEXT_ENTRY	0x8

/* Legal values for the type field. */
#define DEVICE_READ_ERROR		0x1
#define DEVICE_WRITE_ERROR		0x2
#define DEVICE_OPEN_ERROR		0x4
#define DEVICE_CLOSE_ERROR		0x8
#define DEVICE_IOCTL_ERROR		0x10

/*
 * A note to kernel hackers:  To maintain binary compatibility please contact 
 * MIPS before adding new items here.
 */

#endif	_SYS_SYSMIPS_


