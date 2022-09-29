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
/* $Header: prfcntl.h,v 1.8.2.6 90/05/22 18:36:11 wje Exp $ */

#ifndef	_SYS_FS_PRFCNTL_
#define	_SYS_FS_PRFCNTL_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	prfcntl.h - fcntls for /proc */

#define PFC		('p'<<8)

#define PFCGETPR	(PFC|1)	/* read struct proc */
#define PFCOPENT	(PFC|2)	/* open text file for reading */
#define PFCEXCLU	(PFC|3)	/* mark text for exclusive use */

#define PFCSTOP		(PFC|4)	/* send STOP signal and... */
#define PFCWSTOP	(PFC|5)	/* wait for process to STOP */
#define PFCRUN		(PFC|6)	/* make process runnable */

#define PFCSMASK	(PFC|7)	/* set signal trace bit mask */
#define PFCCSIG		(PFC|8)	/* clear current signal */
#define PFCKILL		(PFC|9)	/* send signal */

#define PFCSEXEC	(PFC|10)	/* stop on exec */
#define PFCREXEC	(PFC|11)	/* run on exec */

#define PFCNICE		(PFC|12)	/* set nice priority */

#define PFCGMASK	(PFC|13)	/* get signal trace bit mask */

/* Not supported */
/*#define	PFCTERM		(PFC|14)	/* force proc to terminate */

#define	PFCSSTEP	(PFC|15)	/* single step proc, a la ptrace */

#define PFCGETREGS	(PFC|16)	/* read regs from uarea, see ptrace */
#define PFCPUTREGS	(PFC|17)	/* write regs to  uarea, see ptrace */

#define PFCGETSEG	(PFC|18)	/* get process segment info */
#define PFCGETUSEG	(PFC|19)	/* get process user area info */

struct pfcseg {
	char	*seg_vaddr;		/* segment virtual address */
	int	seg_size;		/* size in bytes */
	short	seg_regtype;		/* region type */
	short	seg_pregtype;		/* pregion type */
	int	seg_reserved;		/* reserved for now */
};

#endif	_SYS_FS_PRFCNTL_
