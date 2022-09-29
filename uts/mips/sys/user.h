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
/* $Header: user.h,v 1.21.1.8.1.2.1.2 90/11/02 17:50:18 beacker Exp $ */

#ifndef	_SYS_USER_
#define	_SYS_USER_	1

#include <sys/param.h>
#include <sys/ucred.h>
#ifdef SYSTYPE_SYSV
#include <sys/reg.h>
#include <sys/systm.h>
#include <sys/pcb.h>
#include <sys/signal.h>
#else
#include <sysv/sys/reg.h>
#endif

#ifdef SYSTYPE_BSD43
#define	MAXSIG	BSD43_NSIG
#else
#include <bsd/sys/time.h>
#endif
#include <bsd43/sys/time.h>
#include <bsd43/sys/resource.h>
#include <bsd43/sys/signal.h>


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 * The user structure.
 * One allocated per process.
 * Contains all per process data that doesn't need to be referenced
 * while the process is swapped.
 * The user block is USIZE*click bytes long; resides at virtual kernel
 * 0xc0000000(3B2);
 * 0xffffc000(mips);
 * contains the system stack per user for exec system call only;
 * is cross referenced with the proc structure for the same process.
 *
 * XXX - check fillers, what are u_priptrsv and u_prisv? 
 */
 
#define	PSARGSZ		80	/* Space in u-block for exec arguments.	*/
				/* Used by ps command.			*/
#define	PSCOMSIZ	80	/* Space in u-block for program being execed */

struct u_bsd43_extension {
	int	ue_bsd43_sigmask[MAXSIG]; /* signals to be blocked */
	int	ue_bsd43_sigonstack;	/* signals to take on sigstack */
	int	ue_bsd43_sigintr;	/* signals that interrupt syscalls */
	int	ue_bsd43_oldmask;	/* saved mask from before sigpause */
	struct	bsd43_(sigstack) ue_bsd43_sigstack;
					/* sp & on stack state variable */
	int	ue_bsd43_sig_flag;	/* miscellaneous signal flags */
	struct	timeval	ue_bsd43_start;	/* start time 			*/
	struct 	bsd43_(rusage) ue_bsd43_ru; /* process resource usage	*/
	struct	bsd43_(rusage) ue_bsd43_cru; /* children's resource usage */
	struct	bsd43_(rlimit) ue_bsd43_rlimit[BSD43_RLIM_NLIMITS];
	};

#define BSD43_U_SF_OMASK 0x0001			/* restore u_oldmask	*/
#define BSD43_U_SF_BSD43_SIGCONTEXT 0x0002 	/* use bsd43_sigcontext */
#define BSD43_U_SF_SYSV_SIGCONTEXT 0x0004 	/* use sigcontext 	*/
#define BSD43_U_SF_POSIX_SIGCONTEXT 0x0008 	/* use posix_sigcontext */
#define BSD43_U_SF_SIGCONTEXT_SELECT (BSD43_U_SF_BSD43_SIGCONTEXT | \
				      BSD43_U_SF_POSIX_SIGCONTEXT | \
				      BSD43_U_SF_SYSV_SIGCONTEXT)
#define BSD43_U_SF_SIGCONTEXT_NOT_SET 0


typedef	struct	user
{
        /* pcb area must be first!! */
	pcb_t	u_pcb;		/* pcb, save area when switching */
	int	u_filler_7[1];

	int 	(*u_sigtramp)();/* address of user's signal return */
				/* handler: _sigtramp in libc	*/
	int	u_code;		/* ``code'' to trap */
	int	u_trapcause;	/* cause for SIGTRAP */
	int	u_trapinfo;	/* extra info concerning SIGTRAP */
	int	u_nshmseg;	/* Nbr of shared memory		*/
				/* currently attached to the process */

	int	u_filler_5[4];
	int 	u_syscall;	/* system call number */

	ulong	u_rambo_ticks[6]; /* num rambo ticks used by user,system */
	                          /* & kernel processes, 64-bits/each */
	int	u_filler_6[1];
	
	char	*u_tracepc;	/* Return PC if tracing enabled */

	void	(*u_signal[MAXSIG])();	/* disposition of signals */

	int	u_filler_8[2];

	label_t	u_qsav;		/* label variable for quits and	interrupts */

	char	u_segflg;	/* IO flag: 0:user D; 1:system;	*/
				/*          2:user I		*/
	char	u_ksegflg;	/* copyin/copyout flag:		*/
				/* 1 = access to kernel allowed */

	uint	u_error;	/* return error code */

	int	u_filler_4[2];

	struct proc *u_procp;	/* pointer to proc structure */

	int	*u_ap;		/* pointer to arglist */

	union {			/* syscall return values */
		struct	{
			int	R_val1;
			int	R_val2;
/*
 * Defining the fields this way lets BSD-origin programs redefine
 * r_val1 to be r_reg.R_val1, if they wish. Ok, so it's a disgusting hack.
 */
#define r_val1	R_val1
#define r_val2	R_val2
		}r_reg;

		off_t	r_off;
		time_t	r_time;
	} u_r;

	caddr_t	u_base;		/* base address for IO */
	unsigned u_count;	/* bytes remaining for IO */
	off_t	u_offset;	/* offset in file for IO */
	short	u_fmode;	/* file mode for IO */

	ushort	u_filler_2[1];
	int	u_filler_3[3];

	short	u_errcnt;	/* syscall error count */
	struct vnode *u_cdir;	/* current directory */
	struct vnode *u_rdir;	/* root directory */
	int	u_filler_1[8];

	int	*u_stack;	/* Ptr to start of kernel stack. */
	char	*u_pofile;	/* Ptr to open file flag array.	 */

	int	u_arg[6];	/* arguments to current system call */

#ifdef	SYSTYPE_BSD43
	size_t	u_tsize;	/* text size (clicks) */
	size_t	u_dsize;	/* data size (clicks) */
	size_t	u_ssize;	/* stack size (clicks) */
#else
	unsigned u_tsize;	/* text size (clicks) */
	unsigned u_dsize;	/* data size (clicks) */
	unsigned u_ssize;	/* stack size (clicks) */
#endif

	time_t	u_utime;	/* this process user time */
	time_t	u_stime;	/* this process system time */
	time_t	u_cutime;	/* sum of childs' utimes */
	time_t	u_cstime;	/* sum of childs' stimes */

	int	*u_ar0;		/* address of users saved R0 */

/*
 * The offsets of these elements must be reflected in ttrap.s and misc.s
 */
	struct {			/* profile arguments */
		short	*pr_base;	/* buffer base */
		unsigned pr_size;	/* buffer size */
		unsigned pr_off;	/* pc offset */
		unsigned pr_scale;	/* pc scaling */
	} u_prof;

#ifdef SYSTYPE_BSD43
	struct tty  *u_ttyp;		/* controlling tty pointer */
#else
	short  *u_ttyp;			/* pointer to pgrp in "tty" struct */
#endif
	dev_t	u_ttyd;			/* controlling tty dev */
	struct vnode *u_ttyvp;		/* vnode of controlling tty (streams) */

	long   u_execsz;

	/*
	 * Executable file info.
	 */
	struct exdata {
		struct    vnode  *vp;
		long      ux_tsize;	/* text size    */
		long      ux_dsize;	/* data size    */
		long      ux_bsize;	/* bss size     */
		long      ux_lsize;  	/* lib size     */
		long      ux_nshlibs; 	/* number of shared libs needed */
		short     ux_mag;   	/* magic number MUST be here */
		long      ux_toffset;	/* file offset to raw text      */
		long      ux_doffset;	/* file offset to raw data      */
		long      ux_loffset;	/* file offset to lib sctn      */
		long      ux_txtorg;	/* start addr. of text in mem   */
		long      ux_datorg;	/* start addr. of data in mem   */
		long      ux_entloc;	/* entry location               */
	} u_exdata;

	char	u_comm[PSCOMSIZ];
	char	u_psargs[PSARGSZ];	/* arguments from exec system call */

	time_t	u_start;
	time_t	u_ticks;
	long	u_mem;
	long	u_ior;
	long	u_iow;
	long	u_iosw;
	long	u_ioch;
	char	u_acflag;

	short	u_cmask;		/* mask for file creation */
	daddr_t	u_limit;		/* maximum write address */

	short	u_lock;			/* process/text locking flags */

	struct itimerval u_timer[3];
	int	u_syscall_type;		/* System type of last syscall. */
	int	u_eosys;		/* special action on end of syscall */

	int	u_filler[8];		/* room to avoid recompiling ps */
	int	u_cttyop;		/* controlling terminal operations */
	struct uio	*u_uio; 	/* ptr to uio struct for IO */

	struct ucred	*u_cred; 	/* ptr to credentials struct for IO */

#define	u_uid		u_cred->cr_uid
#define	u_gid		u_cred->cr_gid
#define	u_groups	u_cred->cr_groups
#define	u_ruid		u_cred->cr_ruid
#define	u_rgid		u_cred->cr_rgid

	char		*u_sav_pix;	/* save/resume array	*/
	char		*u_jmp_pix;	/* setjmp/longjmp array	*/

	int	u_filler_9[1];
	struct u_bsd43_extension u_bsd43_extension;

	/*
	 * THIS MUST STAY LAST!
	 */
	struct file	*u_ofile[1];	/* Start of array of pointers	*/
					/* to file table entries for	*/
					/* open files.			*/
} user_t;

#if defined(KERNEL) || defined(INKERNEL)
    /*
     * This "declaration" tells front end it can use r0 relative addressing.
     * Since u is strange, use up for debugging purposes.
     */
#define	u	(*(struct user *)0xffffc000)
    struct user *up;
#endif


#define u_exuid u_exfile.ux_uid
#define u_exgid u_exfile.ux_gid
#define u_exmode u_exfile.ux_mode

#define	u_rval1	u_r.r_reg.r_val1
#define	u_rval2	u_r.r_reg.r_val2
#define	u_roff	u_r.r_off
#define	u_rtime	u_r.r_time

#define u_ru	u_bsd43_extension.ue_bsd43_ru
#define u_cru	u_bsd43_extension.ue_bsd43_cru
#define u_sigmask u_bsd43_extension.ue_bsd43_sigmask
#define u_sigonstack u_bsd43_extension.ue_bsd43_sigonstack
#define u_sigintr u_bsd43_extension.ue_bsd43_sigintr
#define u_oldmask u_bsd43_extension.ue_bsd43_oldmask
#define u_sigstack u_bsd43_extension.ue_bsd43_sigstack
#define u_sig_flag u_bsd43_extension.ue_bsd43_sig_flag

#define u_onstack u_sigstack.ss_onstack
#define u_sigsp u_sigstack.ss_sp
#define u_rlimit u_bsd43_extension.ue_bsd43_rlimit
#define u_bsd43_start u_bsd43_extension.ue_bsd43_start

/* ioflag values: Read/Write, User/Kernel, Ins/Data */

#define	U_WUD	0
#define	U_RUD	1
#define	U_WKD	2
#define	U_RKD	3
#define	U_WUI	4
#define	U_RUI	5

/* u_ksegflg values:						    */
#define U_KS_USER	0	/* copyin and copyout to user space */
#define U_KS_KERNEL	1	/* copyin and copyout to kernel or  */
				/* user space 			    */

/* u_pofile flag values:					*/
#define	EXCLOSE	01
/* From NFS4.0 */
#define	UF_EXCLOSE	0x1	/* auto-close on exec */
#define	UF_MAPPED 	0x2	/* file mapped from device */
#define	UF_FDLOCK 	0x4	/* file desc locked (SysV style) */

/* distribution: values for u_rflags */
#define FREMOTE	0x0002	/* file is remote  */
#define RFLOCK	0x0004	/* for remote record locking*/

#define	U_RCDIR		0x0001	/* remote current directory */
#define	U_RRDIR		0x0002	/* remote root directory    */
#define	U_RSYS		0x0004	/* system call has gone remote */
#define	U_LBIN		0x0100	/* dotdot at lbin mount */
#define	U_DOTDOT	0x0200
#define U_RCOPY		0x0400	/* used by copyout for non-delay copy */

#define FULLRESTORE	1
#define RESTARTSYS	2
#define NORMALRETURN	3

/* (u_cttyop) controlling terminal operations */
#define	U_SETCTTY	0x0001		/* Set controlling TTY */
#define	U_CLRCTTY	0x0002		/* clear controlling TTY */

#ifdef MIPSEL
#define OBJMAGIC	0x0162
#define OBJMAGIC_MIPS2	0x6301
#endif
#ifdef MIPSEB
#define OBJMAGIC	0x0160
#define OBJMAGIC_MIPS2_OLD 0x0150
#define OBJMAGIC_MIPS2	0x163
#endif

#endif	_SYS_USER_
