/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: debug.h,v 1.1 90/04/11 14:27:01 hal Exp $ */

#ifndef	_SYS_DEBUG_
#define	_SYS_DEBUG_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#ifndef PRW_BUF
#include "sys/cmn_err.h"
#endif

#define	YES 1
#define	NO  0

#if DEBUG == YES
#define ASSERT(EX) if (!(EX))assfail("EX", __FILE__, __LINE__)
#define	METER(x)	x
/* for sgi compatability */
#define	OS_METER
#define	OS_DEBUG
#else
#define ASSERT(x)
#define	METER(x)
#undef	OS_METER
#undef	OS_DEBUG
#endif

#ifdef MONITOR
#define MONITOR(id, w1, w2, w3, w4) monitor(id, w1, w2, w3, w4)
#else
#define MONITOR(id, w1, w2, w3, w4)
#endif

/*
 * BSD type XPRINTF facility
 */

#ifdef LANGUAGE_C

#ifdef KERNEL

struct xprbuf {
	char *xp_msg;
	unsigned xp_arg1, xp_arg2, xp_arg3, xp_arg4;
	unsigned xp_timestamp;
	unsigned xp_pid, xp_tlbpid;
};

extern struct xprbuf *xprbase, *xprptr;
extern int xprsize, xprinitflag;
extern unsigned xpr_flags;

#ifdef XPR_DEBUG
#define XPR1(flags, format) \
	{ \
		if (xpr_flags & flags) \
			xprintf(format, 0, 0, 0, 0); \
	}
#define XPR2(flags, format, arg1) \
	{ \
		if (xpr_flags & flags) \
			xprintf(format, arg1, 0, 0, 0); \
	}
#define XPR3(flags, format, arg1, arg2) \
	{ \
		if (xpr_flags & flags) \
			xprintf(format, arg1, arg2, 0, 0); \
	}
#define XPR4(flags, format, arg1, arg2, arg3) \
	{ \
		if (xpr_flags & flags) \
			xprintf(format, arg1, arg2, arg3, 0); \
	}
#define XPR5(flags, format, arg1, arg2, arg3, arg4) \
	{ \
		if (xpr_flags & flags) \
			xprintf(format, arg1, arg2, arg3, arg4); \
	}
#define XPRINTF		XPR5			/* Compatibility */

#else XPR_DEBUG
#define	XPR1(flags, format)
#define	XPR2(flags, format, arg1)
#define	XPR3(flags, format, arg1, arg2)
#define	XPR4(flags, format, arg1, arg2, arg3)
#define	XPR5(flags, format, arg1, arg2, arg3, arg4)
#define	XPRINTF		XPR5
#endif XPRBUG

#endif KERNEL

#endif LANGUAGE_C

/*
 * flags
 */
#define XPR_CLOCK	0x00000001	/* Clock interrupt handler */
#define XPR_TLB		0x00000002	/* TLB miss handler */
#define XPR_INIT	0x00000004	/* routines called during init */
#define XPR_SCHED	0x00000008	/* Scheduler */
#define XPR_PROCESS	0x00000010	/* newproc/fork */
#define XPR_EXEC	0x00000020	/* Exec */
#define XPR_SYSCALL	0x00000040	/* System calls */
#define XPR_TRAP	0x00000080	/* Trap handler */
#define XPR_NOFAULT	0x00000100	/* Nofault bus error */
#define XPR_VM		0x00000200	/* VM */
#define XPR_SWAP	0x00000400	/* swapin/swapout */
#define XPR_SWTCH	0x00000800	/* swtch, setrq, remrq */
#define	XPR_DISK	0x00001000	/* disk i/o */
#define	XPR_TTY		0x00002000	/* mux i/o */
#define	XPR_TAPE	0x00004000	/* tape i/o */
#define	XPR_BIO		0x00008000	/* blk i/o */
#define	XPR_INTR	0x00010000	/* interrupt handling */
#define	XPR_RMAP	0x00020000	/* resource map handling */
#define	XPR_TEXT	0x00040000	/* shared text stuff */
#define	XPR_CACHE	0x00080000	/* cache handling */
#define	XPR_NFS		0x00100000	/* nfs */
#define	XPR_RPC		0x00200000	/* rpc */
#define	XPR_SIGNAL	0x00400000	/* signal handling */
#define	XPR_FPINTR	0x00800000	/* fp interrupt handling */
#define XPR_ENET	0x01000000	/* ethernet */
#define XPR_IP		0x02000000	/* internet protocol */


/*
 * bit field descriptions for printf %r and %R formats
 */

/*
 * printf("%r %R", val, reg_descp);
 * struct reg_desc *reg_descp;
 *
 * the %r and %R formats allow formatted output of bit fields.
 * reg_descp points to an array of reg_desc structures, each element of the
 * array describes a range of bits within val.  the array should have a
 * final element with all structure elements 0.
 * %r outputs a string of the format "<bit field descriptions>"
 * %R outputs a string of the format "0x%x<bit field descriptions>"
 *
 * The fields in a reg_desc are:
 *	unsigned rd_mask;	An appropriate mask to isolate the bit field
 *				within a word, and'ed with val
 *
 *	int rd_shift;		A shift amount to be done to the isolated
 *				bit field.  done before printing the isolate
 *				bit field with rd_format and before searching
 *				for symbolic value names in rd_values
 *
 *	char *rd_name;		If non-null, a bit field name to label any
 *				out from rd_format or searching rd_values.
 *				if neither rd_format or rd_values is non-null
 *				rd_name is printed only if the isolated
 *				bit field is non-null.
 *
 *	char *rd_format;	If non-null, the shifted bit field value
 *				is printed using this format.
 *
 *	struct reg_values *rd_values;	If non-null, a pointer to a table
 *				matching numeric values with symbolic names.
 *				rd_values are searched and the symbolic
 *				value is printed if a match is found, if no
 *				match is found "???" is printed.
 *				
 */

/*
 * register values
 * map between numeric values and symbolic values
 */
struct reg_values {
	unsigned rv_value;
	char *rv_name;
};

/*
 * register descriptors are used for formatted prints of register values
 * rd_mask and rd_shift must be defined, other entries may be null
 */
struct reg_desc {
	unsigned rd_mask;	/* mask to extract field */
	int rd_shift;		/* shift for extracted value, - >>, + << */
	char *rd_name;		/* field name */
	char *rd_format;	/* format to print field */
	struct reg_values *rd_values;	/* symbolic names of values */
};

#ifdef KERNEL

/*
 *	kernel arguments (and kopt) decoding table
 */

struct	kernargs {
	char	*name;
	int	*ptr;			/* XXX */
	short	readonly;
	short	boolean;
	int	dostring;		/* always copy string if != 0 */
};

/*
 * With vnodes, we do not want this stuff anymore, but it might be
 * handy some time in the future, so keep it around.
 */
#undef INODE_LOCK_CHECK

#ifdef INODE_LOCK_CHECK

extern int Xinode_lock_check;
extern int old_Xinode_lock_check;

#define INODE_LOCK_DISABLED() ((Xinode_lock_check == old_Xinode_lock_check) \
		? Xinode_lock_check : inode_lock_change_status())
#define INODE_LOCK_SET(ip) (INODE_LOCK_DISABLED() ? 0 : inode_lock_set(ip))
#define INODE_LOCK_UNSET(ip) (INODE_LOCK_DISABLED() ? 0 : inode_lock_unset(ip))
#define INODE_LOCK_TEST(pid) (INODE_LOCK_DISABLED() ? 0 : inode_lock_test(pid))

#else INODE_LOCK_CHECK

#define INODE_LOCK_SET(ip) ( 0 )
#define INODE_LOCK_UNSET(ip) ( 0 )
#define INODE_LOCK_TEST(pid) ( 0 )

#endif INODE_LOCK_CHECK
#endif KERNEL

#endif	_SYS_DEBUG_
