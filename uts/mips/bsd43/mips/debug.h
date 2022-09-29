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
/* $Header: debug.h,v 1.6.3.2 90/05/10 04:40:49 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Debug macros.
 */

#ifdef LANGUAGE_C

struct bsd43_(xprbuf) {
	char *xp_msg;
	unsigned xp_arg1, xp_arg2, xp_arg3, xp_arg4;
	unsigned xp_timestamp;
	unsigned xp_pid, xp_tlbpid;
};

#ifdef KERNEL

#ifdef ASSERTIONS
#define BSD43_ASSERT(EX) { if (EX) ; else assfail("EX", "/tmp/safe_cpp.17931.1", 5804) }
#else ASSERTIONS
#define BSD43_ASSERT(EX)
#endif ASSERTIONS

extern struct bsd43_(xprbuf) *bsd43_(xprbase), *bsd43_(xprptr);
extern int bsd43_(xprsize), bsd43_(xprinitflag);
extern unsigned bsd43_(xpr_flags);

#ifdef XPRBUG
#define BSD43_XPRINTF(flags, format, arg1, arg2, arg3, arg4) \
	{ \
		if (bsd43_(xpr_flags) & flags) \
			xprintf(format, arg1, arg2, arg3, arg4); \
	}
#else XPRBUG
#define	BSD43_XPRINTF(flags, format, arg1, arg2, arg3, arg4)
#endif XPRBUG

#endif KERNEL

#endif LANGUAGE_C

/*
 * flags
 */
#define BSD43_XPR_CLOCK	0x00000001	/* Clock interrupt handler */
#define BSD43_XPR_TLB		0x00000002	/* TLB miss handler */
#define BSD43_XPR_INIT	0x00000004	/* routines called during init */
#define BSD43_XPR_SCHED	0x00000008	/* Scheduler */
#define BSD43_XPR_PROCESS	0x00000010	/* newproc/fork */
#define BSD43_XPR_EXEC	0x00000020	/* Exec */
#define BSD43_XPR_SYSCALL	0x00000040	/* System calls */
#define BSD43_XPR_TRAP	0x00000080	/* Trap handler */
#define BSD43_XPR_NOFAULT	0x00000100	/* Nofault bus error */
#define BSD43_XPR_VM		0x00000200	/* VM */
#define BSD43_XPR_SWAP	0x00000400	/* swapin/swapout */
#define BSD43_XPR_SWTCH	0x00000800	/* swtch, setrq, remrq */
#define	BSD43_XPR_DISK	0x00001000	/* disk i/o */
#define	BSD43_XPR_TTY		0x00002000	/* mux i/o */
#define	BSD43_XPR_TAPE	0x00004000	/* tape i/o */
#define	BSD43_XPR_BIO		0x00008000	/* blk i/o */
#define	BSD43_XPR_INTR	0x00010000	/* interrupt handling */
#define	BSD43_XPR_RMAP	0x00020000	/* resource map handling */
#define	BSD43_XPR_TEXT	0x00040000	/* shared text stuff */
#define	BSD43_XPR_CACHE	0x00080000	/* cache handling */
#define	BSD43_XPR_NFS		0x00100000	/* nfs */
#define	BSD43_XPR_RPC		0x00200000	/* rpc */
#define	BSD43_XPR_SIGNAL	0x00400000	/* signal handling */
#define	BSD43_XPR_FPINTR	0x00800000	/* fp interrupt handling */

/*
 * options for mipskopt system call
 */
#define	BSD43_KOPT_GET	1		/* get kernel option */
#define	BSD43_KOPT_SET	2		/* set kernel option */
#define	BSD43_KOPT_BIS	3		/* or in new option value */
#define	BSD43_KOPT_BIC	4		/* clear indicated bits */

#ifdef LANGUAGE_C

/*
 * The following is a table of symbolic names and addresses of kernel
 * variables which can be tuned to alter the performance of the system.
 * They can be modified at boot time as a boot parameter or by the mipskopt
 * system call.  Variables marked as readonly can't be modifed after system
 * boot time (i.e. through the mipskopt call).  "func" is called after the
 * variable is set in case there is processing beyond storing the new value.
 */
struct bsd43_(kernargs) {
	char *bsd43_(name);
	int *ptr;
	int readonly;
	int (*func)();
};

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
struct bsd43_(reg_values) {
	unsigned rv_value;
	char *rv_name;
};

/*
 * register descriptors are used for formatted prints of register values
 * rd_mask and rd_shift must be defined, other entries may be null
 */
struct bsd43_(reg_desc) {
	unsigned rd_mask;	/* mask to extract field */
	int rd_shift;		/* shift for extracted value, - >>, + << */
	char *rd_name;		/* field name */
	char *rd_format;	/* format to print field */
	struct bsd43_(reg_values) *rd_values;	/* symbolic names of values */
};

#ifdef KERNEL
extern struct bsd43_(reg_values) bsd43_(pstat_values)[];
extern struct bsd43_(reg_values) bsd43_(sig_values)[];
extern struct bsd43_(reg_values) bsd43_(imask_values)[];
extern struct bsd43_(reg_values) bsd43_(exc_values)[];
extern struct bsd43_(reg_values) bsd43_(fileno_values)[];
extern struct bsd43_(reg_values) bsd43_(prot_values)[];
extern struct bsd43_(reg_values) bsd43_(syscall_values)[];
extern struct bsd43_(reg_desc) bsd43_(sr_desc)[];
extern struct bsd43_(reg_desc) bsd43_(exccode_desc)[];
extern struct bsd43_(reg_desc) bsd43_(cause_desc)[];
extern struct bsd43_(reg_desc) bsd43_(tlbhi_desc)[];
extern struct bsd43_(reg_desc) bsd43_(tlblo_desc)[];
extern struct bsd43_(reg_desc) bsd43_(tlbinx_desc)[];
extern struct bsd43_(reg_desc) bsd43_(tlbrand_desc)[];
extern struct bsd43_(reg_desc) bsd43_(tlbctxt_desc)[];
extern struct bsd43_(reg_desc) bsd43_(pte_desc)[];
#endif KERNEL
#endif LANGUAGE_C

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define ASSERT BSD43_ASSERT
#   define KOPT_BIC BSD43_KOPT_BIC
#   define KOPT_BIS BSD43_KOPT_BIS
#   define KOPT_GET BSD43_KOPT_GET
#   define KOPT_SET BSD43_KOPT_SET
#   define XPRINTF BSD43_XPRINTF
#   define XPR_BIO BSD43_XPR_BIO
#   define XPR_CACHE BSD43_XPR_CACHE
#   define XPR_CLOCK BSD43_XPR_CLOCK
#   define XPR_DISK BSD43_XPR_DISK
#   define XPR_EXEC BSD43_XPR_EXEC
#   define XPR_FPINTR BSD43_XPR_FPINTR
#   define XPR_INIT BSD43_XPR_INIT
#   define XPR_INTR BSD43_XPR_INTR
#   define XPR_NFS BSD43_XPR_NFS
#   define XPR_NOFAULT BSD43_XPR_NOFAULT
#   define XPR_PROCESS BSD43_XPR_PROCESS
#   define XPR_RMAP BSD43_XPR_RMAP
#   define XPR_RPC BSD43_XPR_RPC
#   define XPR_SCHED BSD43_XPR_SCHED
#   define XPR_SIGNAL BSD43_XPR_SIGNAL
#   define XPR_SWAP BSD43_XPR_SWAP
#   define XPR_SWTCH BSD43_XPR_SWTCH
#   define XPR_SYSCALL BSD43_XPR_SYSCALL
#   define XPR_TAPE BSD43_XPR_TAPE
#   define XPR_TEXT BSD43_XPR_TEXT
#   define XPR_TLB BSD43_XPR_TLB
#   define XPR_TRAP BSD43_XPR_TRAP
#   define XPR_TTY BSD43_XPR_TTY
#   define XPR_VM BSD43_XPR_VM
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


