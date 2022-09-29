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
/* $Header: crash.h,v 1.4.1.3 90/05/09 15:25:48 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#undef NUMIPS
#define NUMIPS 1
#ifndef SYMTAB_C
#define KERNEL 1
#include "sys/param.h"
#include "sys/types.h"
#undef KERNEL
#endif SYMTAB_C
#include "a.out.h"
#ifndef SYMTAB_C
#include "signal.h"
#endif SYMTAB_C
#include "stdio.h"
#ifndef SYMTAB_C
#include "sys/sysmacros.h"
#include "bsd/sys/time.h"
#include "sys/psw.h"
#include "sys/pcb.h"
#include "sys/user.h"
#endif SYMTAB_C
#include "sys/sbd.h"
#ifndef SYMTAB_C
#include "sys/immu.h"
#include "sys/var.h"
#include "sys/conf.h"
#include "sys/sema.h"
#include "sys/comm.h"
#include "sys/region.h"
#include "sys/proc.h"
#define KERNEL 1
#include "sys/file.h"
#include "sys/vfs.h"
#undef KERNEL
#include "sys/vnode.h"
#include "sys/stream.h"
#undef free
#undef malloc
#undef calloc
#include "sys/buf.h"
#undef paddr
#include "sys/pfdat.h"
#include "sys/tuneable.h"
#include "sys/utsname.h"
#define KERNEL 1
#include "sys/fs/ufs_inode.h"
#undef KERNEL
#include "sys/fs/ufs_fsdir.h"
#endif SYMTAB_C

typedef int jmp_buf[28];
#include "string.h"

/* Generalized modifications of sys/immu.h defines */

#define Btotp(x)	((uint)(x) >> pfn_to_byte_shift)
#define Ctob(x)		((uint)(x) << pfn_to_byte_shift)
#define Pnum(x)		((uint)(x) >> pfn_to_byte_shift)
#define Kvtokptbl(X)	(&kptbl[Pnum((uint)(X) - (uint)K2SEG)])
#define	pte_to_pfn(x)	((x) >> pte_to_pfn_shift)
#define	pte_to_byte(x)	(((x) >> pte_to_pfn_shift) << pfn_to_byte_shift)

/* This file should include only command independent declarations */

#define ARGLEN 40	/* max length of argument */

extern FILE *fp;	/* output file */
extern int Procslot;	/* current process slot number */
extern int Virtmode;	/* current address translation mode */
extern int mem;		/* file descriptor for dumpfile */
extern jmp_buf syn;	/* syntax error label */
extern struct var vbuf;	/* tunable variables buffer */
extern char *args[];	/* argument array */
extern int argcnt;	/* number of arguments */
extern int optind;	/* argument index */
extern char *optarg;	/* getopt argument */
extern long getargs();	/* function to get arguments */
extern long strcon();	/* function to convert strings to long */
extern long eval();	/* function to evaluate expressions */
extern struct syment *symsrch(); /* function for symbol search */

struct procent {
	long isym;
	long iline;
	long regmask;
	short regoffset;
	short frameoffset;
};

struct syment {
	char *n_name;
	unsigned long n_value;
	int  n_scnum;
	struct procent *n_proc;
};
#define SYMESZ sizeof(struct syment)

struct syment *SYM_rootvfs,
	*SYM_spec_vfsops,
	*SYM_ufs_vfsops,
	*SYM_nfs_vfsops,
	*SYM_pcfs_vfsops,
	*SYM_lo_vfsops,
	*SYM_rfs_vfsops,
	*SYM_proc_vfsops,
	*SYM_inode,
	*SYM_ifreeh,
	*File,
  	*SYM_vfssw,
	*SYM_vfsNVFS,
	*Pagesize,
	*Proc,
	*V,
	*Panic,
	*Curproc,
	*Pregpp,
	*Region,
	*Pfdat;
	

enum	VFS_TYPE {
		VFS_TYPE_null,
		VFS_TYPE_spec,
		VFS_TYPE_ufs,
		VFS_TYPE_nfs,
		VFS_TYPE_pcfs,
		VFS_TYPE_lo,
		VFS_TYPE_rfs,
		VFS_TYPE_proc };

enum VFS_TYPE get_vfs_type();

#define min(a,b) ((a)>(b)? (b):(a))

#define GETARGS_NO_RANGE 0x80000000
				/* reserved value to disable range checking */
#define GETARGS_NO_MAX 0x7fffffff
				/* reserved vlaue to disable maximum */
				/* checking */
#define GETARGS_NO_SLOT 0x80000001
				/* reserved value to imply address only */
				/* (no slot, default hex values)	*/

struct	findvnodeargs {
		enum VFS_TYPE fva_type;
		int	fva_flags;
		int	fva_index;
		struct	vnode *fva_vp;
		union {
			struct	snode *fva_u_sp;
			struct	inode *fva_u_ip;
			struct	rnode *fva_u_rp;
		} fva_u;
	};
#define fva_sp fva_u.fva_u_sp
#define fva_ip fva_u.fva_u_ip
#define fva_rp fva_u.fva_u_rp

#define FVA_ALL	0x0001
#define FVA_SINGLE_TYPE 0x0002

extern struct	findvnodeargs null_findvnodeargs;

extern find_next_vnode();
