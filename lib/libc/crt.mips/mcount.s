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
/* $Header: mcount.s,v 1.9.2.2 90/05/10 01:22:48 wje Exp $ */

/*
 * mcount.s -- profiling procedure count routine for prof
 */

#include <sys/regdef.h>
#include <sys/asm.h>
#include <sys.s>

/*
 * mcount -- assumed to be called with ra being return address to immediate
 * caller and t0 to be address of address procedure call counter
 */

#define	CBSIZE		8
#define	CB_PC		0
#define	CB_NCALL	4

LEAF(mcount)
	#	register int *selfpc;	/* ra */
	#	register long **cntp;   /* t0 */
	#	register int profiling;	/* t1 */

	/*
	 * check that we aren't recursively invoked.
	 */
	lw	t1,_profiling
	bne	t1,zero,out
	addu	t1,1
	sw	t1,_profiling
	/*
	 * check that counter is allocated
	 */
	lw	t2,0(t0)		# t2 = *cntp
	bne	t2,zero,1f
	/*
	 * check that a counter is available
	 */
	lw	v0,_cntrs
	lw	v1,_numctrs
	beq	v0,v1,overflow
	addu	v0,1
	sw	v0,_cntrs
	lw	v1,_countbase
	sw	ra,CB_PC(v1)
	addu	t2,v1,CB_NCALL
	sw	t2,0(t0)
	addu	v1,CBSIZE
	sw	v1,_countbase
1:
	lw	v0,0(t2)
	addu	v0,1
	sw	v0,0(t2)
	subu	t1,1
	sw	t1,_profiling
out:
	j	ra

overflow:
	li	a0,2
	la	a1,tolimit
	li	a2,25
	li	v0,SYS_write
	syscall
	j	ra

tolimit:
	.asciiz	"mcount: counter overflow\n"
	END()
