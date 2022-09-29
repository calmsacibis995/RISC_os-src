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
/* $Header: syscall.h,v 1.3.4.2 90/05/10 06:39:43 wje Exp $ */
/* "$Header: syscall.h,v 1.3.4.2 90/05/10 06:39:43 wje Exp $" */

#ifndef	_SYS_SYSCALL_
#define	_SYS_SYSCALL_	1



/*
 * SYSCALL -- standard system call sequence
 * The kernel expects arguments to be passed with the normal C calling
 * sequence.  v0 should contain the system call number.  On return from
 * the kernel mode, a3 will be 0 to indicate no error and non-zero to
 * indicate an error; if an error occurred v0 will contain an errno.
 */
#define	SYSCALL(x)					\
LEAF(x);						\
	li	v0,SYS_/**/x;				\
	syscall;					\
	beq	a3,zero,9f;				\
	j	_cerror;				\
9:

/*
 * PSEUDO -- system call sequence for syscalls that are variations of other
 * system calls
 */
#define	PSEUDO(x,y)					\
LEAF(x);						\
	li	v0,SYS_/**/y;				\
	syscall

#define	CALL(y)						\
	jal	y

#define	RET(x)						\
	j	ra;					\
	.end	x

	.globl	_cerror


#endif	_SYS_SYSCALL_
