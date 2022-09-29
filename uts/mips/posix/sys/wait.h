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
/* $Header: wait.h,v 1.3.1.2 90/05/10 06:04:13 wje Exp $ */

#ifndef	_POSIX_SYS_WAIT_
#define	_POSIX_SYS_WAIT_	1

#define WNOHANG		1	/* dont hang in wait */
#define WUNTRACED	2	/* tell about stopped, untraced children */

/* Evaluates to a non-zero value if status was returned for a child
 * process that terminated normally.
 */
#define WIFEXITED(x)	(((x)&0xff) == 0)

/* If the value of WIFEXITED is non-zero, then this macro evaluates to the 
 * low-order 8 bits of the value passed to exit.
 */
#define WEXITSTATUS(x)  ((((x)&0xff) == 0) ? ((x)&0xff00)>>8 : 0)

/* Evaluates to a non-zero value if status was returned for a child
 * process that is currently stopped
 */ 
#define WIFSTOPPED(x)	(((x)&0xff) == 0177)

/* If the value of WIFSTOPPED is non-zero, then this macro evaluates to the 
 * number of the signal that caused this child to stop.
 */
#define WSTOPSIG(x) 	(((x)&0xff) == 0177 ? ((x)&0xff00)>>8 : 0)

/* Evaluates to a non-zero value if status was returned for a child
 * process that terminated due to the receipt of a signal that was
 * not caught.
 */ 
#define WIFSIGNALED(x)	((((x)&0xff) != 0177) && (((x)&0x3f) != 0))

/* If the value of WIFSIGNALED is non-zero, then this macro evaluates to the 
 * number of the signal that caused this child to terminate.
 */
#define WTERMSIG(x)     (((((x)&0xff) != 0177) && (((x)&0x3f) != 0)) ? \
			 				((x)&0x3f) : 0)


extern	pid_t	wait();
extern	pid_t	waitpid();

#endif	_POSIX_SYS_WAIT_
