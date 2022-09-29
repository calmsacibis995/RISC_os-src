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
/* $Header: limits.h,v 1.20.1.4 90/05/10 06:26:48 wje Exp $ */

#ifndef	_SYS_LIMITS_
#define	_SYS_LIMITS_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
        /usr/group Standards File limits.h for System V on 3B2"
*/

#ifndef	SYSTYPE_POSIX
#define	ARG_MAX		20480		/* max length of arguments to exec */
#define	CHILD_MAX	100		/* max # of processes per user id */
#define	OPEN_MAX	100		/* max # of files a proc can have open*/
#endif

#ifndef	_POSIX_LIMITS_
#define	CHAR_BIT	8		/* # of bits in a "char" */
#define SCHAR_MIN	(-128)		/* min value for signed char */
#define SCHAR_MAX	(127)		/* max value for signed char */
#define UCHAR_MAX	255		/* max value for unsigned char */
#define	CHAR_MIN	0		/* min integer value of a "char" */
#define	CHAR_MAX	255		/* max integer value of a "char" */
#define MB_LEN_MAX	1		/* man # bytes in a multi-byte char */
#define	SHRT_MIN	-32768		/* min decimal value of a "short" */
#define	SHRT_MAX	32767		/* max decimal value of a "short" */
#define USHRT_MAX	65535		/* max value for an unsigned short */
#define	INT_MIN		-2147483648	/* min decimal value of an "int" */
#define	INT_MAX		2147483647	/* max decimal value of an "int" */
#define UINT_MAX	4294967295	/* max value for an unsigned int */
#define	LONG_MIN	-2147483648	/* min decimal value of a "long" */
#define	LONG_MAX	2147483647	/* max decimal value of a "long" */
#define ULONG_MAX	4294967295	/* max value for an unsigned long */
#endif  /* _POSIX_LIMITS_ */

#ifndef	CLK_TCK
#define	CLK_TCK		100		/* # of clock ticks per second */
#endif

#ifndef LINK_MAX
#define	LINK_MAX	1000		/* max # of links to a single file */
#endif

#ifndef	PATH_MAX
#define	PATH_MAX	1024		/* max # of characters in a path name */
#endif

#ifndef	NAME_MAX
#define	NAME_MAX	255		/* max # of characters in a file name */
#endif

#ifndef	PIPE_BUF
#define	PIPE_BUF	5120		/* max # bytes atomic in write to a pipe */
#endif

#define	DBL_DIG		15		/* digits of precision of a "double" */
#define	DBL_MAX		1.79769313486231470e+308 /*max decimal value of a "double"*/
#define	DBL_MIN		4.94065645841246544e-324 /*min decimal value of a "double"*/
#define	FCHR_MAX	2147483647	/* max size of a file in bytes */
#define	FLT_DIG		7		/* digits of precision of a "float" */
#define	FLT_MAX		3.40282346638528860e+38 /*max decimal value of a "float" */
#define	FLT_MIN		1.40129846432481707e-45 /*min decimal value of a "float" */
#define	HUGE_VAL	3.40282346638528860e+38 /*error value returned by Math lib*/
#define	PASS_MAX	8		/* max # of characters in a password */
#define	PID_MAX		30000		/* max value for a process ID */
#define	PIPE_MAX	5120		/* max # bytes written to a pipe in a write */
#define	STD_BLK		1024		/* # bytes in a physical I/O block */
#define	SYS_NMLN	65		/* # of chars in uname-returned strings */
#define	UID_MAX		65536 		/* max value for a user or group ID */
#define	USI_MAX		4294967295	/* max decimal value of an "unsigned" */
#define	WORD_BIT	32		/* # of bits in a "word" or "int" */

#endif	_SYS_LIMITS_
