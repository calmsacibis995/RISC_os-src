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
/* $Header: mipskopt.h,v 1.5.4.2 90/05/10 06:28:22 wje Exp $ */

#ifndef	_SYS_MIPSKOPT_
#define	_SYS_MIPSKOPT_	1


/*
 * options for mipskopt system call
 */
#define	KOPT_GET	1		/* get kernel option */
#define	KOPT_SET	2		/* set kernel option */
#define	KOPT_BIS	3		/* or in new option value */
#define	KOPT_BIC	4		/* clear indicated bits */

#endif	_SYS_MIPSKOPT_
