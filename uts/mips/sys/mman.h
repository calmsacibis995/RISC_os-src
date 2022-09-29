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
/* $Header: mman.h,v 1.3.4.2 90/05/10 06:28:33 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mman.h	7.1 (Berkeley) 6/4/86
 */

/* protections are chosen from these bits, or-ed together */
#define PROT_NONE	0x0		/* page can not be accessed */
#define	PROT_READ	0x4		/* pages can be read */
#define	PROT_WRITE	0x2		/* pages can be written */
#define	PROT_EXEC	0x1		/* pages can be executed */
#define	PROT_EXECUTE	0x1		/* pages can be executed */

/* sharing types: choose either SHARED or PRIVATE */
#define	MAP_SHARED	1		/* share changes */
#define	MAP_PRIVATE	2		/* changes are private */
#define MAP_TYPE	0xf		/* mask for mapping type */

/* advice to madvise */
#define	MADV_NORMAL	0		/* no further special treatment */
#define	MADV_RANDOM	1		/* expect random page references */
#define	MADV_SEQUENTIAL	2		/* expect sequential page references */
#define	MADV_WILLNEED	3		/* will need these pages */
#define	MADV_DONTNEED	4		/* dont need these pages */
