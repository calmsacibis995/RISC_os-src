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
/* $Header: protoio.h,v 1.1.2.2 90/05/07 19:41:04 wje Exp $ */

/*
 * protoio.h (tip version) -- protocol io interface file
 */

typedef	FILE	*pdev_t;
extern int *timer_jmpbuf;

#define	PUTC(c, fd)	pputc(c, fd)
#define	GETC(fd)	pgetc(fd)
#define	PUTFLUSH(fd)	putflush(fd)
#define	PINIT(fd)	pinit(fd)
