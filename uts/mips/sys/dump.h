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
/* $Header: dump.h,v 1.6.1.2 90/05/10 06:11:29 wje Exp $ */

#ifndef	_SYS_DUMP_
#define	_SYS_DUMP_	1


/*
 * Constants and definitions for kernel crash dumping stuff
 */

#ifdef	INKERNEL
#define	DUMP_OPEN	1		/* initialize device */
#define	DUMP_WRITE	2		/* write some data */
#define	DUMP_CLOSE	3		/* close the device */
#endif

#define	MAX_PPHDR	((NBPC-16)/4)	/* maximum number of pages which can */
					/* be specified be each dump header  */

struct dumphdr {
	char 		magic[8];
	int		timestamp;
	int		numpages;
	unsigned int 	page[MAX_PPHDR];
};
#endif	_SYS_DUMP_
