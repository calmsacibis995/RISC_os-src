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
/* $Header: sg.h,v 1.4.4.2 90/05/10 06:36:31 wje Exp $ */

#ifndef	_SYS_SG_
#define	_SYS_SG_

/*
 * generic scatter / gather descriptor
 */
struct sg {
	unsigned long sg_ioaddr;
	unsigned long sg_bcount;
};

#endif	_SYS_SG_
