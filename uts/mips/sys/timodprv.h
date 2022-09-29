#ifndef	_SYS_TIMODPRV_
#define	_SYS_TIMODPRV_	1

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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: timodprv.h,v 1.1.1.1.1.1.1.1 90/10/02 18:31:44 beacker Exp $"

struct tim_tim {
	long 	 tim_flags;
	queue_t	*tim_rdq;
	mblk_t  *tim_iocsave;
};

extern struct tim_tim tim_tim[];
extern int tim_cnt;
extern nulldev();

#define TIMOD_ID	3

#endif	_SYS_TIMODPRV_
