#ifndef	_SYS_TIRDWR_
#define	_SYS_TIRDWR_	1

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
#ident "$Header: tirdwr.h,v 1.1.1.1.1.1.1.1 90/10/02 18:32:15 beacker Exp $"

struct trw_trw {
	long 	 trw_flags;
	queue_t	*trw_rdq;
	mblk_t  *trw_mp;
};

#define USED 		001
#define ORDREL 		002
#define DISCON  	004
#define FATAL		010
#define WAITACK 	020

#define TIRDWRPRI 	PZERO+3

extern struct trw_trw trw_trw[];
extern int trw_cnt;
extern nulldev();

#define TIRDWR_ID	4

#endif	_SYS_TIRDWR_
