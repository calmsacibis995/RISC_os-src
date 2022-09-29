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
/* $Header: sm_res.h,v 1.1.1.2.1.1.1.2 90/11/17 11:58:56 beacker Exp $ */

/* @(#)nfs.cmds:nfs/lockd/sm_res.h 1.1 */

struct stat_res {
	res res_stat;
	union {
		sm_stat_res stat;
		int rpc_err;
	}u;
};

#define sm_stat 	u.stat.res_stat
#define sm_state 	u.stat.state

