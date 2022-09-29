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
/* $Header: klm_prot.h,v 1.1.1.1.1.1.1.1 90/11/17 11:42:59 beacker Exp $ */

/* @(#)nfs.cmds:nfs/lockd/klm_prot.h 1.1 */

#define KLM_PROG ((u_long)100020)
#define KLM_VERS ((u_long)1)
#define KLM_TEST ((u_long)1)
#define KLM_LOCK ((u_long)2)
#define KLM_CANCEL ((u_long)3)
#define KLM_UNLOCK ((u_long)4)

#define LM_MAXSTRLEN 1024

enum klm_stats {
	klm_granted = 0,
	klm_denied = 1,
	klm_denied_nolocks = 2,
	klm_working = 3,
	klm_deadlck = 5,
};
typedef enum klm_stats klm_stats;
bool_t xdr_klm_stats();


struct klm_lock {
	char *server_name;
	netobj fh;
	int base;
	int length;
	int type;
	int granted;
	int color;
	int LockID;
	int pid;
	int class;
	long rsys;
	long rpid;
};
typedef struct klm_lock klm_lock;
bool_t xdr_klm_lock();


struct klm_holder {
	bool_t exclusive;
	int base;
	int length;
	int type;
	int granted;
	int color;
	int LockID;
	int pid;
	int class;
	long rsys;
	long rpid;
};
typedef struct klm_holder klm_holder;
bool_t xdr_klm_holder();


struct klm_stat {
	klm_stats stat;
};
typedef struct klm_stat klm_stat;
bool_t xdr_klm_stat();


struct klm_testrply {
	klm_stats stat;
	union {
		struct klm_holder holder;
	} klm_testrply_u;
};
typedef struct klm_testrply klm_testrply;
bool_t xdr_klm_testrply();


struct klm_lockargs {
	bool_t block;
	bool_t exclusive;
	struct klm_lock alock;
};
typedef struct klm_lockargs klm_lockargs;
bool_t xdr_klm_lockargs();


struct klm_testargs {
	bool_t exclusive;
	struct klm_lock alock;
};
typedef struct klm_testargs klm_testargs;
bool_t xdr_klm_testargs();


struct klm_unlockargs {
	struct klm_lock alock;
};
typedef struct klm_unlockargs klm_unlockargs;
bool_t xdr_klm_unlockargs();

