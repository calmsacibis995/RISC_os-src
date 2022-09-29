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
/* $Header: prot_lock.h,v 1.1.1.2.1.1.1.2 90/11/17 11:53:32 beacker Exp $ */

/* @(#)nfs.cmds:nfs/lockd/prot_lock.h 1.4 */
/*
 * This file consists of all structure information used by lock manager 
 */
#include <rpc/rpc.h>
#include <sys/fcntl.h>
#include "flock.h"
#ifdef RISCOS
#include "bsd43/sys/signal.h"
#else
#include "signal.h"
#endif
#include "nlm_prot.h"
#include "klm_prot.h"
#include "lockf.h"

typedef struct nlm_testres remote_result;

#define lstat stat.stat
#define lholder stat.nlm_testrply_u.holder

#ifndef RISCOS
/*
 * Flock call.
 * used to be in /usr/include/sys/file.h
 */
#define LOCK_SH		1	/* shared lock */
#define LOCK_EX		2	/* exclusive lock */
#define LOCK_NB		4	/* don't block when locking */
#define LOCK_UN		8	/* unlock */

/*
 * Used to be in /usr/include/sys/signal.h
 */
#define	SIGLOST		29	/* resource lost (eg, record-lock lost) */
#endif

/*
 * Used to be in flock.c
 */
#ifdef RISCOS
#define SAMEOWNER(flockap, flockbp, sysida, sysidb) \
(((flockap)->l_pid == (flockbp)->l_pid) && (sysida == sysidb))
#else
#define SAMEOWNER(a, b) \
(((a)->l_pid == (b)->l_pid) && ((a)->l_sysid == (b)->l_sysid))
#endif

#define same_proc(x, y) (obj_cmp(&x->lck.oh, &y->lck.oh))

#define NLM_LOCK_RECLAIM	16
#define MSG 	0		/* choices of comm to remote svr */
#define RPC 	1		/* choices of comm to remote svr */

#define	MAXLEN		MAXEND

#define lck 		alock
#define lld		filocks.set
#define svr		server_name
#define caller		caller_name
#define clnt		clnt_name
#define fh_len		fh.n_len
#define fh_bytes	fh.n_bytes
#define oh_len		oh.n_len
#define oh_bytes	oh.n_bytes
#define cookie_len	cookie.n_len
#define cookie_bytes	cookie.n_bytes

#define denied		nlm_denied
#define nolocks 	nlm_denied_nolocks
#define blocking	nlm_blocked
#define grace		nlm_denied_grace_period
#define deadlck		nlm_deadlck
#define rpc_error	6

/*
 * warning:  struct alock consists of klm_lock and nlm_lock,
 * it has to be modified if either structure has been modified!!!
 */
struct alock {
	netobj cookie;

	/* from klm_prot.h */
        char *server_name;
        netobj fh;

	/* from lockf.h */
	struct data_lock lox;

	/* addition from nlm_prot.h */
	char *caller_name;
	netobj oh;
	int svid;
	u_int l_offset;
        u_int l_len;

	/* addition from lock manager */
	char *clnt_name;
	int op;
};


struct reclock {
	netobj cookie;
	bool_t block;
	bool_t exclusive;
	struct alock alock;
	bool_t reclaim;
	int state;

	/* auxiliary structure */
	SVCXPRT *transp; /* transport handle for delayed response due to */ 
			 /* blocking or grace period */
	int rel;
	int w_flag;

	struct reclock *prev; 
	struct reclock *next;
};
typedef struct reclock reclock;

/*
 * Lock manager vp->v_filocks
 */
struct lm_vnode {
        char *server_name;
        netobj fh;
	struct filock *filocks;
	struct lm_vnode *prev;
	struct lm_vnode *next;
	struct reclock *reclox;
	int    rel;
};
typedef struct lm_vnode lm_vnode;

struct timer {
	/* timer goes off when exp == curr */
	int exp;
	int curr;
};
typedef struct timer timer;

/*
 * msg passing structure
 */
struct msg_entry {
	reclock *req;
	remote_result *reply;
	timer t;
	int proc; /* procedure name that req is sent to; needed for reply purpose */
	struct msg_entry *prev;
	struct msg_entry *nxt;
};
typedef struct msg_entry msg_entry;

struct priv_struct {
	int pid;
	int *priv_ptr;
};
