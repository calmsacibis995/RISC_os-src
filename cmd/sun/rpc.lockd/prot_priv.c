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
#ident	"$Header: prot_priv.c,v 1.2.1.2.1.1.1.2 90/11/17 11:56:12 beacker Exp $"
/* @(#)nfs.cmds:nfs/lockd/prot_priv.c	1.5 */
/*
 * Consists of all private protocols for commicating with the
 * status monitor to handle crash and recovery
 */

#include <sys/param.h>
#include <stdio.h>
#include <memory.h>
#ifdef RISCOS
#include <sys/file.h>
#include <bsd43/sys/dirent.h>
#endif
#include "prot_lock.h"
#include "priv_prot.h"
#include "sm_inter.h"

extern int debug;
extern int pid;
#ifdef RISCOS
extern char hostname[MAXNAMLEN];
#else
extern char hostname[MAXNAMELEN];
#endif
extern int local_state;
extern lm_vnode *fh_q;
extern struct msg_entry *retransmitted();
void proc_priv_crash(), proc_priv_recovery();

int cookie;

void
priv_prog(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	char *(*Local)();
	struct status stat;
	extern bool_t xdr_status();

	if (debug)
		printf("Enter PRIV_PROG ...............\n");

	switch (rqstp->rq_proc) {
	case PRIV_CRASH:
		Local = (char *(*)()) proc_priv_crash;
		break;
	case PRIV_RECOVERY:
		Local = (char *(*)()) proc_priv_recovery;
		break;
	default:
		svcerr_noproc(transp);
		return;
	}

	(void) memset(&stat, 0, sizeof (struct status));
	if (!svc_getargs(transp, xdr_status, &stat)) {
		svcerr_decode(transp);
		return;
	}
	(*Local)(&stat);
	if (!svc_sendreply(transp, xdr_void, NULL)) {
		svcerr_systemerr(transp);
	}
	if (!svc_freeargs(transp, xdr_status, &stat)) {
		fprintf(stderr, "unable to free arguments\n");
		exit(1);
	}
}

void
proc_priv_crash()
{
	struct lm_vnode *mp;
	struct reclock *next, *nl;
	struct priv_struct *privp;

	if (debug)
		printf("enter proc_priv_CRASH....\n");

	/*
	 * No crash recovery necessary since server lock manager no longer
	 * keeps track of the locks. All lock information are kept in the
	 * server kernel ufs code.
	 */
}

void
proc_priv_recovery(statp)
	struct status *statp;
{
	struct lm_vnode *mp;
	struct reclock *ff, *t, *nl, *get_reclock();
	struct priv_struct *privp;
	char *xmalloc();

	if (debug)
		printf("enter proc_priv_RECOVERY.....\n");
	privp = (struct priv_struct *) statp->priv;
	if (privp->pid != pid) {
		if (debug)
			printf("this is not for me(%d): %d\n", privp->pid, pid);
		return;
	}

	if (debug)
		printf("enter proc_lm_recovery due to %s state(%d)\n",
			statp->mon_name, statp->state);

	destroy_client_shares(statp->mon_name);

	delete_hash(statp->mon_name);
	if (!up(statp->state)) {
		if (debug)
			printf("%s is not up.\n", statp->mon_name);
		return;
	}
	if (strcmp(statp->mon_name, hostname) == 0) {
		if (debug)
			printf("I have been declared as failed!!!\n");
		/*
		 * update local status monitor number
		 */
		local_state = statp->state;
	}

	if (fh_q != NULL) {
		for (mp = fh_q; mp; mp = mp->next) {
#ifdef RISCOS
			if (mp->reclox == NULL) { /* no record lock list */
			  continue;
                        }
			ff = mp->reclox;
#else
			if (mp->reclox != NULL) {
                                ff = mp->reclox;
                        }
#endif
			for (; ff; ff = ff->next) {
				if ((nl = get_reclock()) != NULL) {
					nl->state = 0;
					nl->reclaim = 1;
					if ((nl->lck.svr =
						xmalloc(strlen(statp->mon_name))) == NULL)
						printf("malloc failed.\n");
					(void) strcpy(nl->lck.svr,
						statp->mon_name);
					if ((nl->lck.caller_name =
						xmalloc(strlen(hostname))) == NULL)
						printf("malloc failed.\n");
					(void) strcpy(nl->lck.caller_name,
						hostname);
					nl->lck.svid = ff->lck.lox.lld.l_pid;
					nl->lck.fh.n_len = 0;
					nl->lck.lox.lld.l_start = ff->lck.lox.lld.l_start;
					nl->lck.lox.lld.l_len = ff->lck.lox.lld.l_len;
					nl->lck.lox.lld.l_type = ff->lck.lox.lld.l_type;
					nl->lck.lox.lld.l_pid = ff->lck.lox.lld.l_pid;
#ifndef RISCOS
					nl->lck.lox.lld.l_sysid = ff->lck.lox.lld.l_sysid;
#endif
					nl->lck.lox.class = 0;
					nl->lck.lox.granted = 0;
					nl->lck.lox.color = 0;
					nl->lck.lox.LockID = ff->lck.lox.LockID;
					if (ff->lck.lox.lld.l_type == F_WRLCK) {
						nl->lck.lox.lld.l_type = F_WRLCK;
						nl->exclusive = TRUE;
						nl->lck.op = LOCK_EX;
					} else {
						nl->lck.lox.lld.l_type = F_RDLCK;
						nl->exclusive = FALSE;
						nl->lck.op = LOCK_SH;
					}
					/*
					 * owner handle ==
					 * 	(hostname, pid);
					 * cannot generate owner handle
					 * use obj_alloc
					 * because additioanl pid
					 * attached at the end
					 */
					nl->lck.oh_len = strlen(hostname) +
						sizeof (int) + 1;
					if ((nl->lck.oh_bytes =
						xmalloc(nl->lck.oh_len)) == NULL)
						printf("malloc failed.\n");
					(void) strcpy(nl->lck.oh_bytes,
						hostname);
					memcpy(&nl->lck.oh_bytes[strlen(hostname)+1],
						(char *) &nl->lck.lox.lld.l_pid,
						sizeof (int));
					/*
					 * get fhandle
					 */
					get_fhandle(nl, ff->lck.lox.LockID);
					/*
					 * generate cookie
					 * cookie is generated from
					 * monitonically increasing #
					 */
					cookie++;
					if (obj_alloc(&nl->cookie,
						(char *) &cookie,
						sizeof (int))== -1)
						printf("obj_alloc failed.\n");
					if (nlm_call(NLM_LOCK_RECLAIM, nl, 0) == -1)
						if (queue(nl, NLM_LOCK_RECLAIM) == NULL)
							fprintf(stderr,
								"reclaim requet (%x) cannot be sent and cannot be queued for resend later!\n", nl);
				}
			}
		}
	}
}

get_fhandle(a, id)
	reclock *a;
	int id;
{
	struct lm_vnode *ptr;
	struct reclock *t;
	char *xmalloc();

	for (ptr = fh_q; ptr; ptr = ptr->next) {
		for (t = ptr->reclox; t != NULL; t = t->next) {
			if (id == t->lck.lox.LockID) {
				if ((a->lck.fh.n_bytes =
					xmalloc(t->lck.fh.n_len)) == NULL)
					printf("malloc failed.\n");
				obj_copy(&a->lck.fh, &t->lck.fh);
				a->lck.fh.n_len = t->lck.fh.n_len;
				a->block = t->block;
				if (debug) {
					printf("get_fhandle (to) : \n");
					pr_lock(a);
					printf("get_fhandle (from) : \n");
					pr_lock(t);
				}
				break;
			}
		}
	}
}
