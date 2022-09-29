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

#ident	"$Header: prot_alloc.c,v 1.2.1.2.1.1.1.2 90/11/17 11:51:57 beacker Exp $"

/* @(#)nfs.cmds:nfs/lockd/prot_alloc.c	1.4 */
/*
 * prot_alloc.c
 * consists of routines used to allocate and free space
 */

#include <stdio.h>
#include "prot_lock.h"

char *xmalloc(), *malloc();
extern int res_len, lock_len, debug;

/* # of entries used */
int used_reclock = 0;
int used_res = 0;
int used_fh = 0;
int used_me = 0;

void
xfree(a)
	char **a;
{
	if (*a != NULL) {
		free(*a);
		*a = NULL;
	}
}

release_res(resp)
	remote_result *resp;
{
	used_res--;
	xfree(&resp->cookie_bytes);
	if ((char *)resp != NULL) free((char *)resp);
}

release_filock(fl)
        struct filock *fl;
{       
	(void) memset((char *) fl, 0, sizeof (*fl));
        if ((char *) fl != NULL) free((char *) fl);
}

release_fh(a)
	lm_vnode *a;
{
	if (debug)
		printf("enter release_fh...\n");
	if (a->rel) {
		used_fh--;
		xfree(&a->svr);
		xfree(&a->fh);
		(void) memset((char *) a, 0, sizeof (*a));
		if ((char *)a != NULL) free((char *) a);
	}
}

free_reclock(a)
	reclock *a;
{
	used_reclock--;
	/*
	 * Make sure that this lock is not queued on msg_q.
	 * This addresses bugs 1012630 and 1011992, though
	 * the cause remains unknown.
	 */
	dequeue_lock(a);
	/* free up all space allocated through malloc */
	xfree(&a->lck.svr);
	xfree(&a->lck.fh_bytes);
	xfree(&a->lck.caller_name);
	xfree(&a->lck.oh_bytes);
	xfree(&a->cookie_bytes);
	xfree(&a->lck.clnt_name);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}

release_reclock(a)
	reclock *a;
{
	if (a->rel) {
		free_reclock(a);
	}
}


release_nlm_lockargs(a)
	nlm_lockargs *a;
{
	/* free up all space allocated through malloc */
	xfree(&a->cookie_bytes);
	xfree(&a->lck.caller_name);
	xfree(&a->lck.fh_bytes);
	xfree(&a->lck.oh_bytes);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}

release_nlm_unlockargs(a)
	nlm_unlockargs *a;
{
	/* free up all space allocated through malloc */
	xfree(&a->cookie_bytes);
	xfree(&a->lck.caller_name);
	xfree(&a->lck.fh_bytes);
	xfree(&a->lck.oh_bytes);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}

release_nlm_testargs(a)
	nlm_testargs *a;
{
	/* free up all space allocated through malloc */
	xfree(&a->cookie_bytes);
	xfree(&a->lck.caller_name);
	xfree(&a->lck.fh_bytes);
	xfree(&a->lck.oh_bytes);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}

release_nlm_cancargs(a)
	nlm_cancargs *a;
{
	/* free up all space allocated through malloc */
	xfree(&a->cookie_bytes);
	xfree(&a->lck.caller_name);
	xfree(&a->lck.fh_bytes);
	xfree(&a->lck.oh_bytes);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}

release_nlm_testres(a)
	nlm_testres *a;
{
	/* free up all space allocated through malloc */
	xfree(&a->cookie_bytes);
	xfree(&a->stat.nlm_testrply_u.holder.oh_bytes);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}

release_nlm_res(a)
	nlm_res *a;
{
	/* free up all space allocated through malloc */
	xfree(&a->cookie_bytes);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}

release_klm_lockargs(a)
	klm_lockargs *a;
{
	/* free up all space allocated through malloc */
	xfree(&a->lck.svr);
	xfree(&a->lck.fh_bytes);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}

release_klm_unlockargs(a)
	klm_unlockargs *a;
{
	/* free up all space allocated through malloc */
	xfree(&a->lck.svr);
	xfree(&a->lck.fh_bytes);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}

release_klm_testargs(a)
	klm_testargs *a;
{
	/* free up all space allocated through malloc */
	xfree(&a->lck.svr);
	xfree(&a->lck.fh_bytes);
	(void) memset((char *) a, 0, sizeof (*a));
	if ((char *) a != NULL) free((char *) a);
}


/*
 * allocate space and zero it;
 * in case of malloc error, print console msg and return NULL;
 */
char *
xmalloc(len)
	unsigned len;
{
	char *new;

	if ((new = malloc(len)) == 0) {
		perror("malloc");
		return (NULL);
	}
	else {
		(void) memset(new, 0, len);
		return (new);
	}
}


/*
 * these routines are here in case we try to optimize calling to malloc
 */
struct lm_vnode *
get_me()
{
	used_me++;
	return ( (struct lm_vnode *) xmalloc(sizeof (struct lm_vnode)) );
}

remote_result *
get_res()
{
	used_res++;
	return ( (remote_result *) xmalloc(res_len) );
}

lm_vnode *
get_fh()
{
	if (debug)
		printf("enter get_fh...\n");
	used_fh++;
	return ( (lm_vnode *) xmalloc(sizeof (struct lm_vnode)) );
}

reclock *
get_reclock()
{
	used_reclock++;
	return ( (reclock *) xmalloc(lock_len) );
}


klm_lockargs *
get_klm_lockargs()
{
	return ((struct klm_lockargs *) xmalloc(sizeof (struct klm_lockargs)));
}

klm_unlockargs *
get_klm_unlockargs()
{
	return ((struct klm_unlockargs *)xmalloc(sizeof (struct klm_unlockargs)));
}

klm_testargs *
get_klm_testargs()
{
	return ((struct klm_testargs *) xmalloc(sizeof (struct klm_testargs)));
}

klm_testrply *
get_klm_testrply()
{
	return ((struct klm_testrply *) xmalloc(sizeof (struct klm_testrply)));
}

klm_stat *
get_klm_stat()
{
	return ((struct klm_stat *) xmalloc(sizeof (struct klm_stat)));
}

nlm_lockargs *
get_nlm_lockargs()
{
	return ((struct nlm_lockargs *) xmalloc(sizeof (struct nlm_lockargs)));
}

nlm_unlockargs *
get_nlm_unlockargs()
{
	return ((struct nlm_unlockargs *)xmalloc(sizeof (struct nlm_unlockargs)));
}

nlm_cancargs *
get_nlm_cancargs()
{
	return ((struct nlm_cancargs *) xmalloc(sizeof (struct nlm_cancargs)));
}

nlm_testargs *
get_nlm_testargs()
{
	return ((struct nlm_testargs *) xmalloc(sizeof (struct nlm_testargs)));
}

nlm_testres *
get_nlm_testres()
{
	return ((struct nlm_testres *) xmalloc(sizeof (struct nlm_testres)));
}

nlm_res *
get_nlm_res()
{
	return ((struct nlm_res *) xmalloc(sizeof (struct nlm_res)));
}
