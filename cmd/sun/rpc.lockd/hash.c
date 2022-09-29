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
/* 	"$Header: hash.c,v 1.2.1.2.1.1.1.2 90/11/17 11:50:57 beacker Exp $"

/* @(#)nfs.cmds:nfs/lockd/hash.c 1.2 */

/*
 * hash.c
 * routines to handle insertion, deletion of hashed monitor, file entries
 */
#include "prot_lock.h"

#define MAX_HASHSIZE 100

extern int debug;

int HASH_SIZE;

typedef struct lm_vnode cache_fp;
typedef struct lm_vnode cache_me;

cache_fp *table_fp[MAX_HASHSIZE];
cache_me *table_me[MAX_HASHSIZE];

/*
 * find_me returns the cached entry;
 * it returns NULL if not found;
 */
struct lm_vnode *
find_me(svr)
	char *svr;
{
	cache_me *cp;

	if (debug)
		(void) printf("enter find_me()...\n");

	cp = table_me[hash(svr)];
	while ( cp != NULL) {
		if (strcmp(cp->svr, svr) == 0) {
			/* found */
			return (cp);
		}
		cp = cp->next;
	}
	return (NULL);
}

void
insert_me(mp)
	struct lm_vnode *mp;
{
	int h;

	if (debug)
		printf("enter insert_me()...\n");

	h = hash(mp->svr);
	mp->next = table_me[h];
	table_me[h] = mp;
}
