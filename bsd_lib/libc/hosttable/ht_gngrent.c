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
#ident	"$Header: ht_gngrent.c,v 1.2.1.5 90/05/10 20:12:20 wje Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ctype.h>

#define	MAXALIASES	35

static char NETGRDB[] = "/etc/netgroup";
static FILE *netgrf = NULL;
static char line[BUFSIZ+1];
static char *any();
static char cur_group[256];
static char *cur_ptr;
static struct mem {
	char *m_buffer;
	struct mem *m_next;
} *memhead = NULL, *memcur = NULL;

char *
ht_setnetgrent(group)
	char *group;
{
	if (netgrf == NULL)
		netgrf = fopen(NETGRDB, "r" );
	else
		rewind(netgrf);
	strcpy(cur_group, group);
}

char *
ht_endnetgrent(a)
	int a;
{
	struct mem *i,*j;

	if (netgrf) {
		fclose(netgrf);
		netgrf = NULL;
	}
	if (memhead == NULL) return;
	for (i = memhead ; i->m_next->m_next ; j = i, i = i->m_next, free(j)) {
		free(i->m_buffer);
	}
	cur_ptr = NULL;
	memhead = memcur = (struct mem *)0;
}

char *
ht_getnetgrent(req)
	Vis_gnetgrent_req *req;
{
	char **machinep = req->r_machinep;
	char **userp = req->r_namep;
	char **domainp = req->r_domainp;
	char *p, *tmp;
	char *index();
	char *malloc();
	char *strsave();

	if (netgrf == NULL || cur_group == NULL)
		return (NULL);
	/* are we in an entry currently */
	if (!cur_ptr) {
	/* we aren't in an entry, we have to find it... */
again:
		if ((p = fgets(line, BUFSIZ, netgrf)) == NULL)
			return (NULL);
		if (*p == '#')
			goto again;
		for (tmp = p ; *tmp == ' ' || *tmp == '\t'; tmp++);
		if (*tmp == '\n')
			goto again;
		p = tmp;
		for (; *tmp != ' ' && *tmp != '\t'; tmp++);
		*tmp = '\0';
		if (strcmp(cur_group, p))
			goto again;
		cur_ptr = tmp + 1;
	}
	/* if we get here, we have a line for group cur_group */
	for (tmp = cur_ptr ; *tmp == ' ' || *tmp == '\t'; tmp++);
	if (*tmp == '\n')
		return(NULL);
	if (*tmp != '(') {
		fprintf(stderr,"Syntax Error in Net Group File\n");
		return(NULL);
	}
	tmp++;
	tmp = strsave(tmp, machinep, ',');
	tmp = strsave(tmp, userp, ',');
	tmp = strsave(tmp, domainp, ')');
	cur_ptr = tmp;
	return((char *)1);
}

static char *
strsave(tmp, ptr, ender)
	char *tmp;
	char **ptr;
	char ender;
{
	char *malloc();
	char *a;

	if (*(tmp) == ender) {
		*ptr = NULL;
		return(tmp + 1);
	}
	else {
		if ( (a = index(tmp, ender)) != NULL)
			*a = NULL;
		if (!memhead) {
			memhead = (struct mem *)malloc(sizeof(struct mem));
			memcur = memhead;
		}
		memcur->m_buffer = malloc(strlen(tmp) + 1);
		strcpy(memcur->m_buffer, tmp);
		*ptr = memcur->m_buffer;
		memcur->m_next = (struct mem*)malloc(sizeof(struct mem));
		memcur = memcur->m_next;
		memcur->m_next = 0;
		memcur->m_buffer = NULL;
		return(a + 1);
	}
}
