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
#ident	"$Header: hostinfo.c,v 1.1.2.2 90/05/09 15:42:37 wje Exp $"

#include <stdio.h>
#include <sys/param.h>

struct hostlist {
	char hostname[MAXHOSTNAMELEN + 1];
	struct hostlist *next;
};

static struct hostlist *Uphosts = NULL;
static struct hostlist *Downhosts = NULL;

extern char verify_hostup;

int
is_down(host)
	char *host;
{
	struct hostlist *cur_ent;

	cur_ent = Downhosts;
	while (cur_ent) {
		if (strcmp(cur_ent->hostname, host) == 0) {
			return 1;
		}
		cur_ent = cur_ent->next;
	}
	return 0;
}

int
is_up(host)
	char *host;
{
	struct hostlist *cur_ent;

	if (!verify_hostup) {
		return 1;
	}

	cur_ent = Uphosts;
	while (cur_ent) {
		if (strcmp(cur_ent->hostname, host) == 0) {
			return 1;
		}
		cur_ent = cur_ent->next;
	}

	/*
	 * Not sure if it's up or not.  Check.
	 */

	if (pinghost(host)) {
		cur_ent = (struct hostlist *)malloc(sizeof(struct hostlist));
		if (cur_ent) {
			strcpy(cur_ent->hostname, host);
			cur_ent->next = Uphosts;
			Uphosts = cur_ent;
		}
		return 1;
	}
	mark_down(host);
	return 0;
}

static int
pinghost(host)
	char *host;
{
	return 1;	/* cheap ping, might even be right ;-) */
}

mark_down(host)
	char *host;
{
	struct hostlist *cur_ent;

	cur_ent = (struct hostlist *)malloc(sizeof(struct hostlist));
	if (cur_ent) {
		strcpy(cur_ent->hostname, host);
		cur_ent->next = Downhosts;
		Downhosts = cur_ent;
	}
}
