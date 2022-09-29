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
#ident	"$Header: sethostent.c,v 1.1.1.3 90/05/07 20:54:33 wje Exp $"

#include <sys/types.h>
#include <netdb.h>


sethostent(f)
	int f;
{
	char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;

	while ((vis_func = vis_nextserv(vis_func, VIS_SETHOSTENT))
		!= VIS_DONE) {
		hp = (int)((*vis_func)(VIS_SETHOSTENT, f));
		if (hp != -1)
			return(hp);
	}
	return(0);
}

endhostent()
{
	char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;

	while ((vis_func = vis_nextserv(vis_func, VIS_ENDHOSTENT))
		!= VIS_DONE) {
		(void)((*vis_func)(VIS_ENDHOSTENT, 0));
	}
	return(0);
}

sethostfile(name)
char *name;
{
#ifdef lint
name = name;
#endif
}
