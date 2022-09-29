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
#ident	"$Header: getnetgrent.c,v 1.4.1.2.1.2 90/08/08 21:39:26 alexp Exp $"

/* 
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <netdb.h>

setnetgrent(grp)
	char *grp;
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;

        while ((vis_func = vis_nextserv(vis_func, VIS_SETNETGRENT))
                != VIS_DONE) {
                hp = (int)((*vis_func)(VIS_SETNETGRENT, grp));
		/*
		if ((hp != -1) && (hp != 0))
                        return(hp);
		*/
        }
        return(0);
}

endnetgrent()
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;

        while ((vis_func = vis_nextserv(vis_func, VIS_ENDNETGRENT))
                != VIS_DONE) {
                hp = (int)((*vis_func)(VIS_ENDNETGRENT, 0));
		/*
                if (hp != -1)
                        return(hp);
		*/
        }
        return(0);
}

getnetgrent(machinep, namep, domainp)
	char **machinep, **namep, **domainp;
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;
	Vis_gnetgrent_req req;

	req.r_machinep = machinep;
	req.r_namep = namep;
	req.r_domainp = domainp;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETNETGRENT))
                != VIS_DONE) {
                hp = (int)((*vis_func)(VIS_GETNETGRENT, &req));
		if ((hp != -1) && (hp != 0))
                        return(hp);
        }
        return(0);
}
