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
#ident	"$Header: innetgr.c,v 1.3.1.2 90/05/07 20:53:04 wje Exp $"

#include <netdb.h>

/* 
 * innetgr: test whether I'm in /etc/netgroup
 * 
 */

innetgr(grp, mach, nm, dom)
	char *grp;
	char *mach;
	char *nm;
	char *dom;
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;
	Vis_innetgr_req req;

	req.r_grp = grp;
	req.r_mach = mach;
	req.r_nm = nm;
	req.r_dom = dom;

        while ((vis_func = vis_nextserv(vis_func, VIS_INNETGR))
                != VIS_DONE) {
                hp = (int)((*vis_func)(VIS_INNETGR, &req));
                if (hp != -1)
                        return(hp);
        }
        return(0);
}
