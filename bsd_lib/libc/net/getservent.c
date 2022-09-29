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
#ident	"$Header: getservent.c,v 1.4.1.2 90/05/07 20:52:52 wje Exp $"

#include <netdb.h>

setservent(f)
	int f;
{
	int hp;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_SETSERVENT))
                != VIS_DONE) {
                hp = (int)((*vis_func)(VIS_SETSERVENT, f));
                if (hp != -1)
                        return(hp);
        }
        return(0);
}

endservent()
{
	int p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_ENDSERVENT))
                != VIS_DONE) {
                p = (int)((*vis_func)(VIS_ENDSERVENT, 0));
                if (p != -1)
                        return(p);
        }
        return(0);
}

struct servent *
getservent()
{

	register struct servent *p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETSERVENT))
                != VIS_DONE) {
                p = (struct servent *)((*vis_func)(VIS_GETSERVENT, 0));
		if ((p != (struct servent *)-1) && (p != (struct servent *)0))
                        return(p);
        }
        return((struct servent *)0);
}


struct servent *
getservbyname(name, proto)
	char *name;
	char *proto;
{
	register struct servent *p;
	Vis_sbyname_req req;
        char *(*vis_func)() = VIS_FIRST_CALL;

	req.r_name = name;
	req.r_proto = proto;
        while ((vis_func = vis_nextserv(vis_func, VIS_GETSERVBYNAME))
                != VIS_DONE) {
                p = (struct servent *)((*vis_func)(VIS_GETSERVBYNAME, &req));
		if ((p != (struct servent *)-1) && (p != (struct servent *)0))
                        return(p);
        }
        return((struct servent *)0);
}

struct servent *
getservbyport(port, proto)
	int port;
	char *proto;
{
	register struct servent *p;
	Vis_sbyport_req req;
        char *(*vis_func)() = VIS_FIRST_CALL;

	req.r_port = port;
	req.r_proto = proto;
        while ((vis_func = vis_nextserv(vis_func, VIS_GETSERVBYPORT))
                != VIS_DONE) {
                p = (struct servent *)((*vis_func)(VIS_GETSERVBYPORT, &req));
		if ((p != (struct servent *)-1) && (p != (struct servent *)0))
                        return(p);
        }
        return((struct servent *)0);
}
