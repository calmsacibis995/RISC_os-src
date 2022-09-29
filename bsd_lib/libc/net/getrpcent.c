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
#ident	"$Header: getrpcent.c,v 1.5.1.2 90/05/07 20:52:18 wje Exp $"

#include <netdb.h>

struct rpcent *
getrpcbynumber(number)
	register int number;
{
	register struct rpcent *p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETRPCBYNUM))
		!= VIS_DONE) {
		p = (struct rpcent *)((*vis_func)(VIS_GETRPCBYNUM, number));
		if ((p != (struct rpcent *)-1) && (p != (struct rpcent *)0))
                        return(p);
        }
        return((struct rpcent *)0);
}

struct rpcent *
getrpcbyname(name)
	char *name;
{
	struct rpcent *p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETRPCBYNAME))
                != VIS_DONE) {
                p = (struct rpcent *)((*vis_func)(VIS_GETRPCBYNAME, name));
		if ((p != (struct rpcent *)-1) && (p != (struct rpcent *)0))
                        return(p);
        }
        return((struct rpcent *)0);
}

setrpcent(f)
	int f;
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;

        while ((vis_func = vis_nextserv(vis_func, VIS_SETRPCENT))
                != VIS_DONE) {
                hp = (int)((*vis_func)(VIS_SETRPCENT, f));
                if (hp != -1)
                        return(hp);
        }
        return(0);
}

endrpcent()
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;

        while ((vis_func = vis_nextserv(vis_func, VIS_ENDRPCENT))
                != VIS_DONE) {
                hp = (int)((*vis_func)(VIS_ENDRPCENT, 0));
                if (hp != -1)
                        return(hp);
        }
        return(0);
}

struct rpcent *
getrpcent()
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	struct rpcent *p;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETRPCENT))
                != VIS_DONE) {
                p = (struct rpcent *)((*vis_func)(VIS_GETRPCENT, 0));
		if ((p != (struct rpcent *)-1) && (p != (struct rpcent *)0))
                        return(p);
        }
        return((struct rpcent *)0);
}
