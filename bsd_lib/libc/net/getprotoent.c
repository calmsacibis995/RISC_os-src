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
#ident	"$Header: getprotoent.c,v 1.4.1.2 90/05/07 20:52:06 wje Exp $"

#include <netdb.h>

setprotoent(f)
	int f;
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;

        while ((vis_func = vis_nextserv(vis_func, VIS_SETPROTOENT))
                != VIS_DONE) {
                hp = (int)((*vis_func)(VIS_SETPROTOENT, f));
		/*
                if (hp != -1)
                        return(hp);
		*/
        }
        return(0);
}

endprotoent()
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	int hp;

        while ((vis_func = vis_nextserv(vis_func, VIS_ENDNETENT))
                != VIS_DONE) {
                hp = (int)((*vis_func)(VIS_ENDNETENT, 0));
		/*
                if (hp != -1)
                        return(hp);
		*/
        }
        return(0);
}

struct protoent *
getprotoent()
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	struct protoent *p;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETPROTOENT))
                != VIS_DONE) {
                p = (struct protoent *)((*vis_func)(VIS_GETPROTOENT, 0));
		if ((p != (struct protoent *)-1) && (p != (struct protoent *)0))
                        return(p);
        }
        return((struct protoent *)0);
}
