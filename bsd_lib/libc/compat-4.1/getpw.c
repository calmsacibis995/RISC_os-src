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
#ident	"$Header: getpw.c,v 1.1.2.4 90/05/10 20:11:52 wje Exp $"

/*LINTLIBRARY*/
#include <stdio.h>
#include <ctype.h>
#include <netdb.h>

extern void rewind();
extern FILE *fopen();

static FILE *pwf;

int
getpw(uid, buf)
int	uid;
char	buf[];
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	Vis_getpw_req req;
        int p;

	req.r_uid = uid;
	req.r_buffer = buf;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETPW))
                != VIS_DONE) {
                p = (int)((*vis_func)(VIS_GETPW, &req));
        }
        return(p);
}
