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
#ident	"$Header: gethostnamadr.c,v 1.2.1.2 90/05/07 20:51:31 wje Exp $"

/*
 * Copyright (c) 1985, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Revistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#include <netdb.h>
#include <ctype.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#ifdef SYSTYPE_BSD43
#include <netinet/in.h>
#include <sys/socket.h>
#else
#ifdef SYSTYPE_SYSV
#include <bsd/netinet/in.h>
#include <bsd/sys/socket.h>
#endif
#endif

int h_errno;
extern errno;

struct hostent *
gethostbyname(name)
	char *name;
{
	register char *cp;
	struct hostent *hp;
	char *(*vis_func)() = VIS_FIRST_CALL;

	/*
	 * visallow names consisting only of digits/dots, unless
	 * they end in a dot.
	 */
	if (isdigit(name[0]))
		for (cp = name;; ++cp) {
			if (!*cp) {
				if (*--cp == '.')
					break;
				h_errno = HOST_NOT_FOUND;
				return ((struct hostent *) NULL);
			}
			if (!isdigit(*cp) && *cp != '.') 
				break;
		}
	
	while ((vis_func = vis_nextserv(vis_func, VIS_GHOSTBYNAME))
		!= VIS_DONE) {
		hp = (struct hostent *)((*vis_func)(VIS_GHOSTBYNAME, name));
		if ((hp != (struct hostent *)-1) && (hp != (struct hostent *)0))
			return(hp);
	}
	return(NULL);
}

struct hostent *
gethostbyaddr(addr, len, type)
	char *addr;
	int len, type;
{
	register struct hostent *hp;
	char *(*vis_func)() = VIS_FIRST_CALL;
	Vis_hbyaddr_req req;
	
	if (type != AF_INET)
		return ((struct hostent *) NULL);

	req.r_addr = addr;
	req.r_len = len;
	req.r_type = type;

	while ((vis_func = vis_nextserv(vis_func, VIS_GHOSTBYADDR))
		!= VIS_DONE) {
		hp = (struct hostent *)((*vis_func)(VIS_GHOSTBYADDR, &req));
		if ((hp != (struct hostent *)-1) && (hp != (struct hostent *)0))
			return(hp);
	}
	return(NULL);
}

struct hostent *
gethostent()
{
	register struct hostent *hp;
	char *(*vis_func)() = VIS_FIRST_CALL;
	
	while ((vis_func = vis_nextserv(vis_func, VIS_GETHOSTENT))
		!= VIS_DONE) {
		hp = (struct hostent *)((*vis_func)(VIS_GETHOSTENT, 0));
		if ((hp != (struct hostent *)-1) && (hp != (struct hostent *)0))
			return(hp);
	}
	return(NULL);
}
