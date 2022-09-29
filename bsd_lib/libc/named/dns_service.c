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
#ident	"$Header: dns_service.c,v 1.2.1.2 90/05/07 20:50:08 wje Exp $"

#include <netdb.h>

char *dns_gethostbyname(), *dns_gethostbyaddr(), *dns_sethostent();
char *dns_endhostent();

struct dns_services {
	unsigned int s_request;
	char *(*s_func)();
} Dns_services[] = {
	VIS_GHOSTBYNAME,	dns_gethostbyname,
	VIS_GHOSTBYADDR,	dns_gethostbyaddr,
	VIS_SETHOSTENT,		dns_sethostent,
	VIS_ENDHOSTENT,		dns_endhostent,
	0,			VIS_DONE
};

char *
dns_service(request, arg)
	unsigned int request;
	char *arg;			/* an opaque cookie */
{
	int i;

	for (i = 0 ; Dns_services[i].s_request ; i++)
		if (Dns_services[i].s_request == request)
			break;
	if (!Dns_services[i].s_request)
		return((char *)-1);
	else
		return((*(Dns_services[i].s_func))(arg));
} /* dns_service() */
