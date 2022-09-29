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
#ident	"$Header: ht_service.c,v 1.2.1.5 90/05/07 20:47:54 wje Exp $"

#include <netdb.h>

char *ht_gethostbyname(), *ht_gethostbyaddr(), *ht_sethostent();
char *ht_endhostent(), *ht_gethostent();;
char *ht_getnetbyaddr(), *ht_getnetbyname();
char *ht_getnetent(), *ht_setnetent(), *ht_endnetent();
char *ht_getnetgrent(), *ht_setnetgrent(), *ht_endnetgrent(), *ht_innetgr();
char *ht_getprotobynumber(), *ht_getprotobyname();
char *ht_setprotoent(),*ht_endprotoent(),*ht_getprotoent();
char *ht_getrpcbynumber(), *ht_getrpcbyname();
char *ht_setrpcent(),*ht_endrpcent(),*ht_getrpcent();
char *ht_getservbyport(), *ht_getservbyname();
char *ht_setservent(),*ht_endservent(),*ht_getservent();
char *ht_getgrgid(), *ht_getgrnam();
char *ht_setgrent(),*ht_endgrent(),*ht_getgrent();
char *ht_getpwuid(), *ht_getpwnam();
char *ht_setpwent(),*ht_endpwent(),*ht_getpwent();
char  *ht_getpw();

struct ht_services {
	unsigned int s_request;
	char *(*s_func)();
} Ht_services[] = {
	VIS_GHOSTBYNAME,	ht_gethostbyname,
	VIS_GHOSTBYADDR,	ht_gethostbyaddr,
	VIS_SETHOSTENT,		ht_sethostent,
	VIS_ENDHOSTENT,		ht_endhostent,
	VIS_GETHOSTENT,		ht_gethostent,
	VIS_GETGRBYNAME,	ht_getgrnam,
	VIS_GETGRBYGID,		ht_getgrgid,
	VIS_SETGRENT,		ht_setgrent,
	VIS_ENDGRENT,		ht_endgrent,
	VIS_GETGRENT,		ht_getgrent,
	VIS_GETPWBYNAME,	ht_getpwnam,
	VIS_GETPWBYUID,		ht_getpwuid,
	VIS_SETPWENT,		ht_setpwent,
	VIS_ENDPWENT,		ht_endpwent,
	VIS_GETPWENT,		ht_getpwent,
	VIS_GETPW,		ht_getpw,
	VIS_GETNETBYADDR,	ht_getnetbyaddr,
	VIS_GETNETBYNAME,	ht_getnetbyname,
	VIS_GETNETENT,		ht_getnetent,
	VIS_ENDNETENT,		ht_endnetent,
	VIS_SETNETENT,		ht_setnetent,
	VIS_GETNETGRENT,	ht_getnetgrent,
	VIS_ENDNETGRENT,	ht_endnetgrent,
	VIS_SETNETGRENT,	ht_setnetgrent,
	VIS_INNETGR,		ht_innetgr, 
	VIS_GETPROTOBYNUM,	ht_getprotobynumber,
	VIS_GETPROTOBYNAME,	ht_getprotobyname,
	VIS_GETPROTOENT,	ht_getprotoent,
	VIS_ENDPROTOENT,	ht_endprotoent,
	VIS_SETPROTOENT,	ht_setprotoent,
	VIS_GETRPCBYNUM,	ht_getrpcbynumber,
	VIS_GETRPCBYNAME,	ht_getrpcbyname,
	VIS_GETRPCENT,		ht_getrpcent,
	VIS_ENDRPCENT,		ht_endrpcent,
	VIS_SETRPCENT,		ht_setrpcent,
	VIS_GETSERVBYPORT,	ht_getservbyport,
	VIS_GETSERVBYNAME,	ht_getservbyname,
	VIS_GETSERVENT,		ht_getservent,
	VIS_ENDSERVENT,		ht_endservent,
	VIS_SETSERVENT,		ht_setservent,
	0,			VIS_DONE
};

char *
ht_service(request, arg)
	unsigned int request;
	char *arg;			/* an opaque cookie */
{
	int i;

	for (i = 0 ; Ht_services[i].s_request ; i++)
		if (Ht_services[i].s_request == request)
			break;
	if (!Ht_services[i].s_request)
		return((char *)-1);
	else
		return((*(Ht_services[i].s_func))(arg));
} /* ht_service() */
