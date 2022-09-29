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
#ident	"$Header: yp_service.c,v 1.2.1.3 90/05/07 21:31:31 wje Exp $"
/* ------------------------------------------------------------------ */
/* | Copyrigyp Unpublished, MIPS Computer Systems, Inc.  All Rigyps | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

#include <netdb.h>

char *yp_gethostbyname(), *yp_gethostbyaddr(), *yp_sethostent();
char *yp_endhostent(), *yp_gethostent();;
char *yp_getnetbyaddr(), *yp_getnetbyname();
char *yp_getnetent(), *yp_setnetent(), *yp_endnetent();
char *yp_getnetgrent(), *yp_setnetgrent(), *yp_endnetgrent(), *yp_innetgr();
char *yp_getprotobynumber(), *yp_getprotobyname();
char *yp_setprotoent(),*yp_endprotoent(),*yp_getprotoent();
char *yp_getrpcbynumber(), *yp_getrpcbyname();
char *yp_setrpcent(),*yp_endrpcent(),*yp_getrpcent();
char *yp_getservbyport(), *yp_getservbyname();
char *yp_setservent(),*yp_endservent(),*yp_getservent();
char *yp_getgrgid(), *yp_getgrnam();
char *yp_setgrent(),*yp_endgrent(),*yp_getgrent(),*yp_fgetgrent();
char *yp_getpwuid(), *yp_getpwnam();
char *yp_setpwent(),*yp_endpwent(),*yp_getpwent();

struct yp_services {
	unsigned int s_request;
	char *(*s_func)();
} Yp_services[] = {
	VIS_GHOSTBYNAME,	yp_gethostbyname,
	VIS_GHOSTBYADDR,	yp_gethostbyaddr,
	VIS_SETHOSTENT,		yp_sethostent,
	VIS_ENDHOSTENT,		yp_endhostent,
	VIS_GETHOSTENT,		yp_gethostent,
	VIS_GETGRBYNAME,	yp_getgrnam,
	VIS_GETGRBYGID,		yp_getgrgid,
	VIS_SETGRENT,		yp_setgrent,
	VIS_ENDGRENT,		yp_endgrent,
	VIS_GETGRENT,		yp_getgrent,
	VIS_FGETGRENT,		yp_fgetgrent,
	VIS_GETPWBYNAME,	yp_getpwnam,
	VIS_GETPWBYUID,		yp_getpwuid,
	VIS_SETPWENT,		yp_setpwent,
	VIS_ENDPWENT,		yp_endpwent,
	VIS_GETPWENT,		yp_getpwent,
	VIS_GETNETBYADDR,	yp_getnetbyaddr,
	VIS_GETNETBYNAME,	yp_getnetbyname,
	VIS_GETNETENT,		yp_getnetent,
	VIS_ENDNETENT,		yp_endnetent,
	VIS_SETNETENT,		yp_setnetent,
	VIS_INNETGR,		yp_innetgr,
	VIS_GETNETGRENT,	yp_getnetgrent,
	VIS_ENDNETGRENT,	yp_endnetgrent,
	VIS_SETNETGRENT,	yp_setnetgrent,
	VIS_GETPROTOBYNUM,	yp_getprotobynumber,
	VIS_GETPROTOBYNAME,	yp_getprotobyname,
	VIS_GETPROTOENT,	yp_getprotoent,
	VIS_ENDPROTOENT,	yp_endprotoent,
	VIS_SETPROTOENT,	yp_setprotoent,
	VIS_GETRPCBYNUM,	yp_getrpcbynumber,
	VIS_GETRPCBYNAME,	yp_getrpcbyname,
	VIS_GETRPCENT,		yp_getrpcent,
	VIS_ENDRPCENT,		yp_endrpcent,
	VIS_SETRPCENT,		yp_setrpcent,
	VIS_GETSERVBYPORT,	yp_getservbyport,
	VIS_GETSERVBYNAME,	yp_getservbyname,
	VIS_GETSERVENT,		yp_getservent,
	VIS_ENDSERVENT,		yp_endservent,
	VIS_SETSERVENT,		yp_setservent,
	0,			VIS_DONE
};

char *
yp_service(request, arg)
	unsigned int request;
	char *arg;			/* an opaque cookie */
{
	int i;

	for (i = 0 ; Yp_services[i].s_request ; i++)
		if (Yp_services[i].s_request == request)
			break;
	if (!Yp_services[i].s_request)
		return((char *)-1);
	else
		return((*(Yp_services[i].s_func))(arg));
} /* yp_service() */
