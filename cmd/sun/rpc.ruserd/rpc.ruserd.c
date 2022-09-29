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
#ident	"$Header: rpc.ruserd.c,v 1.6.2.8 90/06/06 13:27:48 wje Exp $"

#define sgi	1

/*
 * Copyright (c) 1984 by Sun Microsystems, Inc.
 */

#include <rpc/rpc.h>
#include <utmp.h>
#ifdef sgi
#define	nonuser(buf)	(buf.ut_type != USER_PROCESS)
#endif
#include <rpcsvc/rusers.h>
#include <stdio.h>
#include <bsd/sys/socket.h>
#include <sys/stat.h>
#ifdef RISCOS
#include <bsd43/syslog.h>
#endif

#define	DIV60(t)	((t+30)/60)    /* x/60 rounded */
#define MAXINT 0x7fffffff
#define min(a,b) ((a) < (b) ? (a) : (b))

struct utmparr utmparr;
struct utmpidlearr utmpidlearr;
int cnt;
int rusers_service();

#define NLNTH 8			/* sizeof ut_name */

main()
{
	register SVCXPRT * transp;
	struct sockaddr_in addr;
	int len = sizeof(struct sockaddr_in);

#ifdef RISCOS
        openlog("rpc.ruserd",BSD43_LOG_PID,BSD43_LOG_DAEMON);
#endif
#ifdef DEBUG
	{
		int s;
		struct sockaddr_in addr;
		int len = sizeof(struct sockaddr_in);

		if ((s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) {
#ifdef RISCOS
			syslog(BSD43_LOG_ERR,"inet: socket - %m\n");
#else
			perror("inet: socket");
#endif
			return - 1;
		}
		if (bind(s, &addr, sizeof(addr)) < 0) {
#ifdef RISCOS
			syslog(BSD43_LOG_ERR,"bind - %m\n");
#else
			perror("bind");
#endif
			return - 1;
		}
		if (getsockname(s, &addr, &len) != 0) {
#ifdef RISCOS
			syslog(BSD43_LOG_ERR,"getsockname - %m\n");
#else
			perror("inet: getsockname");
#endif
			(void)close(s);
			return - 1;
		}
		pmap_unset(RUSERSPROG, RUSERSVERS_ORIG);
		pmap_set(RUSERSPROG, RUSERSVERS_ORIG, IPPROTO_UDP,
		    ntohs(addr.sin_port));
		pmap_unset(RUSERSPROG, RUSERSVERS_IDLE);
		pmap_set(RUSERSPROG, RUSERSVERS_IDLE, IPPROTO_UDP,
		    ntohs(addr.sin_port));
		if (dup2(s, 0) < 0) {
#ifdef RISCOS
			syslog(BSD43_LOG_ERR,"dup2 - %m\n");
#else
			perror("dup2");
#endif
			exit(1);
		}
	}
#endif	

	if (getsockname(0, &addr, &len) != 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"getsockname - %m\n");
#else
		perror("rstat: getsockname");
#endif
		exit(1);
	}
	if ((transp = svcudp_create(0)) == NULL) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"svc_rpc_udp_create: error\n");
#else
		fprintf(stderr, "svc_rpc_udp_create: error\n");
#endif
		exit(1);
	}
	if (!svc_register(transp, RUSERSPROG, RUSERSVERS_ORIG,
	    rusers_service,0)) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"svc_rpc_register: error\n");
#else
		fprintf(stderr, "svc_rpc_register: error\n");
#endif
		exit(1);
	}
	if (!svc_register(transp, RUSERSPROG, RUSERSVERS_IDLE,
	    rusers_service,0)) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"svc_rpc_register: error\n");
#else
		fprintf(stderr, "svc_rpc_register: error\n");
#endif
		exit(1);
	}
	svc_run();		/* never returns */
#ifdef RISCOS
	syslog(BSD43_LOG_ERR,"run_svc_rpc should never return\n");
#else
	fprintf(stderr, "run_svc_rpc should never return\n");
#endif
}

getutmp(all, idle)
	int all;		/* give all listings? */
	int idle;		/* get idle time? */
{
	struct utmp buf, **p;
	struct utmpidle **q, *console= NULL;
	int np=0, nq=0;
	int minidle;
	FILE *fp;
	char *file;
	char name[NLNTH];
	
	cnt = 0;
	file = "/etc/utmp";
	if ((fp = fopen(file, "r")) == NULL) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"can't open %s\n", file);
#else
		fprintf(stderr, "can't open %s\n", file);
#endif
		exit(1);
	};
	p = utmparr.uta_arr;
	q = utmpidlearr.uia_arr;
	while (fread(&buf, sizeof(buf), 1, fp) == 1) {
		if (buf.ut_line[0] == 0 || buf.ut_name[0] == 0 ||
		    buf.ut_type != USER_PROCESS)
			continue;
		/* 
		 * if tty[pqr]? and not remote, then skip it
		 */
		if (!all && nonuser(buf))
			continue;
		/* 
		 * need to free this
		 */
		if (idle) {
			*q = (struct utmpidle *)
			    malloc(sizeof(struct utmpidle));
			if (strncmp(buf.ut_line, "console",
			    strlen("console")) == 0) {
				console = *q;
				strncpy(name, buf.ut_name, NLNTH);
			    }
			bcopy(&buf, &((*q)->ui_utmp), sizeof(buf));
			(*q)->ui_idle = findidle(&buf);
#ifdef DEBUG
			printf("%-10s %-10s %-18s %s; idle %d",
			    buf.ut_line, buf.ut_name,
			    buf.ut_host, ctime(&buf.ut_time),
			    (*q)->ui_idle);
#endif
			q++;
			if (++nq >= MAXUSERS) break;
		}
		else {
			*p = (struct utmp *)malloc(sizeof(struct utmp));
			bcopy(&buf, *p, sizeof(buf));
#ifdef DEBUG
			printf("%-10s %-10s %-18s %s",
			    buf.ut_line, buf.ut_name,
			    buf.ut_host, ctime(&buf.ut_time));
#endif
			p++;
			if (++np >= MAXUSERS) break;
		}
		cnt++;
	}
	/* 
	 * if the console and the window pty's are owned by the same
	 * user, take the min of the idle times and associate
	 * it with the console
	 */
	if (idle && console) {
		minidle = MAXINT;
		rewind(fp);
		while (fread(&buf, sizeof(buf), 1, fp) == 1) {
			if (nonuser(buf)
			    && strncmp(buf.ut_name, name, NLNTH) == 0)
				minidle = min(findidle(&buf), minidle);
		}
		console->ui_idle = min(minidle, console->ui_idle);
	}
}

rusers_service(rqstp, transp)
	register struct svc_req *rqstp;
	register SVCXPRT *transp;
{
	switch (rqstp->rq_proc) {
	case 0:
		if (svc_sendreply(transp, xdr_void, 0)  == FALSE) {
#ifdef RISCOS
			syslog(BSD43_LOG_ERR, "err: rusersd - %m\n");
#else
			fprintf(stderr, "err: rusersd");
#endif
			exit(1);
		    }
		exit(0);
	case RUSERSPROC_NUM:
		utmparr.uta_arr = (struct utmp **)
		    malloc(MAXUSERS*sizeof(struct utmp *));
		getutmp(0, 0);
		if (!svc_sendreply(transp, xdr_u_long, &cnt))
#ifdef RISCOS
			syslog(BSD43_LOG_ERR,"svc_rpc_send_results");
#else
			perror("svc_rpc_send_results");
#endif
		free(utmparr.uta_arr);
		exit(0);
	case RUSERSPROC_NAMES:
	case RUSERSPROC_ALLNAMES:
		if (rqstp->rq_vers == RUSERSVERS_ORIG) {
			utmparr.uta_arr = (struct utmp **)
			    malloc(MAXUSERS*sizeof(struct utmp *));
			getutmp(rqstp->rq_proc == RUSERSPROC_ALLNAMES, 0);
			utmparr.uta_cnt = cnt;
			if (!svc_sendreply(transp, xdr_utmparr, &utmparr))
#ifdef RISCOS
				syslog(BSD43_LOG_ERR,"svc_rpc_send_results - %m");
#else
				perror("svc_rpc_send_results");
#endif
			svc_freeargs(transp, xdr_utmparr, &utmparr);
			free(utmparr.uta_arr);
			exit(0);
		}
		else {
			utmpidlearr.uia_arr = (struct utmpidle **)
			    malloc(MAXUSERS*sizeof(struct utmpidle *));
			getutmp(rqstp->rq_proc == RUSERSPROC_ALLNAMES, 1);
			utmpidlearr.uia_cnt = cnt;
			if (!svc_sendreply(transp, xdr_utmpidlearr,
			    &utmpidlearr))
#ifdef RISCOS
				syslog(BSD43_LOG_ERR,"svc_rpc_send_results - %m\n");
#else
				perror("svc_rpc_send_results");
#endif
			svc_freeargs(transp, xdr_utmpidlearr, &utmpidlearr);
			free(utmpidlearr.uia_arr);
			exit(0);
		}
	default: 
		svcerr_noproc(transp);
		exit(0);
	}
}

/* find & return number of minutes current tty has been idle */
findidle(up)
	struct utmp *up;
{
	time_t	now;
	struct stat stbuf;
	long lastaction, diff;
	char ttyname[20];

	strcpy(ttyname, "/dev/");
	strncat(ttyname, up->ut_line, sizeof(up->ut_line));
	stat(ttyname, &stbuf);
	time(&now);
	lastaction = stbuf.st_atime;
	diff = now - lastaction;
	diff = DIV60(diff);
	if (diff < 0) diff = 0;
	return(diff);
}
