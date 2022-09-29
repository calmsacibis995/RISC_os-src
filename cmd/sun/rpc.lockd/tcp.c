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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: tcp.c,v 1.2.1.2.1.1.1.2 90/11/17 11:59:37 beacker Exp $"
/*
* "@(#)tcp.c	1.1 88/08/05 NFSSRC4.0 1.8 88/02/07 Copyr 1984 Sun Micro"
*
* make tcp calls
*/
#include <stdio.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include <sys/time.h>
#ifdef RISCOS
#include <bsd43/sys/syslog.h>
#endif

extern int debug;
/*
 *  routine taken from new_calltcp.c;
 *  no caching is done!
 *  continueously calling if timeout;
 *  in case of error, print put error msg; this msg usually is to be
 *  thrown away
 */
int
call_tcp(host, prognum, versnum, procnum, inproc, in, outproc, out, tot )
        char *host;
        xdrproc_t inproc, outproc;
        char *in, *out;
	int tot;
{
        struct sockaddr_in server_addr;
	struct in_addr *get_addr_cache();
        enum clnt_stat clnt_stat;
        struct hostent *hp;
        struct timeval  tottimeout;
        register CLIENT *client;
        int socket = RPC_ANYSOCK;
 
        if ((hp = gethostbyname(host)) == NULL) {
		if (debug)
#ifdef RISCOS
			syslog(LOG_ERR, "RPC_UNKNOWNHOST\n");
#else
			printf( "RPC_UNKNOWNHOST\n");
#endif
		return((int) RPC_UNKNOWNHOST);
	}
        bcopy(hp->h_addr, (caddr_t)&server_addr.sin_addr, hp->h_length);      
        server_addr.sin_family = AF_INET;
        server_addr.sin_port =  0;

        tottimeout.tv_usec = 0;
        tottimeout.tv_sec = tot; 
        if ((client = clnttcp_create(&server_addr, prognum,
            versnum,  &socket, 0, 0)) == NULL)
		{
                clnt_pcreateerror("clnttcp_create");   /* RPC_PMAPFAILURE or RPC_SYSTEMERROR */
		return ((int) rpc_createerr.cf_stat);  /* return (svr_not_avail); */
		}
again:
        clnt_stat = clnt_call(client, procnum, inproc, in,
            outproc, out, tottimeout);
	if (clnt_stat != RPC_SUCCESS)  {
		if( clnt_stat == RPC_TIMEDOUT) {
			if(tot != 0) {
				if(debug)
#ifdef RISCOS
					syslog(LOG_ERR,"call_tcp timeout, retry\n");
#else
					printf("call_tcp timeout, retry\n");
#endif
				goto again;
			}
			/* if tot == 0, no reply is expected */
		}
		else {
			if(debug) {
				clnt_perrno(clnt_stat);
#ifdef RISCOS
				syslog(LOG_ERR,"\n");
#else
				fprintf(stderr, "\n");
#endif
			}
		}
	}
        /* should do cacheing, rather than always destroy */
        (void) close(socket);
        clnt_destroy(client);
        return (int) clnt_stat;
}

