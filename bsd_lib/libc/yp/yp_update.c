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
#ident	"$Header: yp_update.c,v 1.2.1.2 90/05/07 21:31:37 wje Exp $"
#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = 	"@(#)yp_update.c	1.4 88/07/27 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * 1.10 87/10/30 
 */

/*
 * Yellow Pages updater interface
 */
#include <stdio.h>
#include <rpc/rpc.h>
#ifdef SYSTYPE_BSD43
#include <netdb.h>
#include <sys/socket.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/netdb.h>
#include <bsd/sys/socket.h>
#endif
#include <rpcsvc/ypclnt.h>
#include <rpcsvc/ypupdate_prot.h>
#include <des_crypt.h>

#define WINDOW (60*60)
#define TOTAL_TIMEOUT	300	

/*
 * Turn off debugging 
 */
#define debugging 0
#define debug(msg)


yp_update(domain, map, op, key, keylen, data, datalen)
	char *domain;
	char *map;
	unsigned op;
	char *key;
	int keylen;
	char *data;
	int datalen;
{
	struct ypupdate_args args;
	u_int rslt;	
	struct timeval total;
	CLIENT *client;
	struct sockaddr_in server_addr;
	char *ypmaster;
	char ypmastername[MAXNETNAMELEN+1];
	enum clnt_stat stat;
	u_int proc;

	switch (op) {
	case YPOP_DELETE:
		proc = YPU_DELETE;
		break;	
	case YPOP_INSERT:
		proc = YPU_INSERT;
		break;	
	case YPOP_CHANGE:
		proc = YPU_CHANGE;
		break;	
	case YPOP_STORE:
		proc = YPU_STORE;
		break;	
	default:
		return(YPERR_BADARGS);
	}
	if (yp_master(domain, map, &ypmaster) != 0) {
		debug("no master found");
		return (YPERR_BADDB);	
	}

	client = clnt_create(ypmaster, YPU_PROG, YPU_VERS, "tcp");
	if (client == NULL) {
		if (debugging) {
			clnt_pcreateerror("client create failed");
		}
		free(ypmaster);
		return (YPERR_RPC);
	}

	if (! host2netname(ypmastername, ypmaster, domain)) {
		free(ypmaster);
		return (YPERR_BADARGS);
	}
	free(ypmaster);
	clnt_control(client, CLGET_SERVER_ADDR, &server_addr);
	client->cl_auth = authdes_create(ypmastername, WINDOW, 
						 &server_addr, NULL);
	if (client->cl_auth == NULL) {
		debug("auth create failed");
		clnt_destroy(client);
		return (YPERR_RPC);	
	}

	args.mapname = map;	
	args.key.yp_buf_len = keylen;
	args.key.yp_buf_val = key;
	args.datum.yp_buf_len = datalen;
	args.datum.yp_buf_val = data;

	total.tv_sec = TOTAL_TIMEOUT; total.tv_usec = 0;
	clnt_control(client, CLSET_TIMEOUT, &total);
	stat = clnt_call(client, proc,
		xdr_ypupdate_args, &args,
		xdr_u_int, &rslt, total);

	if (stat != RPC_SUCCESS) {
		debug("ypu call failed");
		if (debugging) clnt_perror(client, "ypu call failed");
		rslt = YPERR_RPC;
	}
	auth_destroy(client->cl_auth);
	clnt_destroy(client);
	return (rslt);
}
