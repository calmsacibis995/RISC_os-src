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
#ident	"$Header: snmp.c,v 1.1.1.2 90/05/09 17:03:31 wje Exp $"
/*
 *   CENTER FOR THEORY AND SIMULATION IN SCIENCE AND ENGINEERING
 *			CORNELL UNIVERSITY
 *
 *      Portions of this software may fall under the following
 *      copyrights: 
 *
 *	Copyright (c) 1983 Regents of the University of California.
 *	All rights reserved.  The Berkeley software License Agreement
 *	specifies the terms and conditions for redistribution.
 *
 *  GATED - based on Kirton's EGP, UC Berkeley's routing daemon (routed),
 *	    and DCN's HELLO routing Protocol.
 */

#ifndef	lint
static char *rcsid = "$Header: snmp.c,v 1.1.1.2 90/05/09 17:03:31 wje Exp $";
#endif	not lint

#include "include.h"
#include "sys/stat.h"
#include "snmp.h"

void ipRouteMetric1();
void ipRouteMetric2();
void ipRouteMetric3();
void ipRouteMetric4();
void ipRouteNextHop();
void ipRouteType();
void ipRouteProto();
void ipRouteAge();
void egpInMsgs();
void egpInErrors();
void egpOutMsgs();
void egpOutErrors();
void egpNeighState();
void egpNeighAddr();
struct egpngh *snmp_lookup_ngp();

int client_pipe;

/*
 *	Process an incoming request from SNMPD.
 */
snmpin(from, size, pkt)
        struct sockaddr *from;
        int size;
        char *pkt;
{

	struct snmpmsg * snmppkt = (struct snmpmsg *)pkt;
	struct snmpmsg msg;
	int cnt;

	msg.type = snmppkt->type;
	msg.index = snmppkt->index;

	switch (snmppkt->type) {
	case SNMP_IPROUTEMETRIC1:
		ipRouteMetric1 ((char *) &snmppkt->index, &msg);
		break;

	case SNMP_IPROUTEMETRIC2:
		ipRouteMetric2 ((char *) &snmppkt->index, &msg);
		break;

	case SNMP_IPROUTEMETRIC3:
		ipRouteMetric3 ((char *) &snmppkt->index, &msg);
		break;

	case SNMP_IPROUTEMETRIC4:
		ipRouteMetric4 ((char *) &snmppkt->index, &msg);
		break;

	case SNMP_IPROUTENEXTHOP:
		ipRouteNextHop ((char *) &snmppkt->index, &msg);
		break;

	case SNMP_IPROUTETYPE:
		ipRouteType ((char *) &snmppkt->index, &msg);
		break;

	case SNMP_IPROUTEPROTO:
		ipRouteProto ((char *) &snmppkt->index, &msg);
		break;

	case SNMP_IPROUTEAGE:
		ipRouteAge ((char *) &snmppkt->index, &msg);
		break;

	case SNMP_EGPINMSGS:
		egpInMsgs (&msg);
		break;

	case SNMP_EGPINERRORS:
		egpInErrors (&msg);
		break;

	case SNMP_EGPOUTMSGS:
		egpOutMsgs (&msg);
		break;

	case SNMP_EGPOUTERRORS:
		egpOutErrors (&msg);
		break;

	case SNMP_EGPNEIGHSTATE:
		egpNeighState ((char *) &snmppkt->index, &msg);
		break;

	case SNMP_EGPNEIGHADDR:
		egpNeighAddr ((char *) &snmppkt->index, &msg);
		break;

	default:
		msg.type = SNMP_ERROR;
	}

	if ((cnt = write (client_pipe, &msg, sizeof (msg))) != sizeof (msg)) {
		printf ("gated: write failed %d.\n", cnt);
	}
}

/*
 *  Register all of our supported variables with SNMPD.
 */
register_snmp_vars()
{
}


void ipRouteMetric1(src, msg)
char *src;
struct snmpmsg * msg;
{
	int metric;
	struct sockaddr_in reqdst;
	struct rt_entry *grte;
	
	bzero((char *)&reqdst, sizeof(reqdst));
	reqdst.sin_family = AF_INET;
	bcopy(src, (char *)&reqdst.sin_addr.s_addr, sizeof(u_long));
	grte = rt_lookup((int)INTERIOR|(int)EXTERIOR, &reqdst);
	if (grte == NULL) {
		msg->type = SNMP_ERROR;
		return;
	}
	switch (grte->rt_proto) {
		case RTPROTO_RIP:
		case RTPROTO_DIRECT:
		case RTPROTO_DEFAULT:
		case RTPROTO_REDIRECT:
			metric = mapmetric(HELLO_TO_RIP, grte->rt_metric);
			break;
		case RTPROTO_EGP:
		case RTPROTO_KERNEL:
			metric = mapmetric(HELLO_TO_EGP, grte->rt_metric);
			break;
		case RTPROTO_HELLO:
			metric = grte->rt_metric;
			break;
		default:
			metric = mapmetric(HELLO_TO_RIP, grte->rt_metric);
			break;
	}
	msg->type = SNMP_IPROUTEMETRIC1;
	msg->value = metric;
	return;
};


void ipRouteMetric2(src, msg)
char *src;
struct snmpmsg * msg;
{
	struct sockaddr_in reqdst;
	struct rt_entry *grte;
	
	bzero((char *)&reqdst, sizeof(reqdst));
	reqdst.sin_family = AF_INET;
	bcopy(src, (char *)&reqdst.sin_addr.s_addr, sizeof(u_long));
	grte = rt_lookup((int)INTERIOR|(int)EXTERIOR, &reqdst);
	if (grte == NULL) {
		msg->type = SNMP_ERROR;
		return;
	}

	msg->type = SNMP_IPROUTEMETRIC2;
	msg->value = -1;
	return;
}

void ipRouteMetric3(src, msg)
char *src;
struct snmpmsg * msg;
{
	struct sockaddr_in reqdst;
	struct rt_entry *grte;
	
	bzero((char *)&reqdst, sizeof(reqdst));
	reqdst.sin_family = AF_INET;
	bcopy(src, (char *)&reqdst.sin_addr.s_addr, sizeof(u_long));
	grte = rt_lookup((int)INTERIOR|(int)EXTERIOR, &reqdst);
	if (grte == NULL) {
		msg->type = SNMP_ERROR;
		return;
	}

	msg->type = SNMP_IPROUTEMETRIC3;
	msg->value = -1;
	return;
}

void ipRouteMetric4(src, msg)
char *src;
struct snmpmsg * msg;
{
	struct sockaddr_in reqdst;
	struct rt_entry *grte;
	
	bzero((char *)&reqdst, sizeof(reqdst));
	reqdst.sin_family = AF_INET;
	bcopy(src, (char *)&reqdst.sin_addr.s_addr, sizeof(u_long));
	grte = rt_lookup((int)INTERIOR|(int)EXTERIOR, &reqdst);
	if (grte == NULL) {
		msg->type = SNMP_ERROR;
		return;
	}

	msg->type = SNMP_IPROUTEMETRIC4;
	msg->value = -1;
	return;
}

void ipRouteNextHop(src, msg)
char *src;
struct snmpmsg * msg;
{
	struct sockaddr_in reqdst;
	struct rt_entry *grte;
	
	bzero((char *)&reqdst, sizeof(reqdst));
	reqdst.sin_family = AF_INET;
	bcopy(src, (char *)&reqdst.sin_addr.s_addr, sizeof(u_long));
	grte = rt_lookup((int)INTERIOR|(int)EXTERIOR, &reqdst);
	if (grte == NULL) {
		msg->type = SNMP_ERROR;
	} else {
		msg->type = SNMP_IPROUTENEXTHOP;
		msg->value = sock_inaddr(&grte->rt_router).s_addr;
	}

	return;
}

void ipRouteType(src, msg)
char *src;
struct snmpmsg * msg;
{
	int type;
	struct sockaddr_in reqdst;
	struct rt_entry *grte;
	
	bzero((char *)&reqdst, sizeof(reqdst));
	reqdst.sin_family = AF_INET;
	bcopy(src, (char *)&reqdst.sin_addr.s_addr, sizeof(u_long));
	grte = rt_lookup((int)INTERIOR|(int)EXTERIOR, &reqdst);
	if (grte == NULL) {
		msg->type = SNMP_ERROR;
		return;
	}
	if (grte->rt_metric >= DELAY_INFINITY) {
		type = 2;
	} else {
		switch (grte->rt_proto) {
			case RTPROTO_DIRECT:
				type = 3;
				break;
			default:
				type = 4;
		}
	}

	msg->type = SNMP_IPROUTETYPE;
	msg->value = type;
	return;
};


void ipRouteProto(src, msg)
char *src;
struct snmpmsg * msg;
{
	int proto;
	struct sockaddr_in reqdst;
	struct rt_entry *grte;
	
	bzero((char *)&reqdst, sizeof(reqdst));
	reqdst.sin_family = AF_INET;
	bcopy(src, (char *)&reqdst.sin_addr.s_addr, sizeof(u_long));
	grte = rt_lookup((int)INTERIOR|(int)EXTERIOR, &reqdst);
	if (grte == NULL) {
		msg->type = SNMP_ERROR;
		return;
	}
	if (grte->rt_state & RTS_STATIC) {
		proto = 2;
	} else {
		switch (grte->rt_proto) {
			case RTPROTO_RIP:
				proto = 8;
				break;
			case RTPROTO_HELLO:
				proto = 7;
				break;
			case RTPROTO_EGP:
				proto = 5;
				break;
			case RTPROTO_DIRECT:
				proto = 1;
				break;
			case RTPROTO_REDIRECT:
				proto = 4;
				break;
			case RTPROTO_DEFAULT:
				proto = 1;
				break;
			default:
				proto = 1;
		}
	}

	msg->type = SNMP_IPROUTEPROTO;
	msg->value = proto;
	return;
};


void ipRouteAge(src, msg)
char *src;
struct snmpmsg * msg;
{
	struct sockaddr_in reqdst;
	struct rt_entry *grte;
	
	bzero((char *)&reqdst, sizeof(reqdst));
	reqdst.sin_family = AF_INET;
	bcopy(src, (char *)&reqdst.sin_addr.s_addr, sizeof(u_long));
	grte = rt_lookup((int)INTERIOR|(int)EXTERIOR, &reqdst);
	if (grte == NULL) {
		msg->type = SNMP_ERROR;
	} else {
		msg->type = SNMP_IPROUTEAGE;
		msg->value = grte->rt_timer;
	}

	return;
};


void egpInMsgs(msg)
struct snmpmsg * msg;
{

	if (!(doing_egp)) {
		msg->type = SNMP_ERROR;
	} else {
		msg->type = SNMP_EGPINMSGS;
		msg->value = egp_stats.inmsgs;
	}

	return;
};


void egpInErrors(msg)
struct snmpmsg * msg;
{
	
	if (!(doing_egp)) {
		msg->type = SNMP_ERROR;
	} else {
		msg->type = SNMP_EGPINERRORS;
		msg->value = egp_stats.inerrors;
	}

	return;
};


void egpOutMsgs(msg)
struct snmpmsg * msg;
{
	if (!(doing_egp)) {
		msg->type = SNMP_ERROR;
	} else {
		msg->type = SNMP_EGPOUTMSGS;
		msg->value = egp_stats.outmsgs;
	}

	return;
};


void egpOutErrors(msg)
struct snmpmsg * msg;
{
	
	if (!(doing_egp)) {
		msg->type = SNMP_ERROR;
	} else {
		msg->type = SNMP_EGPOUTERRORS;
		msg->value = egp_stats.outerrors;
	}

	return;
};



void egpNeighState(src, msg)
char * src;
struct snmpmsg * msg;
{
	struct egpngh *ngp;
	
	if (!(doing_egp)) {
		msg->type = SNMP_ERROR;
	} else if ( (ngp = snmp_lookup_ngp(src, msg->next)) == 0 ) {
		msg->type = SNMP_ERROR;
	} else {
		msg->type = SNMP_EGPNEIGHSTATE;
		msg->value = ngp->ng_state + 1;
		msg->index = ngp->ng_addr.s_addr;
	}

	return;
}


void egpNeighAddr(src, msg)
char * src;
struct snmpmsg * msg;
{
	struct egpngh *ngp;
	
	if (!(doing_egp)) {
		msg->type = SNMP_ERROR;
	} else if ( (ngp = snmp_lookup_ngp(src, msg->next)) == 0 ) {
		msg->type = SNMP_ERROR;
	} else {
		msg->type = SNMP_EGPNEIGHADDR;
		msg->value = ngp->ng_addr.s_addr;
		msg->index = ngp->ng_addr.s_addr;
	}

	return;
}


struct egpngh *
snmp_lookup_ngp(src, next)
char *src;
int next;
{
	struct sockaddr_in reqdst;
	struct egpngh *ngp = egpngh;
	
	bzero((char *)&reqdst, sizeof(reqdst));
	reqdst.sin_family = AF_INET;
	bcopy(src, (char *)&reqdst.sin_addr.s_addr, sizeof(u_long));

	if (reqdst.sin_addr.s_addr != (u_long)DEFAULTNET) {
		for (; ngp; ngp = ngp->ng_next) {
			if (ngp->ng_addr.s_addr == reqdst.sin_addr.s_addr) {
				break;
			}
		}
	} 
	if (ngp && next) {
		ngp = ngp->ng_next;
	}
	return(ngp);
}

openserverpipe()
{
	snmp_socket = open(ServerPipeName, O_RDWR);
	if (snmp_socket < 0) {
		(void) printf("gated: open of %s", ServerPipeName);
	}
}

catchserverpipe()
{
	close(snmp_socket);
	openserverpipe();
	signal(SIGPIPE, catchserverpipe);
}

openclientpipe()
{
	client_pipe = open(ClientPipeName, O_RDWR);
	if (client_pipe < 0) {
		(void) printf("gated: open of %s", ClientPipeName);
	}
}

catchclientpipe()
{
	close(client_pipe);
	openclientpipe();
	signal(SIGPIPE, catchclientpipe);
}

int
snmp_init()
{

	(void) unlink (ServerPipeName);
	(void) unlink (ClientPipeName);

	if (mknod(ServerPipeName, S_IFIFO|0666, 0) < 0) {
		(void) printf("gated: mknod of %s", ServerPipeName);
		return (ERROR);
	}
	signal(SIGPIPE, catchserverpipe);
	openserverpipe();

	if (mknod(ClientPipeName, S_IFIFO|0666, 0) < 0) {
		(void) printf("gated: mknod of %s", ClientPipeName);
		return (ERROR);
	}
	signal(SIGPIPE, catchclientpipe);
	openclientpipe();

	return (snmp_socket);
}

