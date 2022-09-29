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
/* $Header: protosw.h,v 1.6.3.2 90/05/10 04:54:03 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)protosw.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Protocol switch table.
 *
 * Each protocol has a handle initializing one of these structures,
 * which is used for protocol-protocol and system-protocol communication.
 *
 * A protocol is called through the pr_init entry before any other.
 * Thereafter it is called every 200ms through the pr_fasttimo entry and
 * every 500ms through the pr_slowtimo for timer based actions.
 * The system will call the pr_drain entry if it is low on space and
 * this should throw away any non-critical data.
 *
 * Protocols pass data between themselves as chains of mbufs using
 * the pr_input and pr_output hooks.  Pr_input passes data up (towards
 * UNIX) and pr_output passes it down (towards the imps); control
 * information passes up and down on pr_ctlinput and pr_ctloutput.
 * The protocol is responsible for the space occupied by any the
 * arguments to these entries and must dispose it.
 *
 * The userreq routine interfaces protocols to the system and is
 * described below.
 */
struct bsd43_(protosw) {
	short	pr_type;		/* socket type used for */
	struct	bsd43_(domain) *pr_domain;	/* domain protocol a member of */
	short	pr_protocol;		/* protocol number */
	short	pr_flags;		/* see below */
/* protocol-protocol hooks */
	int	(*pr_input)();		/* input to protocol (from below) */
	int	(*pr_output)();		/* output to protocol (from above) */
	int	(*pr_ctlinput)();	/* control input (from below) */
	int	(*pr_ctloutput)();	/* control output (from above) */
/* user-protocol hook */
	int	(*pr_usrreq)();		/* user request: see list below */
/* utility hooks */
	int	(*pr_init)();		/* initialization hook */
	int	(*pr_fasttimo)();	/* fast timeout (200ms) */
	int	(*pr_slowtimo)();	/* slow timeout (500ms) */
	int	(*pr_drain)();		/* flush any excess space possible */
};

#define	BSD43_PR_SLOWHZ	2		/* 2 slow timeouts per second */
#define	BSD43_PR_FASTHZ	5		/* 5 fast timeouts per second */

/*
 * Values for pr_flags
 */
#define	BSD43_PR_ATOMIC	0x01		/* exchange atomic messages only */
#define	BSD43_PR_ADDR		0x02		/* addresses given with messages */
/* in the current implementation, PR_ADDR needs PR_ATOMIC to work */
#define	BSD43_PR_CONNREQUIRED	0x04		/* connection required by protocol */
#define	BSD43_PR_WANTRCVD	0x08		/* want PRU_RCVD calls */
#define	BSD43_PR_RIGHTS	0x10		/* passes capabilities */

/*
 * The arguments to usrreq are:
 *	(*protosw[].pr_usrreq)(up, req, m, nam, opt);
 * where up is a (struct socket *), req is one of these requests,
 * m is a optional mbuf chain containing a message,
 * nam is an optional mbuf chain containing an address,
 * and opt is a pointer to a socketopt structure or nil.
 * The protocol is responsible for disposal of the mbuf chain m,
 * the caller is responsible for any space held by nam and opt.
 * A non-zero return from usrreq gives an
 * UNIX error number which should be passed to higher level software.
 */
#define	BSD43_PRU_ATTACH		0	/* attach protocol to up */
#define	BSD43_PRU_DETACH		1	/* detach protocol from up */
#define	BSD43_PRU_BIND		2	/* bind socket to address */
#define	BSD43_PRU_LISTEN		3	/* listen for connection */
#define	BSD43_PRU_CONNECT		4	/* establish connection to peer */
#define	BSD43_PRU_ACCEPT		5	/* accept connection from peer */
#define	BSD43_PRU_DISCONNECT		6	/* disconnect from peer */
#define	BSD43_PRU_SHUTDOWN		7	/* won't send any more data */
#define	BSD43_PRU_RCVD		8	/* have taken data; more room now */
#define	BSD43_PRU_SEND		9	/* send this data */
#define	BSD43_PRU_ABORT		10	/* abort (fast DISCONNECT, DETATCH) */
#define	BSD43_PRU_CONTROL		11	/* control operations on protocol */
#define	BSD43_PRU_SENSE		12	/* return status into m */
#define	BSD43_PRU_RCVOOB		13	/* retrieve out of band data */
#define	BSD43_PRU_SENDOOB		14	/* send out of band data */
#define	BSD43_PRU_SOCKADDR		15	/* fetch socket's address */
#define	BSD43_PRU_PEERADDR		16	/* fetch peer's address */
#define	BSD43_PRU_CONNECT2		17	/* connect two sockets */
/* begin for protocols internal use */
#define	BSD43_PRU_FASTTIMO		18	/* 200ms timeout */
#define	BSD43_PRU_SLOWTIMO		19	/* 500ms timeout */
#define	BSD43_PRU_PROTORCV		20	/* receive from below */
#define	BSD43_PRU_PROTOSEND		21	/* send to below */

#define	BSD43_PRU_NREQ		21

#ifdef PRUREQUESTS
char *bsd43_(prurequests)[] = {
	"ATTACH",	"DETACH",	"BIND",		"LISTEN",
	"CONNECT",	"ACCEPT",	"DISCONNECT",	"SHUTDOWN",
	"RCVD",		"SEND",		"ABORT",	"CONTROL",
	"SENSE",	"RCVOOB",	"SENDOOB",	"SOCKADDR",
	"PEERADDR",	"CONNECT2",	"FASTTIMO",	"SLOWTIMO",
	"PROTORCV",	"PROTOSEND",
};
#endif

/*
 * The arguments to the ctlinput routine are
 *	(*protosw[].pr_ctlinput)(cmd, arg);
 * where cmd is one of the commands below, and arg is
 * an optional argument (caddr_t).
 *
 * N.B. The IMP code, in particular, pressumes the values
 *      of some of the commands; change with extreme care.
 * TODO:
 *	spread out codes so new ICMP codes can be
 *	accomodated more easily
 */
#define	BSD43_PRC_IFDOWN		0	/* interface transition */
#define	BSD43_PRC_ROUTEDEAD		1	/* select new route if possible */
#define	BSD43_PRC_QUENCH		4	/* some said to slow down */
#define	BSD43_PRC_MSGSIZE		5	/* message size forced drop */
#define	BSD43_PRC_HOSTDEAD		6	/* normally from IMP */
#define	BSD43_PRC_HOSTUNREACH		7	/* ditto */
#define	BSD43_PRC_UNREACH_NET		8	/* no route to network */
#define	BSD43_PRC_UNREACH_HOST	9	/* no route to host */
#define	BSD43_PRC_UNREACH_PROTOCOL	10	/* dst says bad protocol */
#define	BSD43_PRC_UNREACH_PORT	11	/* bad port # */
#define	BSD43_PRC_UNREACH_NEEDFRAG	12	/* IP_DF caused drop */
#define	BSD43_PRC_UNREACH_SRCFAIL	13	/* source route failed */
#define	BSD43_PRC_REDIRECT_NET	14	/* net routing redirect */
#define	BSD43_PRC_REDIRECT_HOST	15	/* host routing redirect */
#define	BSD43_PRC_REDIRECT_TOSNET	16	/* redirect for type of service & net */
#define	BSD43_PRC_REDIRECT_TOSHOST	17	/* redirect for tos & host */
#define	BSD43_PRC_TIMXCEED_INTRANS	18	/* packet lifetime expired in transit */
#define	BSD43_PRC_TIMXCEED_REASS	19	/* lifetime expired on reass q */
#define	BSD43_PRC_PARAMPROB		20	/* header incorrect */

#define	BSD43_PRC_NCMDS		21

#ifdef PRCREQUESTS
char	*bsd43_(prcrequests)[] = {
	"IFDOWN", "ROUTEDEAD", "#2", "#3",
	"QUENCH", "MSGSIZE", "HOSTDEAD", "HOSTUNREACH",
	"NET-UNREACH", "HOST-UNREACH", "PROTO-UNREACH", "PORT-UNREACH",
	"FRAG-UNREACH", "SRCFAIL-UNREACH", "NET-REDIRECT", "HOST-REDIRECT",
	"TOSNET-REDIRECT", "TOSHOST-REDIRECT", "TX-INTRANS", "TX-REASS",
	"PARAMPROB"
};
#endif

/*
 * The arguments to ctloutput are:
 *	(*protosw[].pr_ctloutput)(req, so, level, optname, optval);
 * req is one of the actions listed below, so is a (struct socket *),
 * level is an indication of which protocol layer the option is intended.
 * optname is a protocol dependent socket option request,
 * optval is a pointer to a mbuf-chain pointer, for value-return results.
 * The protocol is responsible for disposal of the mbuf chain *optval
 * if supplied,
 * the caller is responsible for any space held by *optval, when returned.
 * A non-zero return from usrreq gives an
 * UNIX error number which should be passed to higher level software.
 */
#define	BSD43_PRCO_GETOPT	0
#define	BSD43_PRCO_SETOPT	1

#define	BSD43_PRCO_NCMDS	2

#ifdef PRCOREQUESTS
char	*bsd43_(prcorequests)[] = {
	"GETOPT", "SETOPT",
};
#endif

#ifdef KERNEL
extern	struct bsd43_(protosw) *bsd43_(pffindproto)(), *bsd43_(pffindtype)();
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define PRCO_GETOPT BSD43_PRCO_GETOPT
#   define PRCO_NCMDS BSD43_PRCO_NCMDS
#   define PRCO_SETOPT BSD43_PRCO_SETOPT
#   define PRC_HOSTDEAD BSD43_PRC_HOSTDEAD
#   define PRC_HOSTUNREACH BSD43_PRC_HOSTUNREACH
#   define PRC_IFDOWN BSD43_PRC_IFDOWN
#   define PRC_MSGSIZE BSD43_PRC_MSGSIZE
#   define PRC_NCMDS BSD43_PRC_NCMDS
#   define PRC_PARAMPROB BSD43_PRC_PARAMPROB
#   define PRC_QUENCH BSD43_PRC_QUENCH
#   define PRC_REDIRECT_HOST BSD43_PRC_REDIRECT_HOST
#   define PRC_REDIRECT_NET BSD43_PRC_REDIRECT_NET
#   define PRC_REDIRECT_TOSHOST BSD43_PRC_REDIRECT_TOSHOST
#   define PRC_REDIRECT_TOSNET BSD43_PRC_REDIRECT_TOSNET
#   define PRC_ROUTEDEAD BSD43_PRC_ROUTEDEAD
#   define PRC_TIMXCEED_INTRANS BSD43_PRC_TIMXCEED_INTRANS
#   define PRC_TIMXCEED_REASS BSD43_PRC_TIMXCEED_REASS
#   define PRC_UNREACH_HOST BSD43_PRC_UNREACH_HOST
#   define PRC_UNREACH_NEEDFRAG BSD43_PRC_UNREACH_NEEDFRAG
#   define PRC_UNREACH_NET BSD43_PRC_UNREACH_NET
#   define PRC_UNREACH_PORT BSD43_PRC_UNREACH_PORT
#   define PRC_UNREACH_PROTOCOL BSD43_PRC_UNREACH_PROTOCOL
#   define PRC_UNREACH_SRCFAIL BSD43_PRC_UNREACH_SRCFAIL
#   define PRU_ABORT BSD43_PRU_ABORT
#   define PRU_ACCEPT BSD43_PRU_ACCEPT
#   define PRU_ATTACH BSD43_PRU_ATTACH
#   define PRU_BIND BSD43_PRU_BIND
#   define PRU_CONNECT BSD43_PRU_CONNECT
#   define PRU_CONNECT2 BSD43_PRU_CONNECT2
#   define PRU_CONTROL BSD43_PRU_CONTROL
#   define PRU_DETACH BSD43_PRU_DETACH
#   define PRU_DISCONNECT BSD43_PRU_DISCONNECT
#   define PRU_FASTTIMO BSD43_PRU_FASTTIMO
#   define PRU_LISTEN BSD43_PRU_LISTEN
#   define PRU_NREQ BSD43_PRU_NREQ
#   define PRU_PEERADDR BSD43_PRU_PEERADDR
#   define PRU_PROTORCV BSD43_PRU_PROTORCV
#   define PRU_PROTOSEND BSD43_PRU_PROTOSEND
#   define PRU_RCVD BSD43_PRU_RCVD
#   define PRU_RCVOOB BSD43_PRU_RCVOOB
#   define PRU_SEND BSD43_PRU_SEND
#   define PRU_SENDOOB BSD43_PRU_SENDOOB
#   define PRU_SENSE BSD43_PRU_SENSE
#   define PRU_SHUTDOWN BSD43_PRU_SHUTDOWN
#   define PRU_SLOWTIMO BSD43_PRU_SLOWTIMO
#   define PRU_SOCKADDR BSD43_PRU_SOCKADDR
#   define PR_ADDR BSD43_PR_ADDR
#   define PR_ATOMIC BSD43_PR_ATOMIC
#   define PR_CONNREQUIRED BSD43_PR_CONNREQUIRED
#   define PR_FASTHZ BSD43_PR_FASTHZ
#   define PR_RIGHTS BSD43_PR_RIGHTS
#   define PR_SLOWHZ BSD43_PR_SLOWHZ
#   define PR_WANTRCVD BSD43_PR_WANTRCVD
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


