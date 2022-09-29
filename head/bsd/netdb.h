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
/* $Header: netdb.h,v 1.9.1.6 90/05/10 18:40:59 wje Locked $ */

#ifndef	_BSD_NETDB_
#define	_BSD_NETDB_	1


/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)netdb.h	5.7 (Berkeley) 5/12/86
 */

/*
 * Structures returned by network
 * data base library.  All addresses
 * are supplied in host order, and
 * returned in network order (suitable
 * for use in system calls).
 */
struct	hostent {
	char	*h_name;	/* official name of host */
	char	**h_aliases;	/* alias list */
	int	h_addrtype;	/* host address type */
	int	h_length;	/* length of address */
	char	**h_addr_list;	/* list of addresses from name server */
#define	h_addr	h_addr_list[0]	/* address, for backward compatiblity */
};

/*
 * Assumption here is that a network number
 * fits in 32 bits -- probably a poor one.
 */
struct	netent {
	char		*n_name;	/* official name of net */
	char		**n_aliases;	/* alias list */
	int		n_addrtype;	/* net address type */
	unsigned long	n_net;		/* network # */
};

struct	servent {
	char	*s_name;	/* official service name */
	char	**s_aliases;	/* alias list */
	int	s_port;		/* port # */
	char	*s_proto;	/* protocol to use */
};

struct	protoent {
	char	*p_name;	/* official protocol name */
	char	**p_aliases;	/* alias list */
	int	p_proto;	/* protocol # */
};

struct rpcent {
	char    *r_name;        /* name of server for this rpc program */
	char    **r_aliases;    /* alias list */
	int     r_number;       /* rpc program number */
};

struct hostent	*gethostbyname(), *gethostbyaddr(), *gethostent();
struct netent	*getnetbyname(), *getnetbyaddr(), *getnetent();
struct servent	*getservbyname(), *getservbyport(), *getservent();
struct protoent	*getprotobyname(), *getprotobynumber(), *getprotoent();
struct rpcent	*getrpcbyname(), *getrpcbynumber(), *getrpcent();

/*
 * Error return codes from gethostbyname() and gethostbyaddr()
 */

extern  int h_errno;	

#define	HOST_NOT_FOUND	1 /* Authoritative Answer Host not found */
#define	TRY_AGAIN	2 /* Non-Authoritive Host not found, or SERVERFAIL */
#define	NO_RECOVERY	3 /* Non recoverable errors, FORMERR, REFUSED, NOTIMP */
#define	NO_DATA		4 /* Valid name, no data record of requested type */
#define	NO_ADDRESS	NO_DATA		/* no address, look for MX record */

/* Stuff for VIRTUAL INFORMATION SERVICES, VIS */
char *(*vis_nextserv())();

#define VIS_DONE		(char *(*)())0x0
#define VIS_FIRST_CALL		(char *(*)())0x1
#define VIS_DEFAULT_SERVICES	"/etc/vis.conf"

/* Service Routines: aliases are provided but not preferred */
#define VIS_FILES	0x0
#define VIS_FILE	0x1	/* alias for VIS_FILES, prefer VIS_FILES */
#define VIS_DNS		0x2
#define VIS_NIS		0x3
#define VIS_YP		0x4	/* alias for VIS_NIS, prefer VIS_NIS */
#define VIS_X500	0x5
#define VIS_IEN116	0x6

char *yp_service(), *dns_service(), *ht_service(), *no_service();

#ifdef VIS
/* We've allowed some room for typos, but we don't want to go overboard */
struct vis_services {
	char *vs_name;
	char *(*vs_func)();
} Vis_services[] = {
/* NB: The first entry is used as the default by vis.c */
	{ "files",	ht_service	}, /* 0 */
	{ "file",	ht_service	}, /* 1: alias 4 files, prefer files */
	{ "dns",	dns_service	}, /* 2 */
	{ "nis",	yp_service	}, /* 3 */
	{ "yp",		yp_service	}, /* 4: alias for nis, prefer nis */
	{ "x500",	no_service	}, /* 5 */
	{ "ien116",	no_service	}, /* 6 */
	{ (char *)0,	VIS_DONE	}
};
#else
extern struct vis_services Vis_services[];
#endif
	
/* requests are VIS_CLASS & VIS_REQUEST, class is high 2 bytes, req low */
#define VIS_CLASS(a)		((a) & 0xffff0000)
#define VIS_RPC			0x00000
#define VIS_HOST		0x10000
#define VIS_PASSWD		0x20000
#define VIS_GROUP		0x30000
#define VIS_NET			0x40000
#define VIS_NETGR		0x50000
#define VIS_PROTO		0x60000
#define VIS_SERV		0x70000

#ifdef VIS
struct vis_classes {
	char *name;
	int class;
} Vis_classes[] = {
	"rpc",		VIS_RPC,		/* 0 */
	"host",		VIS_HOST,		/* 1 */
	"passwd",	VIS_PASSWD,		/* 2 */
	"group",	VIS_GROUP,		/* 3 */
	"net",		VIS_NET,		/* 4 */
	"netgroup",	VIS_NETGR,		/* 5 */
	"proto",	VIS_PROTO,		/* 6 */
	"services",	VIS_SERV,		/* 7 */
	(char *)0,		0
};
#else
extern struct vis_classes Vis_classes[];
#endif

#define VIS_GHOSTBYNAME		VIS_HOST + 1
#define VIS_GHOSTBYADDR		VIS_HOST + 2
#define VIS_SETHOSTENT		VIS_HOST + 3
#define VIS_ENDHOSTENT		VIS_HOST + 4
#define VIS_GETHOSTENT		VIS_HOST + 5

#define VIS_GETGRBYNAME		VIS_GROUP + 1
#define VIS_GETGRBYGID		VIS_GROUP + 2
#define VIS_SETGRENT		VIS_GROUP + 3
#define VIS_ENDGRENT		VIS_GROUP + 4
#define VIS_GETGRENT		VIS_GROUP + 5
#define VIS_FGETGRENT		VIS_GROUP + 6

#define VIS_GETPWBYNAME		VIS_PASSWD + 1
#define VIS_GETPWBYUID		VIS_PASSWD + 2
#define VIS_SETPWENT		VIS_PASSWD + 3
#define VIS_ENDPWENT		VIS_PASSWD + 4
#define VIS_GETPWENT		VIS_PASSWD + 5
#define VIS_GETPW		VIS_PASSWD + 6

#define VIS_GETNETBYADDR	VIS_NET + 1
#define VIS_GETNETBYNAME	VIS_NET + 2
#define VIS_SETNETENT		VIS_NET + 3
#define VIS_ENDNETENT		VIS_NET + 4
#define VIS_GETNETENT		VIS_NET + 5

#define VIS_SETNETGRENT		VIS_NETGR + 1
#define VIS_ENDNETGRENT		VIS_NETGR + 2
#define VIS_GETNETGRENT		VIS_NETGR + 3
#define VIS_INNETGR		VIS_NETGR + 4

#define VIS_GETPROTOBYNUM	VIS_PROTO + 1
#define VIS_GETPROTOBYNAME	VIS_PROTO + 2
#define VIS_GETPROTOENT		VIS_PROTO + 3
#define VIS_SETPROTOENT		VIS_PROTO + 4
#define VIS_ENDPROTOENT		VIS_PROTO + 5

#define VIS_GETRPCBYNUM		VIS_RPC + 1
#define VIS_GETRPCBYNAME	VIS_RPC + 2
#define VIS_GETRPCENT		VIS_RPC + 3
#define VIS_SETRPCENT		VIS_RPC + 4
#define VIS_ENDRPCENT		VIS_RPC + 5

#define VIS_SETSERVENT		VIS_SERV + 1
#define VIS_ENDSERVENT		VIS_SERV + 2
#define VIS_GETSERVENT		VIS_SERV + 3
#define VIS_GETSERVBYNAME	VIS_SERV + 4
#define VIS_GETSERVBYPORT	VIS_SERV + 5

/* requests that require multiple args get structure */

typedef struct {
	char	*r_addr;
	int	r_len;
	int	r_type;
} Vis_hbyaddr_req;

typedef struct {
	int	r_net;
	int	r_type;
} Vis_nbyaddr_req;

typedef struct {
	char	**r_machinep;
	char	**r_namep;
	char	**r_domainp;
} Vis_gnetgrent_req;

typedef struct {
	char	*r_grp;
	char	*r_mach;
	char	*r_nm;
	char	*r_dom;
} Vis_innetgr_req;

typedef struct {
	char	*r_name;
	char	*r_proto;
} Vis_sbyname_req;

typedef struct {
	int	r_port;
	char	*r_proto;
} Vis_sbyport_req;

typedef struct {
	int	r_uid;
	char	*r_buffer;
} Vis_getpw_req;

#endif	_BSD_NETDB_
