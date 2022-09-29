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
/* $Header: bootp.h,v 1.2.2.2 90/05/09 19:10:45 wje Exp $ */

/*
 * Bootstrap Protocol (BOOTP).  RFC 951.
 */

struct bootp {
	u_char	bp_op;		/* packet opcode type */
#define	BOOTREQUEST	1
#define	BOOTREPLY	2
#define	BOOTMSG		3	/* special op for vadmin */
	u_char	bp_htype;	/* hardware addr type */
	u_char	bp_hlen;	/* hardware addr length */
	u_char	bp_hops;	/* gateway hops */
	u_long	bp_xid;		/* transaction ID */
	u_short	bp_secs;	/* seconds since boot began */	
	u_short	bp_unused;
	iaddr_t	bp_ciaddr;	/* client IP address */
	iaddr_t	bp_yiaddr;	/* 'your' IP address */
	iaddr_t	bp_siaddr;	/* server IP address */
	iaddr_t	bp_giaddr;	/* gateway IP address */
	u_char	bp_chaddr[16];	/* client hardware address */
	u_char	bp_sname[64];	/* server host name */
	u_char	bp_file[128];	/* boot file name */
	u_char	bp_cname[64];	/* client IP address */
};

/*
 * UDP port numbers, server and client.
 */
#define	IPPORT_BOOTPS		67
#define	IPPORT_BOOTPC		68

/*
 * "vendor" data permitted for Stanford boot clients.
 */
struct vend {
	u_char	v_magic[4];	/* magic number */
	u_long	v_flags;	/* flags/opcodes, etc. */
	u_char	v_unused[56];	/* currently unused */
};

#define	VM_STANFORD	"STAN"	/* v_magic for Stanford */

/* v_flags values */
#define	VF_PCBOOT	1	/* an IBMPC or Mac wants environment info */
#define	VF_HELP		2	/* help me, I'm not registered */
