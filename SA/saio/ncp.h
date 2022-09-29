#ident "$Header: ncp.h,v 1.2 90/01/16 17:24:30 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * ncp.h -- definitions for network console protocol
 */

/*
 * misc NCP constants
 */
#define	NCP_MAXDATA	1024	/* max data length for ncp packet */
#define	NCP_UNITS	2	/* max number of network consoles */
#define	NCPPORT_CONS	2000	/* udp port base for network consoles */
#define	NCP_REV		1	/* initial protocol revision */
#define	NCP_MAXCREDITS	3	/* # of xmit packets w/o further credits */

/*
 * format of a network console protocol header
 */
struct ncphdr {
	char nh_rev;		/* protocol rev */
	char nh_type;		/* packet type */
	short nh_credits;	/* xmit credits */
	short nh_datalen;	/* data length */
};

/*
 * ncp_packet -- network console protocol packet format
 */
struct ncp_packet {
	struct ncphdr np_ncp;
	char np_data[NCP_MAXDATA];
};

/*
 * ncp packet types
 */
#define	NCPTYPE_OPEN	1	/* ask if we can converse */
#define	NCPTYPE_CLOSE	2	/* end of conversation */
#define	NCPTYPE_DATA	3	/* body of conversation */
#define	NCPTYPE_RESET	4	/* rudely interrupt */
#define	NCPTYPE_ERROR	5	/* note a faux pas, error msg in data */
#define	NCPTYPE_START	6	/* start xmit if stopped */
#define	NCPTYPE_STOP	7	/* stop xmit */
