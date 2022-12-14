#ident "$Header: udp.h,v 1.2 90/01/23 13:33:36 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*	udp.h	6.1	83/07/29	*/

/*
 * Udp protocol header.
 * Per RFC 768, September, 1981.
 */
struct udphdr {
	u_short	uh_sport;		/* source port */
	u_short	uh_dport;		/* destination port */
	short	uh_ulen;		/* udp length */
	u_short	uh_sum;			/* udp checksum */
};
