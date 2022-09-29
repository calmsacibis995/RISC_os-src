#ident "$Header: udpip.c,v 1.1 87/08/18 16:32:07 mdove Exp $"
/*	%Q%	%I%	%M%	*/

/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

#include "sys/param.h"
#include "netinet/in_systm.h"
#include "netinet/in.h"
#include "netinet/ip.h"
#include "netinet/ip_var.h"
#include "netinet/udp.h"

#undef KERNEL
#include "netinet/udp_var.h"
#include "saio/saio.h"
#include "saio/socket.h"
#include "saio/arp.h"
#include "saio/ei.h"
#include "saio/mbuf.h"

#define cei(x)	((struct ether_info *)(x->i_ino_dir))

u_char	_ipcksum = 1;		/* require ip checksum good */

/*
 * Ip input routine.  Checksum and byte swap header.  If fragmented
 * try to reassamble.  If complete and fragment queue exists, discard.
 * Process options.  Pass to next level.
 */
_ip_input(ifp, m)
struct ifnet *ifp;
struct mbuf *m;
{
	register struct ip *ip;
	int hlen, s;
	struct sockaddr_in *sin;

	if (m->m_len < sizeof (struct ip))
		goto bad1;

	ip = mtod(m, struct ip *);
	if ((hlen = ip->ip_hl << 2) > m->m_len) {
bad1:
		veprintf("ip hdr len");
		goto bad;
	}

	if (_ipcksum)
		if (ip->ip_sum = in_cksum(m, hlen)) {
			veprintf("ip cksum");
			goto bad;
		}

	/*
	 * Convert fields to host representation.
	 */
	ip->ip_len = ntohs((u_short)ip->ip_len);
	if (ip->ip_len < hlen) {
		veprintf("ip len");
		goto bad;
	}

	ip->ip_id = ntohs(ip->ip_id);
	ip->ip_off = ntohs((u_short)ip->ip_off);

	/*
	 * Check that the amount of data in the buffers
	 * is as at least much as the IP header would have us expect.
	 * Drop packet if shorter than we expect.
	 */
	if (m->m_len < ip->ip_len) {
		veprintf("ip pkt len");
		goto bad;
	}

	if (hlen > sizeof (struct ip)) {
		veprintf("ip options");
		goto bad;
	}

	/*
	 * Only accept packets addressed directly to us,
	 * in particular, discard broadcast packets
	 */
	sin = (struct sockaddr_in *)&ifp->if_addr;
	if (sin->sin_addr.s_addr != ip->ip_dst.s_addr)
		goto bad;

	/*
	 * Adjust ip_len to not reflect header,
	 * abort if packet is fragmented.
	 */
	ip->ip_len -= hlen;
	if ((ip->ip_off & IP_MF) || ip->ip_off) {
		veprintf("ip fragments");
		goto bad;
	}

	if (ip->ip_p != IPPROTO_UDP) {
bad:
		_m_freem(m);
		return;
	}

	_udp_input(ifp, m);
}

static u_short ip_id;

_ip_output(ifp, m)
struct ifnet *ifp;
struct mbuf *m;
{
	register struct ip *ip = mtod(m, struct ip *);
	int len, hlen = sizeof (struct ip), off;
	struct sockaddr dst;

	/*
	 * Fill in IP header.
	 */
	ip->ip_hl = hlen >> 2;
	ip->ip_v = IPVERSION;
	ip->ip_off &= IP_DF;
	ip->ip_id = htons(ip_id++);

	bzero((caddr_t)&dst, sizeof (dst));
	dst.sa_family = AF_INET;
	((struct sockaddr_in *)&dst)->sin_addr = ip->ip_dst;

	/*
	 * If source address not specified yet, use address
	 * of outgoing interface.
	 */
	if (inet_lnaof(ip->ip_src) == INADDR_ANY)
		ip->ip_src.s_addr =
		    ((struct sockaddr_in *)&ifp->if_addr)->sin_addr.s_addr;

	/*
	 * standalone ip doesn't allow messages to be fragmented
	 */
	if (ip->ip_len > ifp->if_mtu) {
		printf("ip packet too large to xmit\n");
		goto bad;
	}

	ip->ip_len = htons((u_short)ip->ip_len);
	ip->ip_off = htons((u_short)ip->ip_off);
	ip->ip_sum = 0;
	ip->ip_sum = in_cksum(m, hlen);
	return((*ifp->if_output)(ifp, m, &dst));

bad:
	_m_freem(m);
	return(-1);
}

/*
 * UDP protocol implementation.
 * Per RFC 768, August, 1980.
 */

/*
 * _udpcksum must be an int, it's referenced from _init_saio and
 * prom environment routines
 */
int	_udpcksum;	/* 0 => 4.2BSD compatability, 1 => 4.3BSD */

static struct	sockaddr_in udp_in;

/*
 * _udp_input -- unwrap udp packet and hang on appropriate socket buffer
 */
_udp_input(ifp, m0)
struct ifnet *ifp;
struct mbuf *m0;
{
	register struct udpiphdr *ui;
	struct so_table *st;
	int len;

	if (m0->m_len < sizeof (struct udpiphdr)) {
		veprintf("udp hdr len");
		goto bad;
	}
	ui = mtod(m0, struct udpiphdr *);

	if (((struct ip *)ui)->ip_hl > (sizeof (struct ip) >> 2)) {
		veprintf("ip options");
		goto bad;
	}

	/*
	 * Make mbuf data length reflect UDP length.
	 * If not enough data to reflect UDP length, drop.
	 */
	len = ntohs((u_short)ui->ui_ulen);
	if (len > ((struct ip *)ui)->ip_len) {
		veprintf("udp pkt len");
		goto bad;
	}

	/*
	 * Checksum extended UDP header and data.
	 */
	if (_udpcksum && ui->ui_sum) {
		ui->ui_next = ui->ui_prev = 0;
		ui->ui_x1 = 0;
		ui->ui_len = ui->ui_ulen;
		if (ui->ui_sum = in_cksum(m0, len + sizeof (struct ip))) {
			veprintf("udp cksum");
			goto bad;
		}
	}

	/*
	 * find appropriate socket buffer
	 */
	st = _find_socket(ui->ui_dport);
	if (st == 0) {
		veprintf("udp port");
		goto bad;
	}

	/*
	 * Construct sockaddr format source address.
	 * Stuff source address and datagram in user buffer.
	 */
	udp_in.sin_family = AF_INET;
	udp_in.sin_port = ui->ui_sport;
	udp_in.sin_addr = ui->ui_src;
	m0->m_len -= sizeof (struct udpiphdr);
	m0->m_off += sizeof (struct udpiphdr);
	/*
	 * HANG ON SO_TABLE IF SPACE, TOSS OTHERWISE
	 */
	if (_so_append(st, (struct sockaddr *)&udp_in, m0) == 0) {
		veprintf("udp sktbuf");
		goto bad;
	}
	return;

bad:
	_m_freem(m0);
}

_udp_output(io, ifp, m0)
struct iob *io;
struct ifnet *ifp;
struct mbuf *m0;
{
	register struct udpiphdr *ui;
	int len;

	/*
	 * Calculate data length and get a mbuf
	 * for UDP and IP headers.
	 */
	len = m0->m_len;

	/*
	 * Fill in mbuf with extended UDP header
	 * and addresses and length put into network format.
	 */
	m0->m_off -= sizeof (struct udpiphdr);
	if (m0->m_off < MMINOFF)
		_io_abort("udp_output");

	m0->m_len += sizeof (struct udpiphdr);
	ui = mtod(m0, struct udpiphdr *);
	ui->ui_next = ui->ui_prev = 0;
	ui->ui_x1 = 0;
	ui->ui_pr = IPPROTO_UDP;
	ui->ui_len = htons((u_short)len + sizeof (struct udphdr));
	/*
	 * CHECK THESE
	 */
	ui->ui_src = ((struct sockaddr_in *)&ifp->if_addr)->sin_addr;
	ui->ui_dst = cei(io)->ei_dstaddr.sin_addr;
	ui->ui_sport = cei(io)->ei_udpport;
	ui->ui_dport = cei(io)->ei_dstaddr.sin_port;
	ui->ui_ulen = ui->ui_len;

	/*
	 * Stuff checksum and output datagram.
	 */
	ui->ui_sum = 0;
	if (_udpcksum) {
		if ((ui->ui_sum=in_cksum(m0, sizeof(struct udpiphdr)+len)) == 0)
			ui->ui_sum = -1;
	}
	((struct ip *)ui)->ip_len = sizeof (struct udpiphdr) + len;
	((struct ip *)ui)->ip_ttl = MAXTTL;
	return(_ip_output(ifp, m0));
}

int _nocksum;

/*
 * Checksum routine for Internet Protocol family headers (mips version).
 *
 * This routine is very heavily used in the network
 * code and should be modified for each CPU to be as fast as possible.
 */
static
in_cksum(m, len)
struct mbuf *m;
int len;
{
	int ck;
	unsigned short _cksum1();
	int mlen;
	char *addr;

	if (_nocksum)
		return(0);

	mlen = (m->m_len > len) ? len : m->m_len;
	addr = mtod(m, char *);

	if ((int)addr & 1)
		ck = nuxi_s(_cksum1(addr, mlen, 0));
	else
		ck = _cksum1(addr, mlen, 0);

	len -= mlen;
	if (len)
		printf("in_cksum, ran out of data, %d bytes left\n", len);

	return(~ck & 0xFFFF);
}

static
veprintf(errmsg)
char *errmsg;
{
	extern Verbose;

	if (Verbose)
		printf("%s error\n", errmsg);
}
