#ident "$Header: ncp.c,v 1.2 90/01/16 17:22:50 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * ncp.c -- network console protocol implementation
 * (So far: NOT DEBUGGED!)
 */

/*
 * TODO:
 * Credit stuff needs to be thought out.  Currently, credits are
 * asymmetric, only passed from network console source to prom in
 * order to keep prom from swamping remote host.  In order to make
 * symmetric with unreliable protocol (which ncp is) requires prom
 * timers.
 */

#ifdef NCP
#include "sys/param.h"
#include "netinet/in.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "saio/socket.h"
#include "saio/ncp.h"
#include "saio/ei.h"
#include "saio/setjmp.h"

#define cei(x)	((struct ether_info *)(x->i_ino_dir))
extern struct in_addr inet_makeaddr();

static void ncp_getpacket();
static void ncpreply();
static void ncpflush();

/*
 * port_buf represents a network console channel
 * each channel is associated with a UDP port
 */
static struct port_buf {
	struct iob *pb_iob;		/* standalone io parameter block */
	struct sockaddr_in pb_dstaddr;	/* internet address of conversant */
	struct ncp_packet pb_ipkt;	/* input packet */
	struct ncp_packet pb_opkt;	/* output packet */
	char *pb_idp;			/* input buffer ptr */
	char *pb_odp;			/* output buffer ptr */
	int pb_icnt;			/* remaining chars in input buffer */
	int pb_ocnt;			/* chars in output buffer */
	int pb_credits;			/* xmit credits */
	char pb_conversing;		/* non-zero if conversing */
	char pb_stop;			/* non-zero if "xoff"ed */
} port_buf[NCP_UNITS];

_ncpinit()
{
	if (sizeof (struct ether_info) > IOB_INODE)
		_io_abort ("bad size in iob for ncp");
	bzero(port_buf, sizeof(port_buf));
}

/*
 * _ncpopen -- initialize console port
 */
_ncpopen(io)
struct iob *io;
{
	register struct port_buf *pb = &port_buf[io->i_ctlr];

	pb->pb_icnt = pb->pb_ocnt = 0;
	pb->pb_conversing = 0;
	pb->pb_odp = pb->pb_opkt.np_data;
	pb->pb_credits = 0;
	pb->pb_iob = io;
	DEVIOCTL(io, NIOCBIND, htons(NCPPORT_CONS + io->i_ctlr));
	io->i_flgs |= F_SCAN;
	return(0);
}

/*
 * _ncpread -- attempt to get character from network console port
 * returns count of chars transferred
 */
int
_ncpread(io, buf, cnt)
struct iob *io;
char *buf;
int cnt;
{
	register struct port_buf *pb = &port_buf[io->i_ctlr];
	register int cc;
	int ocnt = cnt;

	/*
	 * flush ncp output to ensure prompts are printed before
	 * read for reply, etc
	 */
	ncpflush(io);

	while (cnt) {
		while (pb->pb_icnt == 0) {
			if (io->i_flgs & F_NBLOCK)
				return(ocnt - cnt);
			ncp_getpacket(io);
		}
		cc = _min(cnt, pb->pb_icnt);
		bcopy(pb->pb_idp, buf, cc);
		pb->pb_icnt -= cc;
		buf += cc;
		cnt -= cc;
	}
	return(ocnt);
}

/*
 * _ncpwrite -- write characters to network console
 * returns count of chars transferred
 */
int
_ncpwrite(io, buf, cnt)
struct iob *io;
char *buf;
int cnt;
{
	register struct port_buf *pb = &port_buf[io->i_ctlr];
	register cc;
	int ocnt = cnt;

	while (cnt > 0) {
		cc = _min(cnt, NCP_MAXDATA - pb->pb_ocnt);
		bcopy(buf, pb->pb_odp, cc);
		pb->pb_ocnt += cc;
		pb->pb_odp += cc;
		cnt -= cc;
		if (NCP_MAXDATA - pb->pb_ocnt < NCP_MAXDATA/4)
			ncpflush(io);
	}
	return(ocnt);
}

/*
 * _ncpioctl -- ioctl routine for ncp
 */
_ncpioctl(io, cmd, arg)
struct iob *io;
int cmd;
int arg;
{
	register struct port_buf *pb = &port_buf[io->i_ctlr];

	switch (cmd) {
	case FIOCSCAN:
		if (pb->pb_icnt == 0)
			ncp_getpacket(io);
		break;
	}
	return(0);
}

/*
 * _ncpclose -- flush any buffers sitting around
 */
_ncpclose(io)
struct iob *io;
{
	ncpflush(io);
	return(0);
}

/*
 * ncp_getpacket -- get packets off of network recv queue
 */
static void
ncp_getpacket(io)
struct iob *io;
{
	register struct port_buf *pb = &port_buf[io->i_ctlr];
	register struct ncp_packet *np = &pb->pb_ipkt;
	int cc, type, datalen, credits;

	io->i_ma = (char *)np;
	io->i_cc = sizeof(*np);
	if ((cc = DEVREAD(io)) == 0)
		return;
	cc -= sizeof(struct ncphdr);
	/* denuxi header */
	type = np->np_ncp.nh_type;
	datalen = ntohs(np->np_ncp.nh_datalen);
	credits = ntohs(np->np_ncp.nh_credits);
	/* toss obviously bogus packets */
	if (np->np_ncp.nh_rev != NCP_REV) {
		ncpreply(io, "Incorrect ncp protocol rev");
		return;
	}
	if (cc < 0 || cc < datalen)
		return;
	if (type == NCPTYPE_RESET) {
		ncpreply(io, "Connection RESET");
		ncpflush(io);
	}
	if (pb->pb_conversing
	 && bcmp(pb->pb_dstaddr, cei(io)->ei_srcaddr, sizeof(pb->pb_dstaddr))) {
		/*
		 * Say sorry, but we're already talking with
		 * someone
		 */
		ncpreply(io, "Connection in use");
		return;
	}
	switch (type) {

	case NCPTYPE_DATA:
		pb->pb_icnt = datalen;
		pb->pb_idp = pb->pb_ipkt.np_data;
		pb->pb_credits = _min(pb->pb_credits + credits, NCP_MAXCREDITS);
		if (pb->pb_conversing)
			break;
		credits = 0;
		/* else fall through ... */

	case NCPTYPE_RESET:
	case NCPTYPE_OPEN:
		pb->pb_conversing = 1;
		pb->pb_credits = _min(pb->pb_credits + credits, NCP_MAXCREDITS);
		cei(io)->ei_dstaddr = cei(io)->ei_srcaddr;
		break;

	case NCPTYPE_CLOSE:
		pb->pb_conversing = 0;
		break;

	/*
	 * Oops, we did something wrong!?
	 */
	case NCPTYPE_ERROR:
		printf("%s\n", pb->pb_ipkt.np_data);
		break;

	case NCPTYPE_STOP:
		pb->pb_stop = 1;
		break;

	case NCPTYPE_START:
		pb->pb_stop = 0;
		break;
	}
}

/*
 * ncpflush -- flush output on network port
 */
static void
ncpflush (io)
	register struct iob *io;
{
	register struct port_buf *pb = &port_buf[io->i_ctlr];

	if (pb->pb_ocnt && pb->pb_conversing) {
		io->i_ma = (char *)&pb->pb_opkt.np_ncp;
		io->i_cc = mk_ncp(&pb->pb_opkt, NCPTYPE_DATA, pb->pb_ocnt);
		while (pb->pb_stop || pb->pb_credits <= 0)
			_scandevs();
		DEVWRITE(io);
		pb->pb_credits--;
	}
	pb->pb_ocnt = 0;
	pb->pb_odp = pb->pb_opkt.np_data;
}

/*
 * ncpreply(net_address_ptr, message) -- send error message packet to
 * net_address
 */
static void
ncpreply(io, msg)
struct iob *io;
char *msg;
{
	struct iob iob, *iobp;
	struct ncp_packet ncp;

	/*
	 * Cobble together a short lived iob to send reply
	 */
	iob = *io;
	iobp = &iob;
	strcpy(ncp.np_data, msg);
	cei(iobp)->ei_udpport = cei(io)->ei_srcaddr.sin_port;
	cei(iobp)->ei_dstaddr = cei(io)->ei_srcaddr;
	iob.i_ma = (char *)&ncp;
	iob.i_cc = mk_ncp(&ncp, NCPTYPE_ERROR, strlen(msg));
	DEVWRITE(&iob);
	ncpflush(&iob);
}

/*
 * mk_ncp(ncp_header_ptr, ncp_pkt_type, data_length)
 * fill-in ncp packet header
 */
static
mk_ncp(np, type, datalen)
register struct ncp_packet *np;
int type;
int datalen;
{
	np->np_ncp.nh_rev = NCP_REV;
	np->np_ncp.nh_type = type;
	np->np_ncp.nh_datalen = htons(datalen);
	return(datalen + sizeof(struct ncphdr));
}
#endif NCP
