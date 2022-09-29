#ident "$Header: bfs.c,v 1.6 90/01/17 13:51:47 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * bfs.c -- boot file server protocol implementation
 */

#include "sys/errno.h"
#include "sys/param.h"
#include "netinet/in.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "saio/socket.h"
#include "saio/bfs.h"
#include "saio/setjmp.h"
#include "saio/ei.h"
#include "saio/debug.h"

#define cei(x)	((struct ether_info *)(x->i_ino_dir))

extern struct in_addr inet_makeaddr();
static void mk_bfs();

/*
 * bfs_packet -- boot file server packet format
 */
struct bfs_packet {
	struct bfshdr bp_bfs;
	char bp_data[BFS_MAXPATH+BFS_MAXDATA];
};

_bfsinit()
{
	if (sizeof (struct ether_info) > IOB_INODE)
		_io_abort ("bad size in iob for bfs");
}

/*
 * _bfsopen(io, file, flags) -- find an available server for "file"
 */
_bfsopen(io, filename, flags)
register struct iob *io;
char *filename;
{
	struct bfs_packet xbp, rbp;
	int net;
	char *cp;
	extern char *getenv();
	extern struct in_addr inet_addr();

	if (strlen(filename) > sizeof(cei(io)->ei_filename)-1) {
		printf("path too long for bfs\n");
		goto bad;
	}
	strcpy(cei(io)->ei_filename, filename);

	mk_bfs(&xbp, BFSPT_ENQUIRE, filename, 0, 0, flags);
	if (DEVIOCTL(io, NIOCBIND, htons((u_short)BFSPORT_PROM)) < 0) {
		printf("bind failed\n");
		goto bad;
	}
	/* make an internet broadcast address */
	cp = getenv("netaddr");
	if (cp == NULL || (net = inet_netof(inet_addr(cp))) < 0) {
		printf("$netaddr incorrect: %s", getenv("netaddr"));
		goto bad;
	}
	cei(io)->ei_dstaddr.sin_family = AF_INET;
	cei(io)->ei_dstaddr.sin_addr = inet_makeaddr(net, INADDR_ANY);
	cei(io)->ei_dstaddr.sin_port = htons((u_short)BFSPORT_SERVER);

	/*
	 * Should require a machine name on filename for writes
	 */
	if (bfs_transact(io, &xbp, &rbp, BFSPT_ENQRPY) < 0) {
		printf("No server for %s\n", filename);
bad:
		io->i_errno = ENXIO;
		if (cei(io)->ei_registry > 0)
			_free_socket(cei(io)->ei_registry);
		return(-1);
	}

	cei(io)->ei_dstaddr = cei(io)->ei_srcaddr;
	printf("Obtaining %s from server %s\n", rbp.bp_data,
	    rbp.bp_bfs.bh_server);
	return(0);
}

/*
 * _bfsread(io, buffer, cnt) -- read cnt bytes from channel
 * described by bdp to buffer
 */
int
_bfsread(io, buf, cnt)
struct iob *io;
char *buf;
u_int cnt;
{
	struct bfs_packet xbp, rbp;
	register datalen;
	int cc, pathlen;
	u_int ocnt;
	char *cp;

	cp = getenv("bfsverify");
	ocnt = cnt;
	while (cnt > 0) {
		cc = cnt > BFS_MAXDATA ? BFS_MAXDATA : cnt;
		mk_bfs(&xbp, BFSPT_READ, cei(io)->ei_filename,
		    io->i_offset, cc, 0);
		if (bfs_transact(io, &xbp, &rbp, BFSPT_RDRPY) < 0)
			_io_abort("lost connection");
		datalen = ntohs(rbp.bp_bfs.bh_datalen);
		pathlen = ntohs(rbp.bp_bfs.bh_pathlen);
		/*
		 * an abort may be too severe here
		 */
		if (datalen < 0)
			_io_abort("bfs read failed");
			/* doesn't return */
		if (datalen == 0)
			return(ocnt - cnt);
		bcopy(&rbp.bp_data[pathlen], buf, datalen);
		/*
		 * Play it again Sam.  User set bfsverify to 1 so let's
		 * see if we get same results !  If not, keep trying.
		 */
		if ((cp) && (*cp == 'y')) {
		  mk_bfs(&xbp, BFSPT_READ, cei(io)->ei_filename,
			 io->i_offset, cc, 0);
		  if (bfs_transact(io, &xbp, &rbp, BFSPT_RDRPY) < 0)
		    _io_abort("lost connection");
		  if (datalen != ntohs(rbp.bp_bfs.bh_datalen)) {
		    printf(" bfsverify datalen misscompare: 0x%x 0x%x ",
			   datalen, ntohs(rbp.bp_bfs.bh_datalen));
		    continue;
		  }
		  if (pathlen != ntohs(rbp.bp_bfs.bh_pathlen)) {
		    printf(" bfsverify pathlen misscompare: 0x%x 0x%x ",
			   pathlen, ntohs(rbp.bp_bfs.bh_pathlen));
		    continue;
		  }
		  if (bcmp(&rbp.bp_data[pathlen], buf, datalen)) {
		    printf(" bfsverify data miscompare ");
		    continue;
		  }
		}
		/*
		 *  Time to move on to another block.
		 */
		buf += datalen;
		io->i_offset += datalen;
		cnt -= datalen;
	}
	return(ocnt);
}

#ifdef notdef
/*
 * _bfswrite -- half-baked but should be doable
 */
int
_bfswrite(io, buf, cnt)
struct iob *io;
char *buf;
u_int cnt;
{
	struct bfs_packet xbp, rbp;
	register datalen;
	int cc, pathlen;
	u_int ocnt;

	ocnt = cnt;
	while (cnt > 0) {
		cc = cnt > BFS_MAXDATA ? BFS_MAXDATA : cnt;
		mk_bfs(&xbp, BFSPT_WRITE, cei(io)->ei_filename,
		    io->i_offset, cc, 0);
		pathlen = ntohs(xbp.bp_bfs.bh_pathlen);
		bcopy(buf, &xbp.bp_data[pathlen], cc);
		if (bfs_transact(io, &xbp, &rbp, BFSPT_RDRPY) < 0)
			_io_abort("lost connection");
		datalen = ntohs(rbp.bp_bfs.bh_datalen);
		if (datalen < 0)
			_io_abort("bfs write failed");
			/* doesn't return */
		if (datalen == 0)
			return(ocnt - cnt);
		buf += datalen;
		io->i_offset += datalen;
		cnt -= datalen;
	}
	return(ocnt);
}
#endif notdef

static jmp_buf transact_buf;

/*
 * bfs_transact(io, xmit_packet, recv_packet, rsp_type)
 * transact a packet with server indicated by bfs_desc, return packet
 * is stored in recv_packet, only packets of type rsp_type are accepted
 */
static int
bfs_transact(io, xbp, rbp, rsp_type)
register struct iob *io;
struct bfs_packet *xbp, *rbp;
int rsp_type;
{
	volatile int tries;
	int cc;
	int *old_jmpbuf;
	int old_timer;
	extern int *_timer_jmpbuf;
	extern int Debug;

	/*
	 * save currently running timer
	 */
	old_jmpbuf = _timer_jmpbuf;
	old_timer = old_jmpbuf ? _set_timer(0) : 0;

	tries = 0;
	setjmp(transact_buf);
	if ((tries) && (Debug & DBG_BFS)) printf(" timeout 0x%x ", tries);
	if (++tries > BFS_MAXTRIES)
		return(-1);
	/*
	 * set-up device independent iob parameters
	 */
	io->i_ma = (char *)xbp;
	io->i_cc = ntohs(xbp->bp_bfs.bh_pathlen) + sizeof(struct bfshdr);
	if (rsp_type != BFSPT_RDRPY)
	  io->i_cc += ntohs(xbp->bp_bfs.bh_datalen);
	(*io->i_dp->dt_strategy)(io, WRITE);
	_set_timer(0);
	_timer_jmpbuf = transact_buf;
	_set_timer(BFS_REXMIT);
	while (1) {
		io->i_ma = (char *)rbp;
		io->i_cc = sizeof(*rbp);
		do {
			_scandevs();	/* allow for console interrupts */
		} while ((cc = (*io->i_dp->dt_strategy)(io, READ)) <= 0);
		/*
		 * toss unsolicited packets
		 */
		if (!ok_bfs(xbp, rbp, rsp_type, cc))
			continue;
		if ((rsp_type != BFSPT_ENQRPY) &&
		    (bcmp(&cei(io)->ei_srcaddr, &cei(io)->ei_dstaddr,
		    sizeof(cei(io)->ei_dstaddr)))) {
			printf("unsolicited bfs reply from host %s\n",
			    rbp->bp_bfs.bh_server);
			continue;
		}
		_set_timer(0);
		/*
		 * re-establish previous timer
		 */
		_timer_jmpbuf = old_jmpbuf;
		_set_timer(old_timer);
		return(0);
	}
}

/*
 * mk_bfs -- setup bfs encapsulation on packet
 */
static void
mk_bfs(bfp, type, filename, offset, datalen, flags)
register struct bfs_packet *bfp;
int type;
char *filename;
u_int offset;
int datalen;
{
	if (strlen(filename) > BFS_MAXPATH)
		_io_abort("Pathname too long for bfs");
		/* doesn't return */
	bfp->bp_bfs.bh_rev = BFSREV;
	bfp->bp_bfs.bh_type = type;
	bfp->bp_bfs.bh_pathlen = htons(strlen(filename)+1);
	bfp->bp_bfs.bh_offset = htonl(offset);
	bfp->bp_bfs.bh_datalen = htons(datalen);
	bfp->bp_bfs.bh_flags = htonl(flags);
	strcpy(bfp->bp_data, filename);
}

/*
 * ok_bfs(xmit_bfs_packet_ptr, recv_bfs_packet_ptr, rsp_type, cc)
 * verify received bfs packet is acceptable reply to request
 */
static int
ok_bfs(xbp, rbp, rsp_type, cc)
register struct bfs_packet *xbp, *rbp;
int rsp_type;
int cc;
{
	extern int Debug;

	if (rbp->bp_bfs.bh_type == BFSPT_ERROR) {
		printf("%s:%s: %s\n", rbp->bp_bfs.bh_server, rbp->bp_data,
		    &rbp->bp_data[ntohs(rbp->bp_bfs.bh_pathlen)]);
		return(0);
	}

	/*
	 * verify filename match, offsets match, response type, and
	 * packet length acceptable
	 */
	if (strcmp(xbp->bp_data, rbp->bp_data)
	    || xbp->bp_bfs.bh_offset != rbp->bp_bfs.bh_offset
	    || rbp->bp_bfs.bh_type != rsp_type
	    || cc < sizeof(struct bfshdr) + ntohs(rbp->bp_bfs.bh_pathlen)
	      + ntohs(rbp->bp_bfs.bh_datalen)) {
		/*
		 * only complain if type not ENQRPY, since
		 * multiple ENQRPY's can arrive
		 */
		if (rbp->bp_bfs.bh_type != BFSPT_ENQRPY || (Debug & DBG_BFS)) {
			printf("%s:%s bad packet received\n",
			    rbp->bp_bfs.bh_server, rbp->bp_data);
#ifndef PROM
			if (strcmp(xbp->bp_data, rbp->bp_data))
			    printf("  mismatch on filename \n");
			if (xbp->bp_bfs.bh_offset != rbp->bp_bfs.bh_offset)
			    printf("  mismatch on offset \n");
			if (rbp->bp_bfs.bh_type != rsp_type)
			    printf("  mismatch on type \n");
			if (cc < sizeof(struct bfshdr)
				   + ntohs(rbp->bp_bfs.bh_pathlen)
				   + ntohs(rbp->bp_bfs.bh_datalen))
			    printf("  size too small \n");
#endif !PROM
		}
		return(0);
	}
	return(1);
}
