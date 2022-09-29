#ident "$Header: bootp.c,v 1.6 90/03/06 15:15:30 zach Exp $"
/* $Copyright$ */

/*	bootp.c	1.1	12/17/85	*/

/*
 * BOOTP/TFTP prom bootstrap.
 *
 * history
 * 12/17/85	croft	created.
 * 7/19/88	modified for MIPS by rpharris.
 */

#define DEBUG	1	/* noise compiled in, but controlled by 'Debug' */
#undef DEBUG

#include "sys/errno.h"
#include "sys/param.h"
#include "netinet/in.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "saio/socket.h"
#include "saio/setjmp.h"
#include "saio/arp.h"
#include "saio/ei.h"
#include "saio/debug.h"
#include "saio/tpd.h"
typedef	u_long	iaddr_t;
#include "netinet/tftp.h"
#include "saio/bootp.h"
#include "machine/cpu_board.h"
#include "mipsif/if_enp.h"

#define cei(x)	((struct ether_info *)(x->i_ino_dir))
#define	cei_if		ei_acp->ac_if
#define	cei_enaddr	ei_acp->ac_enaddr

extern char *index();
extern char *getenv();
extern char *_get_iobbuf();
extern struct in_addr inet_addr();
extern struct in_addr inet_makeaddr();

extern char *inet_ntoa();
extern char *ether_sprintf();

extern char **environ;

extern void tftpack();
extern void tftpabort();
extern (*get_tod[])();			/* Pointer to machdep get_tod */

#ifndef MONTE
# ifdef DEBUG
extern int Debug;
# define dprintf(x)	(Debug?printf x :0)
# define Dprintf(n,x)	(Debug>n?printf x :0)
# else  !DEBUG
# define dprintf(x)
# define Dprintf(n,x)
# endif !DEBUG
#else /*MONTE*/
extern int Debug;
# define dprintf(x)	(Debug?printf x : printf x)
#endif /*MONTE*/

/*
 * The BOOTP timeout can be relatively short, since it only does
 * a limited amount of IO.  The TFTP timeout needs to be long,
 * since it may be doing slow things like rewinding cartridge
 * tapes.
 */
#define BOOTP_TIMEOUT	5	/* 5 seconds */
#define TFTP_TIMEOUT	30	/* 30 seconds */
#define BOOTP_TRIES	4
#define TFTP_TRIES	4

/*
 * Private information maintained by BOOTP/TFTP
 */
int bootp_xid;			/* BOOTP transaction ID */

/* TFTP connection state */

extern struct tftp_state tftp_st[MAX_IFCS];
extern unsigned tftp_broadcast;
extern unsigned tftp_noerror;

/*
 * _bootpinit
 */
_bootpinit()
{
	struct bootp *bp;
	struct iob *io;
	register struct tftp_state *tsp;

	if (sizeof (struct ether_info) > IOB_INODE)
		_io_abort("bad size in iob bootp(%x,%x)",
				sizeof(struct ether_info),IOB_INODE);

	if (sizeof (bp->bp_file) > sizeof (cei(io)->ei_filename))
		_io_abort("bad name size in ether info");

#ifndef STANDALONE
	if (sizeof (struct tftpdmsg) > MAXBSIZE)
		_io_abort("message size bigger than _iob_buf size");
#endif

	bzero((char *)tftp_st, sizeof (tftp_st));
	for (tsp = tftp_st; tsp < &tftp_st[MAX_IFCS]; tsp++)
		tsp->ts_blkno = -1;
}

extern char netaddr_default[];

/*
 * _bootpopen(io, file, flags) -- find an available server for "file"
 */
_bootpopen(io, filename, flags)
register struct iob *io;
char *filename;
{
	register char *host;	/* name of server */
	register char *file;	/* file name to request from server */
	char *member;		/* name within file for TPD files */
	register char *cp;
	char buf[512];
	struct tftp_state *tsp;
	struct in_addr myiaddr;
	struct bootp xbp;	/* transmit packet */
	struct bootp rbp;	/* receive packet */
	int sverrno = 0;	/* io->i_errno value to be returned */
	int net;

	io->i_buf = NULL;

	dprintf(("bootpopen: ctlr %d, file %s\n", io->i_ctlr, filename));
	
	if (io->i_ctlr >= MAX_IFCS) {
		printf("controller number too large: %d\n", io->i_ctlr);
		goto errout;
	}

 	tsp = &tftp_st[io->i_ctlr];
	tsp->ts_blkno = -1;
	tsp->ts_ent.te_lbn = 0;

	if (++flags & F_WRITE) {
		printf("writing not supported\n");
		goto errout;
	}
	ASSERT(filename != NULL);
	if (strlen(filename) > sizeof buf - 2) {
		printf("Filename too long\n");
		goto errout;
	}
	strcpy(buf, filename);

	/*
	 * Parse file name to separate host.  If the user wants a pathname
	 * that has a ":" in it, but no host name, then prefix the
	 * name with just ":" which has the desired effect.
	 */
	if ((file = index(buf, ':')) == NULL) {
		host = NULL;
		file = buf;
	} else {
		host = buf;
		*file++ = '\0';
	}

	/*
	 * Now check for a member name within the file name
	 */
#define LEFT_PAREN	0x28	/* so vi can match parens */
#define RIGHT_PAREN	0x29
	if ((member = index(file, LEFT_PAREN)) != NULL) {
		if ((cp = index(member, RIGHT_PAREN)) == NULL) {
			printf("Unmatched parenthesis in file name\n");
			goto errout;
		}
		*member++ = '\0';
		*cp = '\0';
	}

	if (host != NULL && strlen(host) >= sizeof (xbp.bp_sname)) {
		printf("host name too long: %s\n", host);
		goto errout;
	}

	if (strlen(file) >= sizeof (xbp.bp_file)) {
		printf("file name too long: %s\n", file);
		goto errout;
	}

	dprintf(("bootpopen: host %s, file %s %s %s\n",
		host?host:"<NULL>", file,
		member ? ", member " : "",
		member ? member : ""));
	
#ifdef notdef
	/*
	 * Decide whether it is necessary to invoke BOOTP.
	 * BOOTP is not required if all the following conditions
	 * are met:
	 *
	 * 1) the internet address of this system is already known
	 * 2) the internet addresses of the server and gateway
	 *    through which to boot are already known
	 */
	myiaddr.s_addr = 0;
	if ((cp = getenv("netaddr")) != NULL && *cp != '\0') {
		struct in_addr iaddr;

		/*
		 * If Internet address is set in the environment, then
		 * configure the interface for that IP address.
		 */
		myiaddr = inet_addr(cp);
		if (!strcmp(cp, netaddr_default)) {
			printf("%s%s%s",
"Warning: 'netaddr' is set to the default address ", netaddr_default,
".\nUse 'setenv' to reset it to an Internet address on your network.\n");
		}
		if (ntohl(myiaddr.s_addr) == -1) {
			printf("$netaddr is not a valid Internet address\n");
			sverrno = EADDRNOTAVAIL;
			goto errout;
		}
		if (DEVIOCTL(io, NIOCSIFADDR, (caddr_t)&myiaddr) < 0) {
			printf("Unable to set interface address\n");
			goto errout;
		}

		if ((cp = getenv("srvaddr")) == NULL || *cp == '\0') {
			dprintf(("$srvaddr not set\n"));
			goto do_bootp;
		}
		iaddr = inet_addr(cp);
		if (ntohl(iaddr.s_addr) == -1) {
			dprintf(("$srvaddr not valid\n"));
			goto do_bootp;
		}

		cei(io)->ei_dstaddr.sin_family = AF_INET;
		cei(io)->ei_dstaddr.sin_addr.s_addr = iaddr.s_addr;
		cei(io)->ei_dstaddr.sin_port = 0;
		cei(io)->ei_gateaddr = cei(io)->ei_dstaddr;
		
		if ((cp = getenv("gateaddr")) != NULL && *cp != '\0') {
			iaddr = inet_addr(cp);
			if (ntohl(iaddr.s_addr) == -1) {
				dprintf(("$gateaddr not valid\n"));
				goto do_bootp;
			}
			cei(io)->ei_gateaddr.sin_addr.s_addr = iaddr.s_addr;
		}
		dprintf(("skipping BOOTP ...\n"));
		strcpy(cei(io)->ei_filename, file);
		strcpy(rbp.bp_sname, getenv("srvaddr"));
		goto after_bootp;
	}

do_bootp:
	cei(io)->ei_dstaddr.sin_family = AF_INET;
	cei(io)->ei_dstaddr.sin_addr.s_addr = INADDR_BROADCAST;
	cei(io)->ei_dstaddr.sin_port = htons((u_short)IPPORT_BOOTPS);
	cei(io)->ei_gateaddr = cei(io)->ei_dstaddr;
#endif notdef

	/* make an internet broadcast address */
	if ( (cp = getenv("netaddr")) == NULL ) {
		myiaddr = inet_addr("0.0.0.0");
	} else {
		myiaddr = inet_addr(cp);
		if ( (net = inet_netof(myiaddr)) < 0) {
			printf("$netaddr incorrect: %s", getenv("netaddr"));
			goto errout;
		}
	}
	cei(io)->ei_dstaddr.sin_family = AF_INET;
	cei(io)->ei_dstaddr.sin_addr = inet_makeaddr(net, INADDR_ANY);
	cei(io)->ei_dstaddr.sin_port = htons((u_short)IPPORT_BOOTPS);

	if (DEVIOCTL(io, NIOCBIND, htons((u_short)IPPORT_BOOTPC)) < 0) {
		printf("bootp bind failed\n");
		goto errout;
	}

	/*
	 * Format the BOOTP packet
	 */
	mk_bootp(io, &xbp, host, file, myiaddr.s_addr);

	/*
	 * If the IP address of this system is not yet known, then
	 * we must put the interface in promiscuous receive mode
	 * so that it won't drop the BOOTP response message.
	 */
	if (xbp.bp_ciaddr == 0)
		cei(io)->cei_if.if_flags |= IFF_PROMISC;

	dprintf(("sending BOOTP request: ciaddr %s, chaddr %s\n",
		inet_ntoa(xbp.bp_ciaddr), ether_sprintf(xbp.bp_chaddr)));

	if (bootp_transaction(io, &xbp, &rbp) < 0) {
		printf("No server for %s\n", filename);
		sverrno = ENXIO;
		if (cei(io)->ei_registry > 0)
			_free_socket(cei(io)->ei_registry);
		goto errout;
	}

	dprintf(("BOOTP reply from %s(%s): file %s, myiaddr %s, giaddr %s\n",
		rbp.bp_sname, inet_ntoa(rbp.bp_siaddr),
		rbp.bp_file, inet_ntoa(rbp.bp_yiaddr),
		inet_ntoa(rbp.bp_giaddr)));

	/*
	 * If the bp_yiaddr field is not zero, then we don't yet
	 * know our own IP address.  Configure the interface with
	 * the new address.
	 */
	if (rbp.bp_yiaddr != 0) {
/* tbd
		if (DEVIOCTL(io, NIOCSIFADDR, &rbp.bp_yiaddr) < 0) {
			printf("Unable to set interface address\n");
			goto errout;
		}
 */
		printf("Setting $netaddr to %s (from server %s)\n",
			inet_ntoa(rbp.bp_yiaddr), rbp.bp_sname);
		replace_str("netaddr", inet_ntoa(rbp.bp_yiaddr), environ);
		/*
		 * We can turn off promiscuous mode now, since we
		 * know who we are.
		 */
		cei(io)->cei_if.if_flags &= ~IFF_PROMISC;
	}

	/*
	 * Get the server and gateway IP addresses from the message
	 */
	cei(io)->ei_dstaddr.sin_family = AF_INET;
	cei(io)->ei_dstaddr.sin_addr.s_addr = rbp.bp_siaddr;
	cei(io)->ei_dstaddr.sin_port = 0;
#ifdef notdef
	cei(io)->ei_gateaddr = cei(io)->ei_dstaddr;
	if (rbp.bp_giaddr != 0)
		cei(io)->ei_gateaddr.sin_addr.s_addr = rbp.bp_giaddr;
#endif notdef
	
	/*
	 * The file name may be NULL, meaning that we requested a
	 * particular server and he didn't have the file.
	 */
	if (strlen(rbp.bp_file) == 0) {
		sverrno = ENOENT;
		printf("File %s not found on server %s\n", file, host);
		goto errout;
	}

	/*
	 * Store the real file name as returned by the server
	 */
	strcpy(cei(io)->ei_filename, rbp.bp_file);

	/*
	 * Store vendor information somewhere that the OS can find it XXX
	 */

after_bootp:
	/*
	 * Call the TFTP connect routine to open the file.
	 */
	tftp_broadcast = 0;	/* not expecting different host to respond */
	tftp_noerror = 0;	/* report all errors */
	if (tftpconnect(io) < 0) {
		dprintf(("bootpopen: tftp connect fails\n"));
		goto errout;
	}

	printf("Obtaining %s from server %s\n", file, rbp.bp_sname);

	/*
	 * No member name given, so we are done.
	 */
	if (member == NULL) {
		ASSERT(tsp->ts_ent.te_lbn == 0);
		return (0);
	}

	/*
	 * A member name was specified, check the first block
	 * for a TPD format directory.  If the file is not
	 * in TPD format, then complain and return an error.
	 */
	dprintf(("after connect: tftp count %d, offset %d\n",
		tsp->ts_count, tsp->ts_offset));
	if (!is_tpd(tsp->ts_data + tsp->ts_offset)) {
		printf("Remote file %s%s%s format not valid: %s\n",
			host ? host : "", host ? ":" : "", file,
			"first block is not a tape directory"); 
		goto errout;
	}

	/*
	 * Search for the requested member in the directory
	 */
	if (!tpd_match(member,
		(struct tp_dir *)(tsp->ts_data + tsp->ts_offset),
		&tsp->ts_ent)) {

		printf("File %s not found in %s%s%s, directory contains:\n",
			member, host ? host : "", host ? ":" : "", file);
		tpd_list((struct tp_dir *)(tsp->ts_data + tsp->ts_offset));
		goto errout;
	}
	ASSERT(tsp->ts_ent.te_lbn != 0);
	return (0);
	
errout:
	if (io->i_buf != NULL) {
		_free_iobbuf(io->i_buf);
		io->i_buf = NULL;
	}
	/*
	 * Call interface close routine
	 */
	(*io->i_dp->dt_close)(io);
	/*
	 * Set returned error code
	 */
	io->i_errno = (sverrno != 0) ? sverrno : EINVAL;
	
	return (-1);
}

/*
 * _bootpread(io, buffer, cnt) -- read cnt bytes from TFTP
 */
int
_bootpread(io, buf, count)
struct iob *io;
char *buf;
int count;
{
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];
	register int cc;
	register int offset;
	int savecount;
	
	dprintf(("bootpread: ctlr %d, buf %x, count %d\n",
		io->i_ctlr, buf, count));
	ASSERT(0 <= io->i_ctlr && io->i_ctlr < MAX_IFCS);
	ASSERT(SEGSIZE == DEV_BSIZE);
	ASSERT(SEGSIZE == TP_BLKSIZE);	/* so we can mix TFTP blks and tape */
	ASSERT(io->i_buf != NULL);
	ASSERT(tsp->ts_offset <= SEGSIZE);
	/*
	 * Compute current logical position in the file
	 */
	offset = ((tsp->ts_blkno - 1 - tsp->ts_ent.te_lbn) * SEGSIZE) +
		 tsp->ts_offset;

	if (offset < io->i_offset) {

		while (offset < io->i_offset) {
			/*
			 * If we are reading a TPD format file, check that
			 * we are within that file.
			 */
			if (tsp->ts_ent.te_lbn != 0 &&
			    offset >= (signed) tsp->ts_ent.te_nbytes) {
				dprintf(("EOF on seek in TPD file at offset %d, chars %d\n", offset, tsp->ts_ent.te_nbytes));
				return (0);
			}
			if (tsp->ts_count > 0) {
				cc = MIN(io->i_offset - offset, tsp->ts_count);
				tsp->ts_offset += cc;
				tsp->ts_count -= cc;
				offset += cc;
				continue;
			}
			if ((cc = tftpread(io)) <= 0)
				return(cc);
		}

	} else if (offset > io->i_offset) {
		dprintf(("Backward seek to %d (current offset is %d)\n",
			io->i_offset, offset));
		_io_abort("Backward seek on bootp device (not supported)");
	}

	/*
	 * If we are reading a TPD format file, restrict the size
	 * of the read to the remaining data in the subfile.
	 */
	if (tsp->ts_ent.te_lbn != 0)
		count = MIN(count, tsp->ts_ent.te_nbytes - offset);

	savecount = count;
	while (count > 0) {
		/*
		 * Copy characters from the io buffer if any
		 */
		if (tsp->ts_count > 0) {
			cc = MIN(count, tsp->ts_count);
			bcopy(tsp->ts_data + tsp->ts_offset, buf, cc);
			tsp->ts_offset += cc;
			tsp->ts_count -= cc;
			io->i_offset += cc;
			count -= cc;
			buf += cc;
			continue;
		}
		/*
		 * Replenish the buffer
		 */
		if ((cc = tftpread(io)) <= 0)
			break;
	}

	return(savecount - count);
}

/*
 * _bootpclose(io) -- close a BOOTP connection
 */
_bootpclose(io)
register struct iob *io;
{
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];

	dprintf(("bootpclose: ctlr %d\n", io->i_ctlr));
	ASSERT(0 <= io->i_ctlr && io->i_ctlr < MAX_IFCS);
	if (io->i_buf != NULL) {
		_free_iobbuf(io->i_buf);
		io->i_buf = NULL;
	}

	/*
	 * If some data blocks have been received, but EOF not reached,
	 * send "abort" to prevent server from retrying
	 */ 
	if (tsp->ts_blkno > 0 && !tsp->ts_eof) {
		tftpabort(io);
	}
}

/*
 * Format a BOOTP message for transmission
 */
mk_bootp(io, bp, host, file, ciaddr)
register struct iob *io;
register struct bootp *bp;	/* packet buffer */
char *host;		/* requested server name */
char *file;		/* boot file name */
u_long ciaddr;		/* Internet address of client (0 if unknown) */
{
	struct in_addr iaddr;
	char *cp;

	/*
	 * Construct the BOOTP header.
	 */
	bzero(bp, sizeof *bp);
	bp->bp_op = BOOTREQUEST;
	bp->bp_htype = ARPHRD_ETHER;
	bp->bp_hlen = sizeof (struct ether_addr);
	/*
	 * Select a new BOOTP transaction id
	 */
	bootp_xid = getrand(1024, 65535);
	bp->bp_xid = bootp_xid;
	bp->bp_ciaddr = ciaddr;

	bcopy((char *)cei(io)->cei_enaddr, bp->bp_chaddr,
		sizeof (struct ether_addr));
	if (host)
		strcpy(bp->bp_sname, host);
	if (file)
		strcpy(bp->bp_file, file);
}

static jmp_buf transact_buf;

bootp_transaction(io, xbp, rbp)
register struct iob *io;
register struct bootp *xbp;	/* transmit packet buffer */
register struct bootp *rbp;	/* receive packet buffer */
{
	volatile int tries;
	int cc;
	int *old_jmpbuf;
	int old_timer;
	extern int *_timer_jmpbuf;

	/*
	 * save currently running timer
	 */
	old_jmpbuf = _timer_jmpbuf;
	old_timer = old_jmpbuf ? _set_timer(0) : 0;

	tries = 0;
	if (setjmp(transact_buf))
		dprintf(("BOOTP timeout\n"));
	if (++tries > BOOTP_TRIES) {
		/*
		 * re-establish previous timer
		 */
		_set_timer(0);
		_timer_jmpbuf = old_jmpbuf;
		_set_timer(old_timer);
		return(-1);
	}
	/*
	 * set-up device independent iob parameters
	 */
	io->i_ma = (char *)xbp;
	io->i_cc = sizeof(struct bootp);
	DEVWRITE(io);
	_set_timer(0);
	_timer_jmpbuf = transact_buf;
	_set_timer(BOOTP_TIMEOUT);
	while (1) {
		io->i_ma = (char *)rbp;
		io->i_cc = sizeof(*rbp);
		do {
			_scandevs();	/* allow for console interrupts */
		} while ((cc = DEVREAD(io)) <= 0);
		
		if (!bootp_ok(io, rbp, cc))
			continue;
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
 * Verify a received message is valid BOOTP reply
 */
bootp_ok(io, rbp, count)
register struct iob *io;
register struct bootp *rbp;	/* receive packet buffer */
register int count;		/* receive count */
{
	if (count < sizeof (struct bootp)) {
		dprintf(("bootp: msg too short (%d)\n", count));
		return (0);
	}
	if (rbp->bp_op != BOOTREPLY) {
		dprintf(("bootp: not BOOTP reply (%d)\n", rbp->bp_op));
		return (0);
	}
	if (rbp->bp_xid != bootp_xid) {
		dprintf(("bootp: wrong TXID (got %d, expected %d)\n",
			rbp->bp_xid, bootp_xid));
		return (0);
	}
	if (bcmp(rbp->bp_chaddr, (char *)cei(io)->cei_enaddr,
		sizeof (struct ether_addr)) != 0) {

		dprintf(("bootp: wrong ether address (%s)\n",
			ether_sprintf(rbp->bp_chaddr)));
		return (0);
	}
	return (1);
}

/*
 * Make up a sort-of-random number.
 *
 * Note that this is not a general purpose routine, but is geared
 * toward the specific application of generating an integer in the
 * range of 1024 to 65k.
 */
getrand(lb, ub)
int lb;		/* lower bound */
int ub;		/* upper bound */
{
	register int secs;
	register int rand;

	/*
	 * Read the real time clock which returns the number of
	 * seconds from some epoch.
	 */
	secs = (*MACHDEP(get_tod))();

	/*
	 * Combine different bytes of the word in various ways
	 */
	rand  = (secs & 0xff00) >> 8;
	rand *= (secs & 0xff0000) >> 16;
	rand += (secs & 0xff);
	rand *= (secs & 0xff000000) >> 24;
	rand >>= (secs & 7);

	/*
	 * Now fit the range
	 */
	ub -= lb;
	rand = rand % ub;
	rand += lb;

	dprintf(("getrand returns %d\n", rand));

	return (rand);
}
/* tbd: put in libc.c */
/*
 * Convert network-format internet address
 * to base 256 d.d.d.d representation.
 */
char *
inet_ntoa(in)
	struct in_addr in;
{
	static char b[18];
	register char *p;

	p = (char *)&in;
#define	UC(b)	(((int)b)&0xff)
	sprintf(b, "%d.%d.%d.%d", UC(p[0]), UC(p[1]), UC(p[2]), UC(p[3]));
	return (b);
}
/* should be in libc?? */
/*
 * Convert Ethernet address to printable representation.
 */
char *
ether_sprintf(ap)
	register u_char *ap;
{
	static char etherbuf[18];

#define	C(i)	(*(ap+i) & 0xff)
	sprintf(etherbuf, "%x:%x:%x:%x:%x:%x",
		C(0), C(1), C(2), C(3), C(4), C(5));
#undef	C
	return (etherbuf);
}
/* should be added to tpd.c */
tpd_match(name, td, tep)
register char *name;
struct tp_dir *td;
struct tp_entry *tep;
{
	register struct tp_entry *te;
	
	for (te = td->td_entry; te < &td->td_entry[TP_NENTRIES]; te++) {
		if (strncmp(name, te->te_name, TP_NAMESIZE) == 0) {
			*tep = *te;
			return(1);
		}
	}

	return(0);
}

tpd_list(td)
struct tp_dir *td;
{
	register struct tp_entry *te;

	for (te = td->td_entry; te < &td->td_entry[TP_NENTRIES]; te++) {
		if (te->te_nbytes)
			printf("\t%s\t%d bytes\n", te->te_name, te->te_nbytes);
	}
}
assfail(a, f, l)
register char *a, *f;
{
	/*	Save all registers for the dump since crash isn't
	 *	very smart at the moment.
	 */
	
	register int	r6, r5, r4, r3;

	printf("assertion failed: %s, file: %s, line: %d",
		a, f, l);
	_exit(1);
}
