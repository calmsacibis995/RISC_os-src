#ident "$Header: nfs.c,v 1.2 90/04/16 18:35:03 beacker Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
 * RARP/TFTP prom bootstrap.
 *
 * history
 * 9/17/89	monte pickard (uni-xperts)	created.
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
#include "saio/mbuf.h"
#include "saio/tpd.h"
typedef	u_long	iaddr_t;
#include "netinet/tftp.h"
#include "saio/bootp.h"
#include "machine/cpu_board.h"
#include "mipsif/if_enp.h"

#define cei(x)	((struct ether_info *)(x->i_ino_dir))
#define	cei_if		ei_acp->ac_if
#define	cei_enaddr	ei_acp->ac_enaddr

#define	streq(a,b)	(strcmp(a,b)==0)

extern char *index();
extern char *getenv();
extern char *_get_iobbuf();
extern struct in_addr inet_addr();
extern struct in_addr inet_makeaddr();

extern char *inet_ntoa();
extern char *ether_sprintf();

extern char **environ;
extern struct string_list environ_str;
extern struct string_list exec_environ;

extern void tftpack();
extern void tftpabort();

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
#define dprintf(x)	(Debug? printf x : printf x )
#endif /*MONTE*/

static u_char etherbroadcastaddr[6] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
/*
 * The RARP timeout can be relatively short, since it only does
 * a limited amount of IO.  The TFTP timeout needs to be long,
 * since it may be doing slow things like rewinding cartridge
 * tapes.
 */
#define RARP_TIMEOUT	5	/* 5 seconds */
#define TFTP_TIMEOUT	30	/* 30 seconds */
#define RARP_TRIES	4
#define TFTP_TRIES	4

/*
 * Private information maintained by RARP/TFTP
 */

/* TFTP connection state */

extern struct tftp_state tftp_st[MAX_IFCS];

unsigned long local_inet;
char gen_filename[16];
#ifdef MONTE
unsigned mdpspecial = 0;
unsigned mdponce = 0;
mdpprintmsg(m)
register struct mbuf *m;
{
	register int len = m->m_len;
	register char *mptr = mtod(m,char *);

	if ( mdponce || (len<=0) ) {
		return;
	}
	mdponce++;
	printf("msg starts at %x for %x\n",mptr,len);
	while( len-- ) {
		printf("%x,",*mptr++);
	}
	printf("\n");
}
#endif /*MONTE*/

unsigned long response_host;

/*
 * _nfsinit
 */
_nfsinit()
{
	struct iob *io;
	register struct tftp_state *tsp;

	response_host = 0;

	if (sizeof (struct ether_info) > IOB_INODE)
		_io_abort("bad size in iob");

	bzero((char *)tftp_st, sizeof (tftp_st));
	for (tsp = tftp_st; tsp < &tftp_st[MAX_IFCS]; tsp++)
		tsp->ts_blkno = -1;
}

extern unsigned tftp_broadcast;
extern unsigned tftp_noerror;
struct mbuf *have_rarp_response;
unsigned char inet_ascii[16];

/*
 * _nfsopen(io, file, flags) -- find an available server for "file"
 */
_nfsopen(io, filename, flags)
register struct iob *io;
char *filename;
{
	register char *host;	/* name of server */
	register char *file;	/* file name to request from server */
	register struct sockaddr_in *sin;
	char *member;		/* name within file for TPD files */
	char *cp;
	char buf[512];
	char gfilename[16];
	struct tftp_state *tsp;
	struct in_addr myiaddr;
	int sverrno = 0;	/* io->i_errno value to be returned */
	int net;
	unsigned need_inet_addr = 0;
	char *get_inet_addr();
	void get_netfile();

	io->i_buf = NULL;

	dprintf(("nfsopen: ctlr %d, file %s\n", io->i_ctlr, 
			((filename  && *filename)?filename:"NULL")));
	
	if (io->i_ctlr >= MAX_IFCS) {
		printf("cntlr # too large: %x\n", io->i_ctlr);
		goto errout;
	}

 	tsp = &tftp_st[io->i_ctlr];
	tsp->ts_blkno = -1;
	tsp->ts_ent.te_lbn = 0;

	if (++flags & F_WRITE) {
		printf("NO WRITE\n");
		goto errout;
	}
	if (strlen(filename) > sizeof buf - 2) {
		printf("NAME too long\n");
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
			printf("parenthesis mismatch\n");
			goto errout;
		}
		*member++ = '\0';
		*cp = '\0';
	}
	/*
	 *  Decide whether or not we need to issue a reverse ARP
	 *  request to get the local host internet address.
	 */
	need_inet_addr = 0;
	if ((cp = getenv("netaddr")) == NULL ) {
		need_inet_addr++;
	} else {
		if ( !valid_inet_addr(cp) ) {
			need_inet_addr++;
		}
	}
	sin = (struct sockaddr_in *)&(cei(io)->ei_acp->ac_if.if_addr);
	if ( need_inet_addr ) {
		if ( (cp = get_inet_addr(io,&response_host)) == NULL ) {
			printf("Can't estab. inet address\n");
			goto errout;
		}
		strcpy(inet_ascii,cp);
		dprintf(("Got inetnet address from %x\n", response_host));
		replace_str("netaddr", inet_ascii, environ);
		replace_str("netaddr", inet_ascii, &exec_environ);
		sin->sin_addr = inet_addr(inet_ascii);
	} else {
		strcpy(inet_ascii,cp);
	}
	dprintf(("nfsopen: internet address %s\n", inet_ascii));
	myiaddr = inet_addr(inet_ascii);
	local_inet = myiaddr.s_addr;
	if ( streq(filename,"unix") ) {
		(void)get_netfile(&gfilename[1]);
		gfilename[0] = '/';
		file = gfilename;
	}
	dprintf(("nfsopen: host %s, file %s %s %s\n",
		host?host:"<NULL>", file,
		member ? ", member " : "",
		member ? member : ""));
	if ( (net = inet_netof(myiaddr)) < 0) {
		printf("$netaddr incorrect: %s", inet_ascii);
		goto errout;
	}

	/*
	 * Put the server information into the IO structure.
	 */
	cei(io)->ei_dstaddr.sin_family = AF_INET;
	cei(io)->ei_dstaddr.sin_addr.s_addr = response_host;
	cei(io)->ei_dstaddr.sin_port = 0;

	/* put file name into io structure */
	strcpy(cei(io)->ei_filename, file);

	/*
	 * Call the TFTP connect routine to open the file.
	 * (Directed TFTP request to responding server, if one).
	 */
	tftp_noerror++;		/* errors OK while trying to find server */
	switch(response_host) {

	default:
		if (tftpconnect(io) >= 0) {
			break;
		}
		dprintf(("nfsopen: directed tftp fails\n"));
		/* fall thru */

	case 0:
		cei(io)->ei_dstaddr.sin_family = AF_INET;
		cei(io)->ei_dstaddr.sin_addr = 
				inet_makeaddr(net, INADDR_BROADCAST);
		cei(io)->ei_dstaddr.sin_port = 0;
		tftp_broadcast++;
		if (tftpconnect(io) >= 0) {
			break;
		}
		dprintf(("nfsopen: INADDR_BROADCAST tftp fails\n"));
		cei(io)->ei_dstaddr.sin_family = AF_INET;
		cei(io)->ei_dstaddr.sin_addr = inet_makeaddr(net, INADDR_ANY);
		cei(io)->ei_dstaddr.sin_port = 0;
		tftp_broadcast++;
		if (tftpconnect(io) >= 0) {
			break;
		}
		dprintf(("nfsopen: INADDR_ANY tftp fails\n"));
		goto errout;
	}

	printf("Obtaining %s from server %s\n", file, 
			inet_ntoa(cei(io)->ei_dstaddr.sin_addr.s_addr));

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
 * _nfsread(io, buffer, cnt) -- read cnt bytes from TFTP
 */
int
_nfsread(io, buf, count)
struct iob *io;
char *buf;
int count;
{
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];
	register int cc;
	register int offset;
	int savecount;
	
	dprintf(("nfsread: ctlr %d, buf %x, count %d\n",
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
		_io_abort("Backward seek on nfs device (not supported)");
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
 * _nfsclose(io) -- close a BOOTP connection
 */
_nfsclose(io)
register struct iob *io;
{
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];

	dprintf(("nfsclose: ctlr %d\n", io->i_ctlr));
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
void get_netfile(fname)
register char *fname;
{
	register char *p;

	p = (char *)&local_inet;
#define	UC(b)	(((int)b)&0xff)
	sprintf(fname, "%.2x%.2x%.2x%.2x", 
			UC(p[0]), UC(p[1]), UC(p[2]), UC(p[3]));
}

char *
get_inet_addr(io,responder)
register struct iob *io;
unsigned long *responder;
{
	register struct mbuf *m;
	register struct arpcom *ac;
	register struct ether_header *eh;
	register struct ether_arp *ea;
	volatile int tries;
	struct sockaddr sa;
	jmp_buf arp_buf;
	int *old_jmpbuf;
	int old_timer;
	extern int *_timer_jmpbuf;

	/* get arpcom address, else return null is none. */

	if ( (ac = cei(io)->ei_acp) == NULL ) {
		printf("Cannot establish network path\n");
		return(NULL);
	}
	/*
	 * must save currently running timer since bfs has one running
	 * when arp is called
	 */
	old_jmpbuf = _timer_jmpbuf;
	old_timer = old_jmpbuf ? _set_timer(0) : 0;

	/*
	 * set timer and try 5 times
	 */
	tries = 0;
	if (setjmp(arp_buf)) {
		if (++tries >= RARP_TRIES) {
			_io_abort("RARP couldn't resolve network address");
		}
	}

	if ((m = _m_get(M_DONTWAIT, MT_DATA)) == NULL)
		_io_abort("out of mbufs");
	m->m_len = sizeof *ea + sizeof *eh;
	m->m_off = MMAXOFF - m->m_len;
	ea = mtod(m, struct ether_arp *);
	eh = (struct ether_header *)sa.sa_data;
	bzero((caddr_t)ea, sizeof (*ea));
	bcopy((caddr_t)etherbroadcastaddr, (caddr_t)eh->ether_dhost,
	   sizeof (etherbroadcastaddr));
	eh->ether_type = ETHERPUP_RARPTYPE;	/* if_output will swap */
	ea->arp_hrd = htons(ARPHRD_ETHER);
	ea->arp_pro = htons(ETHERPUP_IPTYPE);
	ea->arp_hln = sizeof ea->arp_sha;	/* hardware address length */
	ea->arp_pln = sizeof ea->arp_spa;	/* protocol address length */
	ea->arp_op = htons(RARPOP_REQUEST);
	bcopy((caddr_t)ac->ac_enaddr, (caddr_t)ea->arp_sha,
	   sizeof (ea->arp_sha));
	bcopy((caddr_t)ac->ac_enaddr, (caddr_t)ea->arp_tha,
	   sizeof (ea->arp_tha));
	sa.sa_family = AF_UNSPEC;
	have_rarp_response = NULL;
#ifdef MONTE
	mdpprintmsg(m);
	mdponce = 0;
	printf("calling %x\n",ac->ac_if.if_output);
#endif /*MONTE*/
	(void) (*ac->ac_if.if_output)(&ac->ac_if, m, &sa);

	_set_timer(0);
	_timer_jmpbuf = arp_buf;
	_set_timer(RARP_TIMEOUT);
	while (have_rarp_response == NULL) {
		_scandevs();
	}
	_set_timer(0);
	_timer_jmpbuf = old_jmpbuf;
	_set_timer(old_timer);
#ifdef MONTE
	mdponce = 0;
	mdpprintmsg(have_rarp_response);
#endif /*MONTE*/
	ea = mtod(have_rarp_response, struct ether_arp *);
	bcopy(ea->arp_spa,responder,ea->arp_pln);
	bcopy(ea->arp_tpa,&local_inet,ea->arp_pln);
	dprintf(("get_inet_addr local_inet = %x from %x\n",
			local_inet,ea->arp_tpa));
	return(inet_ntoa(local_inet));
}
valid_inet_addr(internet_addr)
char *internet_addr;
{
	struct in_addr myiaddr;

	myiaddr = inet_addr(internet_addr);
	if ( myiaddr.s_addr == 0 ) {
		return(0);
	} else {
		return(1);
	}
}
/*
 * Called from network device recv interrupt routines when ether
 * packet type ETHERPUP_RARP is received.  
 */
_rarpinput(ac, m)
struct arpcom *ac;
struct mbuf *m;
{
	register struct ether_arp *ea;
	struct ether_header *eh;
	struct arptab *at = 0;  /* same as "merge" flag */
	struct sockaddr_in sin;
	struct sockaddr sa;
	struct in_addr isaddr,itaddr,myaddr;

#ifdef DEBUG
	printf("rarpinput\n");
#endif
	if (m->m_len < sizeof *ea)
		goto out;
	if (have_rarp_response) {
		/* already have response */
		goto out;
	}
	have_rarp_response = m;
	return;
out:
	_m_freem(m);
	return;
}
