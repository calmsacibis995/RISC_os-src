#ident "$Header: tftp.c,v 1.2 90/04/16 18:35:49 beacker Exp $"
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
 * TFTP - trivial file transfer
 *
 * history
 * 10/05/89	monte pickard (uni-xperts) created
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
/*MONTEextern (*get_tod[])();			/* Pointer to machdep get_tod */

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
extern int bootp_xid;			/* BOOTP transaction ID */

/* TFTP connection state */

struct tftp_state tftp_st[MAX_IFCS];
unsigned tftp_broadcast;
unsigned tftp_noerror;

static jmp_buf transact_buf;
/*
 * TFTP connect
 *
 * Called from bootpopen to initiate a TFTP pseudo-connection.
 * Sends RRQ (Read ReQuest) packet to the server and waits for the response.
 */
tftpconnect(io)
register struct iob *io;
{
	struct tftpdmsg msg;	/* max TFTP packet */
	register struct tftphdr *thp = &msg.th;
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];
	register int sendcount;	/* TFTP message size */
	int recvcount;
	int tries;
	register u_char *cp;

	tftp_broadcast = 0;
	tftp_noerror = 0;

	ASSERT(0 <= io->i_ctlr && io->i_ctlr < MAX_IFCS);
	io->i_buf = _get_iobbuf();
	tsp->ts_count = 0;
	tsp->ts_data = io->i_buf;
	tsp->ts_offset = 0;

	/*
	 * Initialize the TFTP header for RRQ packet.
	 */
	thp->th_opcode = htons(RRQ);
	sendcount = sizeof *thp;
	strcpy(thp->th_stuff, cei(io)->ei_filename);	/* set filename */
	for (cp = (u_char *)thp->th_stuff ; *cp ; cp++, sendcount++);
	cp++, sendcount++;
	strcpy(cp, "octet");			/* binary transfer mode */
	sendcount += 6;				/* 6 = strlen("octet") + 1 */
	tries = 0;
	
	/*
	 * Set the destination port address to the TFTP server port
	 */
	cei(io)->ei_dstaddr.sin_port = htons((u_short)IPPORT_TFTP);

	dprintf(("TFTP: RRQ for file %s to %s\n",
		cei(io)->ei_filename,
		inet_ntoa( ntohl( cei(io)->ei_dstaddr.sin_addr.s_addr ))));

	/*
	 * send "RRQ" until a valid block is received or have to give up.
	 */
	for (;;) {
		/*
		 * Choose a different port on each attempt to avoid
		 * confusion with stale replies.
		 */
		if (cei(io)->ei_registry >= 0) {
			_free_socket(cei(io)->ei_registry);
			cei(io)->ei_registry = -1;
		}
		/*
		 * Check for zero bootp_xid in case the BOOTP step
		 * was skipped.
		 */
		if (bootp_xid == 0)
			bootp_xid = getrand(1024, 65535);
		tsp->ts_myport = ++bootp_xid & 0x3FFF;
		if (DEVIOCTL(io, NIOCBIND, htons(tsp->ts_myport)) < 0) {
			printf("tftp bind failed\n");
			return(-1);
		}

		if (++tries > TFTP_TRIES) {
			dprintf(("TFTP: no response from server\n"));
			io->i_errno = ETIMEDOUT;
			return(-1);
		}

		io->i_cc = sendcount;
		io->i_ma = (char *)&msg;
		DEVWRITE(io);

		/*
		 * Return if get positive number of bytes or negative
		 * return value indicating an error.
		 */
		if ((recvcount = tftprecv(io)) != 0)
			break;
		/*
		 * No data bytes returned.  If the last block has been
		 * received, this means the file was zero length!
		 */
		if (tsp->ts_eof)
			break;
	}
	return(recvcount);
}

/*
 * TFTP read routine
 *
 * Return value is number of new data bytes read.
 */
tftpread(io)
register struct iob *io;
{
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];
	int recvcount, tries;
	ASSERT(0 <= io->i_ctlr && io->i_ctlr < MAX_IFCS);
	/*
	 * Connection should have been established by tcpopen
	 */
	if (tsp->ts_blkno < 0) {
		printf("tftpread: internal error, no connection\n");
		return(-1);
	}

	/*
	 * If EOF has already been received, just return.
	 */
	if (tsp->ts_eof)
		return(0);

	dprintf(("TFTP: waiting for blk %d\n", tsp->ts_blkno+1));
	tries = 0;

	/*
	 * Wait for the next block.  If it doesn't arrive, resend
	 * the last acknowledgement.
	 */
	for (;;) {
		/*
		 * Receive until a good reply comes in or timeout.
		 */
		if ((recvcount = tftprecv(io)) != 0)
			break;

		/*
		 * No data bytes returned.  Check for the case in which
		 * the last block contains zero data bytes and just
		 * return instead of retrying.
		 */
		if (tsp->ts_eof)
			break;
		
		if (++tries > TFTP_TRIES) {
			dprintf(("Server timeout ... Giving up.\n"));
			io->i_errno = ETIMEDOUT;
			return(-1);
		}

		/*
		 * Poke the server by sending another ACK for the last block
		 */
		tftpack(io);
	}
	return (recvcount);	/* return number of data bytes received */
}

/*
 * TFTP receive.
 *
 * Receive messages until we get a valid TFTP reply or time out.
 * ACKs are sent to valid TFTP data blocks.
 *
 * Return value is the actual number of TFTP data bytes received
 * or -1 if error.
 */
tftprecv(io)
register struct iob *io;
{
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];
#ifdef STANDALONE
	struct tftpdmsg msg;
	register struct tftpdmsg *rmp = &msg;
#else
	register struct tftpdmsg *rmp = (struct tftpdmsg *)io->i_buf;
#endif
	register int recvcount;
	int cc;
	int *old_jmpbuf;
	int old_timer;
	extern int *_timer_jmpbuf;
	ASSERT(0 <= io->i_ctlr && io->i_ctlr < MAX_IFCS);
	/*
	 * save currently running timer
	 */
	old_jmpbuf = _timer_jmpbuf;
	old_timer = old_jmpbuf ? _set_timer(0) : 0;

	if (setjmp(transact_buf)) {
		dprintf(("TFTP timeout\n"));
		/*
		 * re-establish previous timer
		 */
		_set_timer(0);
		_timer_jmpbuf = old_jmpbuf;
		_set_timer(old_timer);
		/*
		 * let higher level handle the retry
		 */
		return(0);
	}

	_set_timer(0);
	_timer_jmpbuf = transact_buf;
	_set_timer(TFTP_TIMEOUT);
	while (1) {
		io->i_cc = sizeof (*rmp);
		io->i_ma = (char *)rmp;
		do {
			_scandevs();	/* allow for console interrupts */
		} while ((recvcount = DEVREAD(io)) <= 0);
		
		/*
		 * Check the packet
		 */
		recvcount = tftp_ok(io, rmp, recvcount);
		/*
		 * If it isn't the one we are awaiting, loop again.
		 * Note that we will get zero data bytes
		 * as a legitimate receive on the last block of a file
		 * whose length is a multiple of 512.  When this happens,
		 * the EOF flag will be set.
		 */
		if (recvcount == 0 && !tsp->ts_eof)
			continue;

		/*
		 * cancel our timer and re-establish previous timer
		 */
		_set_timer(0);
		_timer_jmpbuf = old_jmpbuf;
		_set_timer(old_timer);
		return(recvcount);
	}

}

/*
 * Check the validity of a TFTP message
 *
 * Return values:
 *	0 - not a TFTP packet or a TFTP error that can be retried
 *    < 0 - non-recoverable TFTP error (e.g. abort from server)
 *    > 0 - number of TFTP data bytes in the packet
 */
tftp_ok(io, rmp, recvcount)
register struct iob *io;
register struct tftpdmsg *rmp;
int recvcount;
{
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];
	ASSERT(0 <= io->i_ctlr && io->i_ctlr < MAX_IFCS);
	/*
	 * Would like to use (sizeof (struct tftphdr)) here, but
	 * that doesn't quite work, so ...
	 */
#define TFTP_HDR_SIZE	((int)(rmp->th.th_data) - (int)(rmp))
	/*
	 * Take a look at the received packet
	 */
	if (recvcount < TFTP_HDR_SIZE) {
		dprintf(("tftprecv: short packet (len %d)\n",
			recvcount));
		return(0);
	}
	if (tsp->ts_blkno > 0 &&
	    ntohs(cei(io)->ei_srcaddr.sin_port) != tsp->ts_srvport) {
		dprintf(("tftprecv: pkt from server port %d, not %d\n",
			ntohs(cei(io)->ei_srcaddr.sin_port),
			tsp->ts_srvport));
		return(0);
	}
	if (cei(io)->ei_srcaddr.sin_addr.s_addr !=
	    cei(io)->ei_dstaddr.sin_addr.s_addr) {
		if ( tftp_broadcast ) {	
			if ( ntohs(rmp->th.th_opcode) == DATA ) {
				dprintf(("tftp_braodcast found %x for server\n",
					cei(io)->ei_srcaddr.sin_addr.s_addr));
				cei(io)->ei_dstaddr.sin_addr.s_addr =
				   cei(io)->ei_srcaddr.sin_addr.s_addr;
				tftp_broadcast = 0;
				tftp_noerror = 0;
				goto takeit;
			}
		}
		dprintf(("tftprecv: from wrong server (%s) wanted %x\n",
		    inet_ntoa(cei(io)->ei_srcaddr.sin_addr.s_addr),
		    cei(io)->ei_dstaddr.sin_addr.s_addr));
		dprintf(("message %x (%s)\n",ntohs(rmp->th.th_opcode),
		    ((ntohs(rmp->th.th_opcode) == ERROR) ? "ERROR" :
		    ((ntohs(rmp->th.th_opcode) == DATA) ? "DATA" :
		    "UNKNOWN"))));
		return(0);
	}
takeit:

	switch (ntohs(rmp->th.th_opcode)) {
	default:
		dprintf(("tftprecv: invalid TFTP opcode 0x%x\n",
			rmp->th.th_opcode));
		return(0);

	case ERROR:
		if ( tftp_broadcast ) {
			/* OK if errors while broadcasting */
			return(0);
		}
		if ( !tftp_noerror ) {
			printf("\nTFTP error: %s (code %d)\n",
		    		rmp->th.th_msg, ntohs(rmp->th.th_code));
		}
		io->i_errno = ECONNABORTED;
		return (-1);

	case DATA:
		tftp_noerror = 0;	/* report errors now */
		break;
	}
	
	if (tsp->ts_blkno < 0) {	/* receiving 1st block */
		tsp->ts_srvport = ntohs(cei(io)->ei_srcaddr.sin_port);
		cei(io)->ei_dstaddr.sin_port = htons(tsp->ts_srvport);
		tsp->ts_blkno = 0;
		tsp->ts_eof = 0;
	}

	/*
	 * Be careful with the block number.  The th_block field
	 * in the header is declared as a signed short, which would
	 * limit the size of files transferred with TFTP to 16 megabytes.
	 * Fortunately the server code treats the block number as
	 * an unsigned short, which allows the transfer of files
	 * up to 32 megabytes in length.
	 */
	if ((u_short)ntohs(rmp->th.th_block) != tsp->ts_blkno+1) {
		dprintf(("tftprecv: got blk %d, expect %d\n",
			(u_short)rmp->th.th_block, tsp->ts_blkno+1));
		/*
		 * Resend the last ack at this point in an attempt
		 * to get back in sync with the server.
		 */
		tftpack(io);
		return (0);
	}

	/*
	 * Increment block number
	 */
	tsp->ts_blkno++;

	/*
	 * Compute actual count of TFTP data bytes received
	 */
	recvcount -= TFTP_HDR_SIZE;
#ifdef STANDALONE
	/* 
	 * This check (hack) is to allow for Interphase eagle based servers
	 * which bump the transmit count by up to 3 for DMA allignment purposes.
	 */
	if (recvcount == (DEV_BSIZE+2)) recvcount -=2; /* its always 2 over */
	/*
	 * The following check should never be violated
	 */
	if (recvcount > DEV_BSIZE)
		_io_abort("tftp receive too large (protocol error)");
	bcopy(rmp->th.th_data, io->i_buf, recvcount);
	tsp->ts_data = io->i_buf;
#else
	tsp->ts_data = rmp->th.th_data;
#endif
	tsp->ts_offset = 0;
	tsp->ts_count = recvcount;

	dprintf(("tftprecv: got blk nbr %d, %d bytes\n",
		tsp->ts_blkno, recvcount));
	
	if (recvcount < SEGSIZE)
		/*
		 * This is the last block of the file.
		 */
		tsp->ts_eof = 1;

	/*
	 * Acknowledge this block now to get some overlap with the server.
	 */
	tftpack(io);

	/*
	 * Note that recvcount may be 0 in the case that we
	 * have just received the last block of a file that has
	 * length divisible by 512 bytes.
	 */
	return(recvcount);

#undef TFTP_HDR_SIZE
}

/*
 * TFTP send acknowledgement routine.
 *
 * Format and transmit an acknowledgement for the most recently
 * received block.
 */
void
tftpack(io)
register struct iob *io;
{
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];
	struct tftphdr msg;	/* TFTP packet */
	register struct tftphdr *thp = &msg;
	ASSERT(0 <= io->i_ctlr && io->i_ctlr < MAX_IFCS);
	/*
	 * It is an error if we are acknowledging a block
	 * number less than 1.
	 */
	if (tsp->ts_blkno < 1) {
		printf("tftpack: called for bogus block %d\n", tsp->ts_blkno);
		return;
	}

	/*
	 * Initialize the TFTP header for ACK.
	 */
	thp->th_opcode = htons(ACK);
	thp->th_block = htons(tsp->ts_blkno);
	dprintf(("TFTP send ACK for blk %d\n", tsp->ts_blkno));

	io->i_cc = sizeof *thp;
	io->i_ma = (char *)thp;
	DEVWRITE(io);
}

/*
 * TFTP error send routine.
 *
 * Used when we don't intend to read all the blocks of the file.
 * Sends a TFTP error packet to stop the server from retrying.
 */
void
tftpabort(io)
register struct iob *io;
{
	register struct tftp_state *tsp = &tftp_st[io->i_ctlr];
	struct tftphdr msg;	/* TFTP packet */
	register struct tftphdr *thp = &msg;
	ASSERT(0 <= io->i_ctlr && io->i_ctlr < MAX_IFCS);
	/*
	 * Initialize the TFTP header for ERROR packet.
	 */
	thp->th_opcode = htons(ERROR);
	thp->th_code = htons(ENOSPACE);
	dprintf(("tftpabort: TFTP send ERROR packet\n"));

	/*
	 * Send "ERROR" only once, since it will not be acknowledged.
	 */
	io->i_cc = sizeof *thp;
	io->i_ma = (char *)thp;
	DEVWRITE(io);
}

