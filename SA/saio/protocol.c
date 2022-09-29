#ident "$Header: protocol.c,v 1.2 90/01/16 17:35:50 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * protocol.c -- serial line protocol
 */

#include "saio/protocol.h"
#include "saio/protoio.h"
#include "saio/setjmp.h"
#include "saio/saioctl.h"
#include "saio/debug.h"

#define	NULL	0
#define	CTRL(x)	('x'&0x1f)

char *str_buf();

/*
 * Serial line protocol description:
 *	This protocol is a ack/nack, byte-stuffing, half-duplex protocol.
 *	The protocol may be used for bidirectional communication, but the
 *	hand-shaking necessary to turn the line around is the responsibility
 *	of the user of the protocol.
 *
 *	SYN characters are used purely to synchonize the protocol and
 *	always resync the protocol when received.  SYN as part of the
 *	data must be represented as DLE S.  DLE as part of the data
 *	is represented as DLE D.  CTRL(C) is not a protocol special
 *	character, but it is the prom monitor "interrupt" character,
 *	so it is escaped as DLE C.  CTRL(S) and CTRL(Q) are also
 *	escaped (DLE s and DLE q) to avoid conflict with common tty
 *	flow control conventions.
 *
 *	The protocol supports 8 bit data, but does not use 8 bit characters
 *	in the packet header so the protocol is usable with 7 bit data paths
 *	if the client is prepared to deal with 7 bit data.
 *
 * serial line protocol format:
 * <SYN><type_len0><len1><seq><data0>...<data_len><csum0><csum1><csum2>
 * 
 * <SYN>       ::=	Ascii sync character
 *
 * <type_len0> ::=	Packet type and high order bits of data length
 *			Length is data length BEFORE DLE escapes are
 *			inserted.
 *			Bit 6 is set to 1 to avoid inadvertent SYN character
 *			Bit 5 == 1 => Data packet (DATA_PKTTYPE)
 *			Bit 5 == 0 => Acknowledgment packet (ACK_PKTTYPE)
 *			Bits 4 -- 0 => Bits 10 -- 6 of data length
 *
 * <len1>      ::=	Low order data length bits.
 *			Bit 6 is set to 1 to avoid inadvertent SYN character
 *			Bits 5 -- 0 => Bits 5 -- 0 of data length
 *
 * <seq>       ::=	6 bit sequence number, encoded as
 *			low 6 bits of 1 character.
 *			Character is OR'ed with 0x40 to
 *			avoid conflict with SYN character.
 *			ACK_PKTTYPE packets carry sequence number
 *			of NEXT expected DATA_PKTTYPE packet.
 *
 * <dataN>     ::=	Body of message
 *
 * <csum0..2>  ::=	18 bit checksum of all bytes of packet 
 *			excluding SYN character and checksum
 *			itself, encoded as 3 characters with
 *			most significant 6 bits in first character
 *			middle significant 6 bits in second
 *			character and least significant 6 bits
 *			in third character.
 *			Each character is OR'ed with 0x40 to
 *			avoid conflict with SYN character.
 *
 */

static unsigned next_getseq;		/* next expected receive seq number */
static unsigned current_putseq;		/* seq number for current xmit packet */
static unsigned acked_putseq;		/* last acknowledged xmit packet seq */
static unsigned get_csum;		/* receive csum accumulator */
static unsigned put_csum;		/* xmit csum accumulator */
static n_acked;				/* number of ACK pkts received */
static n_xmited;			/* number of transmitted pkts */
static packet_size;			/* current packet size */

/*
 * init_proto -- reset protocol state info
 */
init_proto(fd)
pdev_t fd;
{
	PINIT(fd);
	next_getseq = current_putseq = 0;
	acked_putseq = -1;
	n_acked = n_xmited = 0;
	/*
	 * initial packet size is 1/2 of MAXPACKET/2 (256 bytes roughly)
	 * (MAXPACKET/2 to handle worst case byte stuffing)
	 */
	packet_size = (MAXPACKET / 2) / 2;
}

#ifdef PROTO_PKTSIZE
/*
 * proto_pktsize -- line quality adaptive transmission routine
 */
proto_pktsize()
{
	int len;
	int ack_ratio;

	if (n_xmited > MIN_XMIT_PKTS) {
		ack_ratio = n_acked * 100 / n_xmited;
		if ( ack_ratio < MIN_ACK_THRESH) {
			/*
			 * If we're having trouble, try halving
			 * packet size.
			 */
			packet_size =
			    max(MINPACKET, packet_size / 2);
			n_acked = n_xmited = 0;
		} else if (ack_ratio > MAX_ACK_THRESH) {
			/*
			 * Max packet size is limited to MAXPACKET/2
			 * to allow for worst case DLE expansion
			 */
			packet_size =
			    min(MAXPACKET/2, (packet_size * 8) / 7);
			n_acked = n_xmited = 0;
		}
	}
	return(packet_size);
}
#endif PROTO_PKTSIZE

static jmp_buf resync_buf;

/*
 * getpkt -- unwrap incoming packet, check sequence number and checksum
 * and send appropriate acknowledgment.
 * Returns data length.
 */
getpkt(fd, buf, cnt, pkt_typep)	/* returns data length */
pdev_t fd;		/* device to receive from */
char *buf;		/* buf for received packet */
unsigned cnt;		/* max data len */
int *pkt_typep;		/* in: pointer to desired packet type, */
			/* out: pointer to received packet type */
{
	register char *cp;
	register int i;
	unsigned csum, pkt_csum, seq;
	int pkt_type;
	int new_packet, type_len, len;
 	unsigned nseq;
	extern int Debug;

	if (*pkt_typep != DATA_PKTTYPE && *pkt_typep != ACK_PKTTYPE &&
	    *pkt_typep != ANY_PKTTYPE) {
		printf("*** getpkt: BAD PKTTYPE\n");
		return(0);
	}
	new_packet = 0;
	while (!new_packet)
	{
		while ((i = GETC(fd)) != SYN) {	/* sync to start of packet */
			if (Debug & DBG_PROTOOOB)
				putchar(i);
			continue;
		}
		
		setjmp(resync_buf);		/* longjmp here on SYN */

		get_csum = 0;

		type_len = csum_getc(fd);

		pkt_type = type_len & MASK_PKTTYPE;
		if (pkt_type != *pkt_typep && *pkt_typep != ANY_PKTTYPE) {
			vprintf("bad type, got %s\n",
			    pkt_type == DATA_PKTTYPE ? "data" : "ack");
			goto sendack;
		}

		len = (type_len & 0x1f) << 6;
		len |= csum_getc(fd) & 0x3f;
		if (len > cnt) {	/* don't accept long packets */
			vprintf("bad len\n");
			goto sendack;
		}

		seq = (pkt_type == DATA_PKTTYPE)
		    ? next_getseq		    /* next expected data seq */
		    : (current_putseq + 1) & 0x3f;  /* next expected ack seq */
		if ((nseq = (csum_getc(fd) & 0x3f)) != seq) {
			vprintf("bad seq, got 0x%x wanted 0x%x\n", nseq, seq);
			goto sendack;
		}

		cp = buf;
		i = len;
		while (i-- > 0)
			*cp++ = csum_getc(fd);

		pkt_csum = get_csum;
		csum = (csum_getc(fd) & 0x3f) << 12;
		csum |= (csum_getc(fd) & 0x3f) << 6;
		csum |= csum_getc(fd) & 0x3f;

		if ((pkt_csum & 0x3ffff) != csum) {
			vprintf("bad csum\n");
			goto sendack;
		}
		
		new_packet = 1;		/* got a good packet */
		if (pkt_type == DATA_PKTTYPE)
			next_getseq = (next_getseq + 1) & 0x3f;
		else
			acked_putseq = current_putseq;

sendack:
		if (Debug & DBG_PROTOCOL)
			printf("getpkt len=%d buf=%s seq=0x%x type %s\r\n",
			    len, str_buf(buf, len), nseq,
			    pkt_type == ACK_PKTTYPE ? "ACK" : "DATA");

		/*
		 * Don't send ACKs to ACKs
		 */
		if (pkt_type != ACK_PKTTYPE)
			putpkt(fd, NULL, 0, ACK_PKTTYPE);
	}

	*pkt_typep = pkt_type;
	return(len);
}

/*
 * csum_getc -- get next character, handling checksum calculation
 * and DLE escapes
 */
static
csum_getc(fd)
pdev_t fd;
{
	unsigned c;

	c = GETC(fd) & 0xff;
	get_csum += c;

	if (c == SYN) {
		vprintf("got unexpected sync\n");
		longjmp(resync_buf, 1);
	}

	if (c == DLE) {
		c = csum_getc(fd);

		switch (c) {

		case 'S':
			c = SYN;
			break;

		case 'D':
			c = DLE;
			break;

		case 'C':
			c = CTRL(C);
			break;

		case 's':
			c = CTRL(S);
			break;

		case 'q':
			c = CTRL(Q);
			break;

		default:
			printf("unknown DLE escape, 0x%x\n", c);
			break;
		}
	}
	return (c);
}

/*
 * putpkt -- wrap data in packet and transmit.
 * Waits for acknowledgment and retransmits as necessary
 */
putpkt(fd, buf, cnt, pkt_type)
pdev_t fd;
char *buf;
int cnt;
int pkt_type;
{
	jmp_buf rexmit_buf;
	register char *cp;
	register i;
	int ack_type;
	unsigned seq;
	char type_len;
	extern int *_timer_jmpbuf;
	extern int Debug;

	if (pkt_type != DATA_PKTTYPE && pkt_type != ACK_PKTTYPE) {
		printf("*** putpkt: ILLEGAL PACKET TYPE\n");
		return;
	}

	if (cnt > MAXPACKET/2) {
		printf("*** putpkt: ILLEGAL PACKET SIZE\n");
		return;
	}

	/* restart here if timeout */
	if (setjmp(rexmit_buf))
		vprintf("retransmitting packet 0x%x\n", current_putseq);

	while (current_putseq != acked_putseq) {

		PUTC(SYN, fd);

		put_csum = 0;

		type_len = pkt_type | ((cnt >> 6) & 0x1f) | 0x40;
		csum_putc(type_len, fd);			/* TYPE_LEN */

		csum_putc((cnt & 0x3f) | 0x40, fd);		/* LEN1 */

		seq = (pkt_type == DATA_PKTTYPE)
		    ? current_putseq	/* sequence for data packets */
		    : next_getseq;	/* sequence for ack packets */
		csum_putc(seq | 0x40, fd);			/* SEQ */

		cp = buf;
		for (i = cnt; i > 0; i--)			/* DATA */
			csum_putc(*cp++, fd);

		PUTC(((put_csum >> 12) & 0x3f) | 0x40, fd);	/* CSUM */
		PUTC(((put_csum >> 6) & 0x3f) | 0x40, fd);
		PUTC((put_csum & 0x3f) | 0x40, fd);

		PUTFLUSH(fd);

		if (Debug & DBG_PROTOCOL)
			printf("putpkt len=%d buf=%s seq=0x%x type %s\r\n",
			    cnt, str_buf(buf, cnt), seq,
			    pkt_type == ACK_PKTTYPE ? "ACK" : "DATA");

		if (pkt_type == ACK_PKTTYPE)	/* don't send ACKs to ACKs */
			return;

		n_xmited++;

		_timer_jmpbuf = rexmit_buf;
		_set_timer(REXMIT_TIME);

		ack_type = ACK_PKTTYPE;
		getpkt(fd, NULL, 0, &ack_type);

		_set_timer(0);
		_timer_jmpbuf = NULL;
	}

	if (pkt_type == DATA_PKTTYPE) {
		current_putseq = (current_putseq + 1) & 0x3f;
		n_acked++;
	}
}

/*
 * csum_putc -- put character handling checksum calculation and doing
 * DLE stuffing for characters that must be escaped
 */
static
csum_putc(c, fd)
unsigned c;
pdev_t fd;
{
	switch (c) {

	case SYN:
		put_csum += DLE;
		PUTC(DLE, fd);
		c = 'S';
		break;

	case DLE:
		put_csum += DLE;
		PUTC(DLE, fd);
		c = 'D';
		break;

	case CTRL(C):
		put_csum += DLE;
		PUTC(DLE, fd);
		c = 'C';
		break;

	case CTRL(S):
		put_csum += DLE;
		PUTC(DLE, fd);
		c = 's';
		break;

	case CTRL(Q):
		put_csum += DLE;
		PUTC(DLE, fd);
		c = 'q';
		break;
	}

	put_csum += (c & 0xff);
	PUTC(c, fd);
}

/*
 * vprintf -- print error messages if $verbose set
 */
vprintf(fmt, arg1, arg2)
{
	extern Verbose;

	if (Verbose)
		printf(fmt, arg1, arg2);
}

char *
str_buf(buf, len)
char *buf;
int len;
{
	static char tmpbuf[64];
	int i;

	if (len == 0)
		strcpy(tmpbuf, "\"\"");
	else {
		tmpbuf[0] = '"';
		for (i = 0; i < len && i < 20; i++)
			tmpbuf[i+1] = buf[i];
		tmpbuf[++i] = '"';
		tmpbuf[++i] = 0;
	}
	return(tmpbuf);
}

#ifdef STANDALONE
/*
 * proto_enable -- enable a device for protocol use
 */
proto_enable(fd)
pdev_t fd;
{
	if (!isatty(fd))
		_io_abort("not a character device");
	ioctl(fd, TIOCRAW, 1);	/* disable special chars */
	ioctl(fd, TIOCFLUSH, 0);	/* flush input */
	/* protocol mode temporarily disable use as pseudo-console */
	ioctl(fd, TIOCPROTO, 1);	/* turn-on protocol mode */
}

/*
 * proto_disable -- disable a getc device for protocol use
 */
proto_disable(fd)
pdev_t fd;
{
	ioctl(fd, TIOCRAW, 0);	/* enable special characters */
	ioctl(fd, TIOCFLUSH, 0);	/* flush input */
	ioctl(fd, TIOCPROTO, 0);	/* turn-off protocol mode */
}
#endif
