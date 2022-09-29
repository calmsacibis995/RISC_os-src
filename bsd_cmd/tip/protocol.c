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
#ident	"$Header: protocol.c,v 1.2.2.2 90/05/07 19:40:52 wje Exp $"

/*
 * protocol.c -- serial line protocol
 */

#include "tip.h"
#include "protoio.h"
#include "protocol.h"

#ifdef DEBUG
char *str_buf();
#endif DEBUG

/*
 * Serial line protocol description:
 *	This protocol is a simple transmit/ack/nack, byte-stuffing,
 *	half-duplex protocol.  (Half-duplex is all that's reasonable to
 *	implement for the prom and standalone code.) The protocol may be
 *	used for bidirectional communication, but the the hand-shaking
 *	necessary to turn the line around is the responsibility of the
 *	user of the protocol.
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
init_proto(dev)
pdev_t dev;
{
	PINIT(dev);
	next_getseq = current_putseq = 0;
	acked_putseq = -1;
	n_acked = n_xmited = 0;
	/*
	 * initial packet size is 1/2 of MAXPACKET/2 (256 bytes roughly)
	 * (prom is tuned for max of 1023 byte packets at 4MHz clock,
	 * MAXPACKET/2 to handle worst case byte stuffing)
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
	int opacket_size = packet_size;

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
#ifdef notdef
			if (packet_size != opacket_size)
				vprintf("\r\nSetting packet size to %d\r\n",
				    packet_size);
#endif notdef
		} else if (ack_ratio > MAX_ACK_THRESH) {
			/*
			 * Max packet size is limited to MAXPACKET/2
			 * to allow for worst case DLE expansion
			 * with prom routines tuned to handle 1024 byte
			 * packets with 4MHz clock.
			 */
			packet_size =
			    min(MAXPACKET/2, (packet_size * 8) / 7);
			n_acked = n_xmited = 0;
#ifdef notdef
			if (packet_size != opacket_size)
				vprintf("\r\nSetting packet size to %d\r\n",
				    packet_size);
#endif notdef
		}
	}
	return(packet_size);
}
#endif

static jmp_buf resync_buf;

/*
 * getpkt -- unwrap incoming packet, check sequence number and checksum
 * and send appropriate acknowledgment.
 * Returns data length.
 */
getpkt(dev, buf, cnt, pkt_typep)	/* returns data length */
pdev_t dev;		/* device to receive from */
char *buf;		/* buf for received packet */
unsigned cnt;		/* max data len */
char *pkt_typep;	/* in: pointer to desired packet type, */
			/* out: pointer to received packet type */
{
	unsigned csum, pkt_csum, seq;
	char pkt_type;
	char *cp;
	int new_packet, type_len, len, i;
	unsigned nseq;

	new_packet = 0;
	while (!new_packet)
	{
		while (GETC(dev) != SYN)	/* sync to start of packet */
			continue;
		
		setjmp(resync_buf);		/* longjmp here on SYN */

		get_csum = 0;

		type_len = csum_getc(dev);

		pkt_type = type_len & MASK_PKTTYPE;
		if (pkt_type != *pkt_typep && *pkt_typep != ANY_PKTTYPE) {
			/*
			 * Should "verbose" variable to control printing
			 * these.
			 */
			vprintf("\r\nbad type\r\n");
			goto sendack;
		}

		len = (type_len & 0x1f) << 6;
		len |= csum_getc(dev) & 0x3f;
		if (len > cnt) {	/* don't accept long packets */
			vprintf("\r\nbad len\r\n");
			goto sendack;
		}

		seq = (pkt_type == DATA_PKTTYPE)
		    ? next_getseq		    /* next expected data seq */
		    : (current_putseq + 1) & 0x3f;  /* next expected ack seq */
		if ((nseq = (csum_getc(dev) & 0x3f)) != seq) {
			vprintf("\r\nbad seq, got 0x%x wanted 0x%x\r\n",
			    nseq, seq);
			goto sendack;
		}

		cp = buf;
		i = len;
		while (i-- > 0)
			*cp++ = csum_getc(dev);

		pkt_csum = get_csum;
		csum = (csum_getc(dev) & 0x3f) << 12;
		csum |= (csum_getc(dev) & 0x3f) << 6;
		csum |= csum_getc(dev) & 0x3f;

		if ((pkt_csum & 0x3ffff) != csum) {
			vprintf("\r\nbad csum\r\n");
			goto sendack;
		}
		
		new_packet = 1;		/* got a good packet */
		if (pkt_type == DATA_PKTTYPE)
			next_getseq = (next_getseq + 1) & 0x3f;
		else
			acked_putseq = current_putseq;

sendack:
#ifdef DEBUG
		printf("getpkt len=%d buf=%s seq=0x%x type %s\r\n",
		    len, str_buf(buf, len), nseq,
		    pkt_type == ACK_PKTTYPE ? "ACK" : "DATA");
#endif

		/*
		 * Don't send ACKs to ACKs
		 */
		if (pkt_type != ACK_PKTTYPE)
			putpkt(dev, NULL, 0, ACK_PKTTYPE);
	}

	*pkt_typep = pkt_type;
	return(len);
}

/*
 * csum_getc -- get next character, handling checksum calculation
 * and DLE escapes
 */
static
csum_getc(dev)
pdev_t dev;
{
	unsigned c;

	c = GETC(dev) & 0xff;
	get_csum += c;

	if (c == SYN) {
		vprintf("\r\ngot unexpected sync\r\n");
		longjmp(resync_buf, 1);
	}

	if (c == DLE) {
		c = csum_getc(dev);

		switch (c) {

		case 'S':
			c = SYN;
			break;

		case 'D':
			c = DLE;
			break;

		case 'C':
			c = CTRL('C');
			break;

		case 's':
			c = CTRL('S');
			break;

		case 'q':
			c = CTRL('Q');
			break;

		default:
			printf("\r\nunknown DLE escape, 0x%x\r\n", c);
			break;
		}
	}
	return (c);
}

/*
 * putpkt -- wrap data in packet and transmit.
 * Waits for acknowledgment and retransmits as necessary
 */
putpkt(dev, buf, cnt, pkt_type)
pdev_t dev;
char *buf;
int cnt;
char pkt_type;
{
	jmp_buf rexmit_buf;
	register char *cp;
	register i;
	char ack_type;
	unsigned seq;
	char type_len;
	int rexmit_cnt = 0;

	if (pkt_type != DATA_PKTTYPE && pkt_type != ACK_PKTTYPE) {
		printf("\r\n*** putpkt: ILLEGAL PACKET TYPE\r\n");
		return(-1);
	}

	if (cnt > MAXPACKET/2) {
		printf("\r\n*** ILLEGAL PACKET SIZE\r\n");
		return(-1);
	}

	if (setjmp(rexmit_buf)) {
		/* restart here if timeout */
		if (++rexmit_cnt > 2)
			return(-1);
		vprintf("\r\nRetransmitting packet, packet size = %d\r\n",
		    packet_size);
	}

	while (current_putseq != acked_putseq) {

		PUTC(SYN, dev);

		put_csum = 0;

		type_len = pkt_type | ((cnt >> 6) & 0x1f) | 0x40;
		csum_putc(type_len, dev);			/* TYPE_LEN */

		csum_putc((cnt & 0x3f) | 0x40, dev);		/* LEN1 */

		seq = (pkt_type == DATA_PKTTYPE)
		    ? current_putseq	/* sequence for data packets */
		    : next_getseq;	/* sequence for ack packets */
		csum_putc(seq | 0x40, dev);			/* SEQ */

		cp = buf;
		for (i = cnt; i > 0; i--)			/* DATA */
			csum_putc(*cp++, dev);

		PUTC(((put_csum >> 12) & 0x3f) | 0x40, dev);	/* CSUM */
		PUTC(((put_csum >> 6) & 0x3f) | 0x40, dev);
		PUTC((put_csum & 0x3f) | 0x40, dev);

		PUTFLUSH(dev);

#ifdef DEBUG
		printf("putpkt len=%d buf=%s seq=0x%x type %s\r\n",
		    cnt, str_buf(buf, cnt), seq,
		    pkt_type == ACK_PKTTYPE ? "ACK" : "DATA");
#endif

		if (pkt_type == ACK_PKTTYPE)	/* don't send ACKs to ACKs */
			return(cnt);

		n_xmited++;

		timer_jmpbuf = rexmit_buf;
		set_timer(REXMIT_TIME);

		ack_type = ACK_PKTTYPE;
		getpkt(dev, NULL, 0, &ack_type);

		set_timer(0);
		timer_jmpbuf = NULL;
	}

	if (pkt_type == DATA_PKTTYPE) {
		current_putseq = (current_putseq + 1) & 0x3f;
		n_acked++;
	}
	return(cnt);
}

/*
 * csum_putc -- put character handling checksum calculation and doing
 * DLE stuffing for characters that must be escaped
 */
static
csum_putc(c, dev)
unsigned c;
pdev_t dev;
{
	switch (c) {

	case SYN:
		put_csum += DLE;
		PUTC(DLE, dev);
		c = 'S';
		break;

	case DLE:
		put_csum += DLE;
		PUTC(DLE, dev);
		c = 'D';
		break;

	case CTRL('C'):
		put_csum += DLE;
		PUTC(DLE, dev);
		c = 'C';
		break;

	case CTRL('S'):
		put_csum += DLE;
		PUTC(DLE, dev);
		c = 's';
		break;

	case CTRL('Q'):
		put_csum += DLE;
		PUTC(DLE, dev);
		c = 'q';
		break;
	}

	put_csum += (c & 0xff);
	PUTC(c, dev);
}

vprintf(format, arg1, arg2, arg3)
{
	if (boolean(value(VERBOSE)))
		printf(format, arg1, arg2, arg3);
}

#ifdef DEBUG
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
#endif DEBUG
