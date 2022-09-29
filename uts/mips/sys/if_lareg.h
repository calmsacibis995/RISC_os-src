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
/* $Header: if_lareg.h,v 1.3.2.2 90/05/10 06:22:45 wje Exp $ */


/*
 * Software fixes for the errata mentioned in the Errata Sheet for AM7990
 * Lance are ifdef'd based on their categorization in this document.
 */
#define ERRATA_8461	1
#define P1SA		1
#define P1SB		1
#define P1SC		1
#define P1SF		1
#define P1SG		1
#define P3		1
#define P4		1


/* 7990 'CSR' values */

/* bits in csr0 */
#define CSR0_ERR	0x8000		/* error summary */
#define CSR0_BABL	0x4000		/* babble error */
#define CSR0_CERR	0x2000		/* collision error */
#define CSR0_MISS	0x1000		/* missed packet */
#define CSR0_MERR	0x0800		/* memory error */
#define CSR0_RINT	0x0400		/* receiver interrupt */
#define CSR0_TINT	0x0200		/* transmitter interrupt */
#define CSR0_IDON	0x0100		/* initialization done */
#define CSR0_INTR	0x0080		/* interrupt flag */
#define CSR0_INEA	0x0040		/* interrupt enable */
#define CSR0_RXON	0x0020		/* receiver on */
#define CSR0_TXON	0x0010		/* transmitter on */
#define CSR0_TDMD	0x0008		/* transmit demand */
#define CSR0_STOP	0x0004		/* stop the lance */
#define CSR0_STRT	0x0002		/* start the lance */
#define CSR0_INIT	0x0001		/* initialize the lance */


/*
 * Initialization Block.
 *	Chip initialization includes the reading of the init block to obtain
 *	the operating parameters.
 */

struct drp {
#ifdef Murphys_Revenge
	u_short		dra:8;		/* high byte of ring desc address */
	u_short		len:3;		/* ring length, power of 2 */
	u_short		res:5;		/* reserved */
#else
	u_short		len:3;		/* ring length, power of 2 */
	u_short		res:5;		/* reserved */
	u_short		dra:8;		/* high byte of ring desc address */
#endif
};

struct initblk {
	u_short		ib_mode;	/* mode register */
#ifdef Murphys_Revenge
#define IBM_DRX		0x0100		/* disable receiver */
#define IBM_DTX		0x0200		/* disable transmitter */
#define IBM_LOOP	0x0400		/* loopback */
#define IBM_DTCR	0x0800		/* disable transmit crc */
#define IBM_COLL	0x1000		/* force collision */
#define IBM_DRTY	0x2000		/* disable retry */
#define IBM_INTL	0x4000		/* internal loopback */
#define IBM_PROM	0x0080		/* promiscuous mode */
#else
#define IBM_DRX		0x0001		/* disable receiver */
#define IBM_DTX		0x0002		/* disable transmitter */
#define IBM_LOOP	0x0004		/* loopback */
#define IBM_DTCR	0x0008		/* disable transmit crc */
#define IBM_COLL	0x0010		/* force collision */
#define IBM_DRTY	0x0020		/* disable retry */
#define IBM_INTL	0x0040		/* internal loopback */
#define IBM_PROM	0x8000		/* promiscuous mode */
#endif
	u_char		ib_padr[6];	/* physical address */
	u_short		ib_ladrf[4];	/* logical address filter */
	u_short 	ib_rdra;	/* rcv ring desc addr(low) */
	struct drp	ib_rdrp;	/* rcv ring len and desc addr(high) */
	u_short 	ib_xdra;	/* xmit ring desc addr(low) */
	struct drp	ib_xdrp;	/* xmit ring len and desc addr(high) */
};

/* 
 * Buffer Management is accomplished through message descriptors organized
 * in ring structures in main memory. There are two rings allocated for the
 * device: a receive ring and a transmit ring. The following defines the 
 * structure of the descriptor rings.
 */

/* receive message descriptor */
struct rmd {
	u_short		ladr;		/* low order word of the buf addr */
#ifdef Murphys_Revenge
	u_char		hadr;		/* high order byte of the buf addr */
	u_char		flags;		/* misc error and status bits */
#else
	u_char		flags;		/* misc error and status bits */
	u_char		hadr;		/* high order byte of the buf addr */
#endif
					/* flags bits */
#define RFLG_ENP	0x01		/* end of packet */
#define RFLG_STP	0x02		/* start of packet */
#define RFLG_BUFF	0x04		/* buffer error */
#define RFLG_CRC	0x08		/* crc error */
#define RFLG_OFLO	0x10		/* overflow error */
#define RFLG_FRAM	0x20		/* framing error */
#define RFLG_ERR	0x40		/* error summary */
#define RFLG_OWN	0x80		/* ownership bit, 1==LANCE */
	u_short		bcnt;		/* buffer byte count */
#define RBCNT_BCNT	0x0fff		/* buf byte count, in 2's compl */
#define RBCNT_ONES	0xf000		/* must be ones */
	u_short		mcnt;		/* message byte count */
#define RMCNT_BCNT	0x0fff		/* message byte count */
#define RMCNT_RES	0xf000		/* reserved, read as zeros */
};


/* transmit message descriptor entry */
struct xmd {
	u_short		ladr;		/* low order word of the buf addr */
#ifdef Murphys_Revenge
	u_char		hadr;		/* high order byte of the buf addr */
	u_char		status;		/* misc status bits */
#else
	u_char		status;		/* misc status bits */
	u_char		hadr;		/* high order byte of the buf addr */
#endif
#define XST_ENP		0x01		/* end of packet */
#define XST_STP		0x02		/* start of packet */
#define XST_DEF		0x04		/* defer while trying to transmit */
#define XST_ONE		0x08		/* one retry was needed */
#define XST_MORE	0x10		/* more than one retry was needed */
#define XST_RES		0x20		/* reserved bit */
#define XST_ERR		0x40		/* error summary */
#define XST_OWN		0x80		/* ownership bit, 1==LANCE */
#define XST_XMIT	(XST_STP | XST_ENP | XST_OWN)
	u_short		bcnt;		/* buffer byte count */
	u_short		error;		/* misc error bits */
#ifdef Murphys_Revenge
#define XERR_TDR	0xff03		/* time domain reflectometry */
#define XERR_RTRY	0x0004		/* retry error */
#define XERR_LCAR	0x0008		/* loss of carrier */
#define XERR_LCOL	0x0010		/* late collision */
#define XERR_RES	0x0020		/* reserved bit */
#define XERR_UFLO	0x0040		/* underflow error */
#define XERR_BUFF	0x0080		/* buffer error */
#else
#define XERR_TDR	0x03ff/* time domain reflectometry */
#define XERR_RTRY	0x0400		/* retry error */
#define XERR_LCAR	0x0800		/* loss of carrier */
#define XERR_LCOL	0x1000		/* late collision */
#define XERR_RES	0x2000		/* reserved bit */
#define XERR_UFLO	0x4000		/* underflow error */
#define XERR_BUFF	0x8000		/* buffer error */
#endif
};

/* error stat accumulation */

#define	LASTAT_IERRORS(x)	(x)[ERR_CRC] + (x)[ERR_FRAM]
#define	LASTAT_OERRORS(x)	(x)[ERR_RTRY] + (x)[ERR_LCAR]
#define	LASTAT_CERRORS(x)	(x)[ERR_MORE] + (x)[ERR_ONE]

/* buffer count constants */

#ifdef	LANCETEST
#define XBCNT_MIN	32		/* minimum length of buf */
#define XBCNT_MINFC	32		/* minimum length of first chain buf */
#else
#define XBCNT_MIN	64		/* minimum length of buf */
#define XBCNT_MINFC	100		/* minimum length of first chain buf */
#endif

#define XBCNT_BCNT	0x0fff		/* buf byte count, in 2's compl */
#define XBCNT_ONES	0xf000		/* must be ones */

#define LOG_NUM_RECV	5			/* # receives, log base 2 */
#if	defined(STANDALONE) && !defined(SABLE)
#define LOG_NUM_XMIT	3			/* # xmits, log base 2 */
#else
#define LOG_NUM_XMIT	4			/* # xmits, log base 2 */
#endif

#define NUM_RECV	(1 << LOG_NUM_RECV)	/* # receive ring entries */
#define NUM_XMIT	(1 << LOG_NUM_XMIT)	/* # xmit ring entries */

#define MAX_XPKT 1514			/* transmit len--7990 adds cksum */
#define MIN_XPKT 60
#define MIN_RPKT (MIN_XPKT+CRC_LEN)

#define	ALIGNFACTOR	0x08
#define	ALIGNPAD	0x04

#define	ROUNDUP(x, y)	(((x) % (y)) ? (((x)/(y))*(y) + (y)) : (x))

#define	SIZE_RECVBUF	MAX_XPKT + CRC_LEN
#define	SIZE_XMITBUF	MAX_XPKT

/* macros used to stuff the ring buffers and initialization block */

#ifdef Murphys_Revenge
#define _BS_(bs) (((bs & 0xff00) >> 8) | ((bs & 0xff) << 8))
#else
#define _BS_(bs) (bs)
#endif

#define	HADR(x)	((x >> 16) & 0xff)

#define	LADR(x)	((_BS_(x)) & 0xffff)
#define	BCNT(x)	(_BS_(-(x)))
#define	MCNT(x)	(_BS_(x))

#define	BCNT_XTRCT(x)	(-(_BS_(x)))

#define	RCVADR(x) ((((x)->hadr) << 16) | _BS_((x)->ladr))
#define	XMTADR(x) (RCVADR(x))

/*
 * Defines pertaining to statistics gathering (diagnostic only)
 */

/* receive errors */
#define ERR_FRAM	0		/* framing error */
#define ERR_OFLO	1		/* overflow error */
#define ERR_CRC		2		/* crc error */
#define ERR_RBUFF	3		/* receive buffer error */

/* transmit errors */
#define ERR_MORE	4		/* more than one retry */
#define ERR_ONE		5		/* one retry */
#define ERR_DEF		6		/* defer'd packet */
#define ERR_TBUFF	7		/* transmit buffer error */
#define ERR_UFLO	8		/* underflow error */
#define ERR_LCOL	9		/* late collision */
#define ERR_LCAR	10		/* loss of carrier */
#define ERR_RTRY	11		/* retry error, >16 retries */

/* errors reported in csr0 */
#define ERR_BABL	12		/* transmitter timeout error */
#define ERR_MISS	13		/* missed packet */
#define ERR_MEM		14		/* memory error */
#define ERR_CERR	15		/* collision errors */
#define XMIT_INT	16		/* transmit interrupts */
#define RCV_INT		17		/* receive interrupts */

#define	CSR_ERRSTART	12
#define	CSR_ERRTOT	4

#define NHARD_ERRORS	18		/* error types used in diagnostic */

/* other statistics */
#define ERR_TTOUT	18		/* transmit timeouts */
#define ERR_ITOUT	19		/* init timeouts */
#define ERR_INITS	20		/* reinitializations */
#define ERR_RSILO	21		/* silo ptrs misaligned on recv */
#define ERR_TSILO	22		/* silo ptrs misaligned on xmit */
#define ERR_SINTR	23		/* spurious interrupts */

#define NUM_ERRORS	24		/* number of errors types */

#define	INITS		24		/* number of initializations */
#define	XMT_PKTS	25		/* number of packets xmitted */
#define	RCV_PKTS	26		/* number of packets received */

#define NUM_STATS	27

#define HARD_ERROR_STRINGS \
	"Framing error", "Overflow error", "CRC error", \
	"Rcv buffer error", "More than one retry", "One retry", \
	"Deferred Packet", "Transmit buffer error", \
	"Underflow error", "Late collision", "Loss of carrier", \
	"Retry error", "Babble error", "Missed packet", \
	"Memory error", "Collision error", "Transmit interrupts", \
	"Receive Interrupts"

#define	CRC_LEN	4

/* iocb specific items */

#define LAIOCBMAX 16

#define LANOWAIT	1
#define LASPIN		2
#define LASLEEP		3
#define LASCAN		4

#define	DONT_INTERRUPT	0
#define	DO_INTERRUPT	1

/* 
 * command packets consist of the command and a null delimited argument list 
 *
 * status returns consist of the returned status (> 0: success, -1: failure) and
 * a null delimited status list 
 */

#define LA_ERROR	-1

#define LA_PROBE	1
/*
 * arg list: interrupt flag
 * status: LA_PROBE, 6 byte physical ethernet address in cmd block
 */

#define LA_INIT		2
/* 
 * arg list: ptr to initialization block
 * status: LA_INIT
 */
#define LA_STOP		3
/* 
 * arg list: no args
 * status: LA_STOP
 */
#define LA_STRT		4
/* 
 * arg list: no args
 * status: LA_STRT
 */
#define LA_RECV		5
/* 
 * status: LA_RECV
 */
#define LA_XMIT		6
/* 
 * status: LA_XMIT
 */
#define LA_XMIT_DONE	7
/* 
 * status: LA_XMIT_DONE
 */
#define LA_STAT		8
/* 
 * arg list: no args
 * status: LA_STAT, CSR_ERRTOT errors in cmd blk
 */
#define LA_INIT_DONE	9
/* 
 * status: LA_INIT_DONE
 */
#define LA_RESET	10
/* 
 * status: LA_RESET
 */

#define LA_DBG_ON	11
/* 
 * status: LA_DBG_ON
 */

#define LA_DBG_OFF	12
/* 
 * status: LA_DBG_OFF
 */

#define LA_MISS		13
/*
 * status: LA_MISS
 */

#define LA_LAST		LA_MISS		/* keep up to date */
#define	NUM_CMDS	LA_LAST+1

#define INT_LB		0x8	/*  Internal loopback mode           */
#define INT_CL		0x9	/*  Internal loopback collision mode */
#define INT_CE		0xA	/*  Internal loopback CRC error mode */
#define EXT_LB		0xC	/*  External loopback mode           */

	/*  UNIT  */
#define	UNIT_AF_INET		0x0	/*  UNIT for INET domain      */
#define	UNIT_AF_LANCETEST	0x1	/*  UNIT for LANCETEST demain */

/* __EOF__ */
