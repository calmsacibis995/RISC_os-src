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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: lastruct.h,v 1.4.1.3.1.3.1.2 90/10/16 12:19:32 beacker Exp $ */
/*
 * AMD 7990 "Lance" Ethernet Controller Structures
 */

/*
 * Ethernet software status per interface
 *
 * Each interface is referenced by a network interface structure ls_if, which 
 * the routing code uses to locate the interface.  This structure contains 
 * the output queue for the interface, its address, ...
 */
struct	la_softc
{
	struct arpcom	ls_ac;		/* Common Ethernet structures */
#define	ls_if		ls_ac.ac_if	/* Network-Visible Interface */
#define	ls_enaddr	ls_ac.ac_enaddr	/* Hardware Ethernet address */
	unsigned char enaddr_set;	/* Have we set the enet addr with an ioctl */
	unsigned char laf_set;		/* Have we set the logical address filter with an ioctl */
	unsigned char	*ls_laf;	/* Address of Logical Address Filter */
	int		ls_rxin;	/* Index of next rcv DRE to fill */
	int		ls_rxout;	/* Index of next rcv DRE to read */
	int		ls_txin;	/* Index of next vacant xmit DRE */
	int		ls_txout;	/* Index of next xmit DRE to complete */
	struct ladevice *ls_addr;	/* our device */
}; 


/* Lance Ethernet controller registers */
struct ladevice
{
	unsigned short	pad;		/* rdp is at base + 2 */
	volatile unsigned short	csr_rdp;	/* Register Data Port */
	unsigned short	pad1;		/* rap is at base + 6 */
	volatile unsigned short	csr_rap;	/* Register Data Port */
};


/* Lance Initialisation Block */
struct la_ib
{
	unsigned short	ib_mode;	/* Mode Register */
	unsigned char	ib_padr[6];	/* Physical (Ethernet Station) Addr */
	unsigned char	ib_ladrf[8];	/* Logical Address Filter */
	unsigned short	ib_rdral;	/* Receive Descriptor Ring Address -
					   Least Significant Part (0 - 15) */
	unsigned char	ib_rlen;	/* Receive Ring Length */
	unsigned char	ib_rdram;	/* Receive Descriptor Ring Address -
					   Most Significant Part (16 - 23) */
	unsigned short	ib_tdral;	/* Transmit Descriptor Ring Address -
					   Least Significant Part (0 - 15) */
	unsigned char	ib_tlen;	/* Transmit Ring Length */
	unsigned char	ib_tdram;	/* Transmit Descriptor Ring Address -
					   Most Significant Part (16 - 23) */
};


/* Lance Descriptor Ring Entry */
struct la_dre
{
	unsigned short	dre_ladr;	/* Low Order Addr of Buffer (0 - 15) */
	unsigned char	dre_stat;	/* Status of Entry */
	unsigned char	dre_hadr;	/* High Order Addr of Buff (16 - 23) */
	short	dre_bcnt;	/* Buffer Byte Count */
	unsigned short	dre_mcnt;	/* Message Byte Count */
};


/* Low level lance routines */
struct eth_low {
	struct ladevice *(*e_low_probe)();
	int		(*e_low_set_lance)();
	int		(*e_low_intr)();
	unsigned short	(*e_low_swap)();
	unsigned char	(*e_low_g_stat)();
	int		(*e_low_s_stat)();
	unsigned char	(*e_low_g_hadr)();
	int		(*e_low_s_hadr)();
	caddr_t		(*e_low_bufaddr)();
	int		(*e_low_rcvcopy)();
	int		(*e_low_getmem)();
	int		(*e_low_TxCopy)();
};

extern struct eth_low *e_low_ptr;

#define LOW_ETHER(dev,y)	(e_low_ptr->e_low_/**/y)

#define	LA_SWAP(x)	LOW_ETHER(0,swap)(x)

#define	SET_STAT(x,st)	LOW_ETHER(0,s_stat)((unsigned char *)(&x), (unsigned char)(st))
#define	SET_LEN(x,st)	SET_STAT(x, st)
#define	GET_STAT(x)	LOW_ETHER(0,g_stat)((unsigned char *)(&x))
#define	GET_LEN(x)	GET_STAT(x)

#define	SET_HADR(x,st)	LOW_ETHER(0,s_hadr)((unsigned char *)(&x), (unsigned char)(st))
#define	GET_HADR(x)	LOW_ETHER(0,g_hadr)((unsigned char *)(&x))
/*

 * These structs point to the data area of the mbuf used to store the
 * network data.
 */

extern struct la_vtop {
	int virtual;
	int physical;
	} R_vtop[1 << MAX_RXDRE], T_vtop[1 << MAX_TXDRE];

/*	lastats -- statistics on operation and errors.
 */

#define	LASTATVERS	1

extern	struct	lastats {
	int	lasRcvFram;	/* framing errors */
	int	lasRcvOflo;	/* Over flow errors */
	int	lasRcvCRC;	/* CRC errors */
	int	lasRcvBuf;	/* Buf errors */
	int	lasXmitBuf;	/* Buffer ownership error */
	int	lasXmitUflo;	/* Underflow error */
	int	lasXmitLate;	/* Late collision error */
	int	lasXmitLcar;	/* Carrier dropped */
	int	lasXmitRtry;	/* Retry error */
	int	lasBabl;	/* Babble error interrupt bit */
	int	lasHeartBeat;	/* No Heart Beat error */
	int	lasMiss;	/* Missed a packet */
	int	lasMerr;	/* Error from memory system */
	int	lasInterrupts;	/* Calls to interrupt routine */
	int	lasRcvInts;	/* Receive Packet interrupts */
	int	lasXmitInts;	/* Transmit Packet interrupts */
	int	lasLostTxon;	/* Lance Transmit had dropped */
	int	lasUfloDrop;	/* Packets with underflow not retried */
	} lastats;
