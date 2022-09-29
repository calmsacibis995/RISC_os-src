#ident "$Header: if_lance.c,v 1.24 90/11/14 18:58:43 chungc Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
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
 * AMD 7990 "Lance" Ethernet Controller Driver
 *
 * Modified for Intrepid by mdove, himel, djl.
 *
 * $Log:	if_lance.c,v $
 * Revision 1.24  90/11/14  18:58:43  chungc
 * changed global variables to static.
 * 
 * Revision 1.23  90/05/23  15:15:22  huang
 * changes made to support GENESIS
 * 
 * Revision 1.22  90/03/27  18:09:53  zach
 * Brought code in line with 4.50 kernel algorithms.  Removed kernel only
 * code and dead code.  Insure lance stopped when device closed.  Added
 * support for RB3125/genesis.
 * 
 * Revision 1.20  90/02/28  22:08:10  chungc
 * added 3030 support
 * 
 * Revision 1.19  90/01/16  17:03:28  huang
 * Added $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
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
 *  $ and $Header: if_lance.c,v 1.24 90/11/14 18:58:43 chungc Exp $
 * 
 * Revision 1.18  89/02/25  18:51:04  rpharris
 * allow successive large boots WITHOUT using the "init" command
 * 
 * Revision 1.17  88/09/22  16:31:23  rpharris
 * putting stand-alone and unix versions in sync
 * for stand-alone added pointer init for bootp and mbuf len
 * subtraction for tftp.
 * Moved line that was causing bfs problems to correct location...
 * small amount of cleanup also.
 * 
 * Revision 1.16  88/09/20  12:20:07  rpharris
 * added fix for a rx'ed packet CRC which crosses an mbuf boundary.
 * 
 * Revision 1.15  88/09/14  20:25:37  rpharris
 * added queueing for tx overflows. make rx code robust in the face of
 * overflows leading to various flavors of lanced missed packets.
 * Added fix for ifconfig up and down ioctl's.
 * 
 * Revision 1.14  88/08/29  15:49:05  marker
 * Added reverse arp.
 * 
 * Revision 1.13  88/06/13  15:05:44  mdove
 * Fxied a couple bugs post release
 * 
 * Revision 1.12  88/06/02  14:20:01  mdove
 * Trailers now seem to work.  Finally.
 * 
 * Revision 1.11  88/06/01  17:09:00  mdove
 * Fixed PANIC:mfree problem and last memory leak (hopefully).  Also
 * most diagnostic messages only come out upon booting with showconfig
 * set.
 * 
 * Revision 1.10  88/05/16  18:13:36  mdove
 * Made error correcting a bit more robust.
 * 
 * Revision 1.9  88/05/08  03:05:00  mdove
 * major cleanup and rearrangment...error handling much better
 * 
 * Revision 1.8  88/05/05  10:24:47  mdove
 * Trailers initially work.  Need more testing, but no longer kill things
 * .,
 * 
 * Revision 1.7  88/04/30  14:25:18  mdove
 * Fixed up test to be much more graceful on miscompare of mbufs
 * in decriptors rings for whatever reasons.
 * 
 * Revision 1.6  88/04/12  14:44:02  mdove
 * Made timeout on dre owndership of recieve buffers a bit longer.
 * 
 * Revision 1.5  88/04/11  17:20:34  mdove
 * Only fillup received packets if they are more than one mbuf.  Otherwise
 * it is a waste of resources...
 * 
 * 
 * Revision 1.4  88/04/11  16:36:22  mdove
 * Added printing of error every 20 times...
 * 
 * Revision 1.3  88/04/11  15:23:30  mdove
 * Made NFS and rcp work.  Problems with mbuf alignment and packets of
 * illegal length.
 * 
 * Revision 1.2  88/03/29  18:25:13  mdove
 * Lance driver now functional.  May still be a small memory leak...
 * 
 * Revision 1.6  88/03/04  16:14:52  mdove
 * Just checking in a current version to save state
 * 
 * Revision 1.5  88/02/15  22:00:47  rpharris
 * Fixed static problems.
 * 
 * Revision 1.4  88/02/13  02:21:18  mdove
 * Some various changes
 * 
 * Revision 1.3  88/01/26  20:11:24  mdove
 * Fixed some minor bugs, now appears to work
 * 
 * Revision 1.2  88/01/23  17:42:31  mdove
 * Major changes required for Intrepid and M2000 (later).
 * 
 * Revision 1.1  88/01/11  05:55:16  mdove
 * Initial revision
 * 
 * Revision 1.4  88/01/07  00:58:30  djl
 * Added in-line log messages.
 * 
 *
 */

extern int LANCE_ADDR[];	/* See saio/machaddr.c for definition */
extern int ENETPROM_ADDR[];	/* See saio/machaddr.c for definition */


#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "sys/errno.h"
#include "sys/param.h"
#include "netinet/in.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "saio/socket.h"
#include "saio/debug.h"
#include "saio/arp.h"
#include "saio/ei.h"
#include "saio/mbuf.h"
#include "netinet/in_systm.h"
#include "netinet/ip.h"
#include "machine/ladefs.h"
#include "machine/lastruct.h"
#include "machine/dvh.h"

#define cei(x)	((struct ether_info *)(x->i_ino_dir))
#define panic(a)	{ printf(a); exit(-1); }

#define VtoK1(addr)	PHYS_TO_K1(addr)

extern _arpinput(), _arpresolve();
extern int Debug;

/*
 * remove and add high byte address modifier for access to VME 32 space
 * by cpu
 */

/* remove modifier */
#define	devaddr(addr)	((long)(addr) & 0x00ffffff)
/* add modifier */

/* Procedures called from Kernel */
int	laoutput(), lawatch();
int	laattach(), lainit();
int	_laopen(), _lastrategy(), _lainit();

/* External Structures Referenced */

extern struct mbuf *mbufs;

/* External Procedures Called (some indirectly via macros) */

char *malloc();

/* Declarations of Procedures returning non-integers */
struct	mbuf *laget();

/**************************/
/* Structure Declarations */
/**************************/

/* Lance Network Interface (basically struct ifnet but more) */
static struct	la_softc Ls;

/* Initialization Block */
static struct	la_ib *La_ibp;

/***********************************************************************
   Note that 1 more descriptor ring entry than required is specified to
   ensure that the descriptor ring entries start on a quad word boundary
   as required by the LANCE (see address calculations later using QUAD)
***********************************************************************/


/* Receive Descriptor Ring */
static struct	la_dre *La_rxdrep;
static char	La_rcvmarks[1 << MAX_RXDRE];	/* Set if a rcv dre is initialized */

/* Transmit Descriptor Ring */
static struct	la_dre *La_txdrep;

/* Receive Buffers */
static char	*rxbufferp;

/* Transmit Buffers */
static char	*txbufferp;

/* Globally Accessible (within this module) Variables */

static int la_havemem;			/* Have we already malloced memory */
static int la_isopen;			/* Have we opened before, done init */
static	la_Interrupt;		/* Set if interrupt routine active */

/*************/
/* laattach */
/*************/

/*
 * Make Lance available by filling in network interface record.  System 
 * will initialize the interface when it is ready to accept packets
 */

laattach(io)
struct iob *io;
{
	register struct	ladevice	*addr;
	register struct	ifnet		*ifp;
	register struct	sockaddr_in	*sin;

	XPR1(XPR_ENET,"laattach Entry\n");

	/* Set up for accessing Lance I/O Ports */

	addr = Ls.ls_addr;
	ifp = &Ls.ls_if;
	ifp->if_unit = io->i_ctlr;	/* Sub-unit Number (0) */

	ifp->if_name = "la";
	ifp->if_mtu = ETHERMTU;		/* Maximum Transmission Unit (1500) */

	/* Check if Lance installed */

	if (badaddr(&addr->csr_rap, sizeof(addr->csr_rap))) {
		printf("laattach: No Lance on this machine!\n");
		return;
	};

	set_lance(addr);

	/*
	 * This is one way to set addresses for V.0 (no ioctl()):
	 *	sin->sin_addr.s_addr = (u_long)fd->ui_flags;
	 *
	 * - continue to use last 3 bytes of station addr (from PROM) as
	 *   host number since dont want to change kernel for each machine
	 * - In V.2 it can be set with an ioctl(), but still use this as
	 *   an initial value.
	 */

	/* Set up Lance procedure calls */
	ifp->if_init = lainit;
	ifp->if_output = laoutput;
	ifp->if_watchdog = lawatch;

	/* Attach Network Interface ? */
	XPR1(XPR_ENET,"laattach: complete\n");
}

/*******************************/
/* Initialization of interface */
/*******************************/

lainit()
{
	XPR1(XPR_ENET, "lainit: entering\n");
	/* this routine is for compatibility, it is not called */
	printf("lance: This lainit message should not be printed\n");
	XPR1(XPR_ENET, "lainit: complete\n");
}

/********************************/
/*      Poll the interface      */
/********************************/


lapoll()
{
	register struct ladevice *addr;
	register short	rdp;
	static	lastmissmess;
	
	addr = Ls.ls_addr;
	rdp = addr->csr_rdp;
	if(la_Interrupt || !la_havemem)
		return;
	la_Interrupt = 1;
	while (rdp & CSR_INTR) {
		XPR2(XPR_ENET,"Lance RDP=%x\n",rdp);
		if ((rdp & CSR_BABL) != 0)
			panic("***BABBLE PACKET from Lance***\n");
		if ((rdp & CSR_MERR) != 0)
			panic("***MEMORY ERROR from Lance***\n");
		if ((rdp & CSR_MISS) != 0)
		{
			XPR1(XPR_ENET, "Lance Missed Packet\n");
			if(lastmissmess == 0) {
				/* If we start missing packets, we try to
				 * subsequently notify the user only after
				 * a period of successful arrivals.
				 */
				printf("Lance: Packet Miss\n");
			}
			lastmissmess = 100;
			addr->csr_rdp = CSR_MISS; wbflush();
			laread();
		} else if(lastmissmess > 0) {
			lastmissmess--;
		}
		if ((rdp & CSR_CERR) != 0)
		{
			printf("Lance No Heartbeat\n");
			addr->csr_rdp = CSR_CERR; wbflush();
		}
		if ((rdp & CSR_IDON) == CSR_IDON) {
			/* Clear the IDON bit */
			addr->csr_rdp = CSR_IDON; wbflush();
		}

		/* Now process expected Interrupts */
		if ((rdp & CSR_TINT) == CSR_TINT)
		{
			/* Clear the TINT bit */
			addr->csr_rdp = CSR_TINT; wbflush();
			laflushxmit();
			if(Ls.ls_if.if_snd.ifq_head)
				lastart();
		}
		if ((rdp & CSR_RINT) == CSR_RINT)
		{
			/* Clear the RINT bit */
			addr->csr_rdp = CSR_RINT; wbflush();
			laread();
		}
		rdp = addr->csr_rdp;
	}
	la_Interrupt = 0;
	return;
}

/*******************/
/* Receive Packets */
/*******************/

static
laread()
{
	unsigned short ether_type;
	register struct mbuf *m;

 	XPR1(XPR_ENET,"laread: entering\n");

	for (;;)	/* keeping pulling packets off until we are out */
	{
		m = laget(&ether_type);
		lafillrcv();
		if(!m) {
			return;
		}

		/* Bump number of packets received */
		Ls.ls_if.if_ipackets++;

		/*
		 * By the time we get here, we have a complete mbuf chain 
		 * the ether_type is correctly set, and the descriptors 
		 * have been returned to LANCE
		 */

		switch (ether_type) {

		case ETHERPUP_IPTYPE:
			_ip_input(&Ls.ls_if, m);
			break;
		case ETHERPUP_ARPTYPE:
			_arpinput(&Ls.ls_ac, m);
			break;
		case ETHERPUP_RARPTYPE:
			_rarpinput(&Ls.ls_ac, m);
			break;
		default:
			_m_freem(m);
			continue;
		}
	}
}



/********************/
/* Transmit Packets */
/********************/

/*
 * Encapsulate a packet of type family for the local net.
 */

laoutput(ifp, m0, dst)
register struct	ifnet		*ifp;
register struct	mbuf		*m0;
register struct	sockaddr	*dst;
{
	register int	type, level, error;
	u_char		edst[6];
	struct in_addr	idst;
	register struct	mbuf	*m = m0;
	register struct	ether_header	*la;

	XPR1(XPR_ENET,"laoutput: entering\n");
	switch (dst->sa_family)
	{
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;

		if (!_arpresolve(&Ls.ls_ac, &idst, edst))
			return(NULL);	/* if not yet resolved */

		type = ETHERPUP_IPTYPE;
		break;

	case AF_UNSPEC:
		XPR1(XPR_ENET,"AF_UNSPEC\n");
		la = (struct ether_header *)dst->sa_data;
		bcopy((caddr_t)la->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = la->ether_type;
		break;

	default:
		printf("la%d: can't handle af%d\n", ifp->if_unit,
		dst->sa_family);
		error = EAFNOSUPPORT;
		break;
	}

	/*
         * Add local net header.  If no space in first mbuf, allocate 
         * another.
         */
	if (m->m_off + m->m_len > MLEN || MMINOFF +
		sizeof (struct ether_header) > m->m_off)
	{
		m = _m_get();

		if (m == 0)
		{
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMAXOFF;
		m->m_len = sizeof (struct ether_header);
	} 
	else
	{
		m->m_off -= sizeof (struct ether_header);
		m->m_len += sizeof (struct ether_header);
	}
	la = mtod(m, struct ether_header *);
	bcopy((caddr_t)edst, (caddr_t)la->ether_dhost, sizeof(edst));
	la->ether_type = htons((u_short)type);
	bcopy((caddr_t)Ls.ls_enaddr, (caddr_t)la->ether_shost, 6);


	/* Queue message on interface and start output */

	if (IF_QFULL(&ifp->if_snd))
	{
		IF_DROP(&ifp->if_snd);
		error = ENOBUFS;

		goto qfull;
	}
	IF_ENQUEUE(&ifp->if_snd, m);

	lastart();

qfull:

        XPR1(XPR_ENET,"laoutput: queue full\n");

bad:
	return(error);
}


/*************************/
/* Start output on Lance */
/*************************/

/* If interface is not already active, get another datagram to send off from 
   the interface queue and start the output. */

static 
lastart()
{

	XPR1(XPR_ENET,"lastart: entering\n");

	/* Check for LANCE losing TXON, if so, reinitialise */
	if ((Ls.ls_addr->csr_rdp & CSR_TXON) != CSR_TXON)
	{
		printf("Lance has lost TXON %x\n",Ls.ls_addr->csr_rdp);
		reinit();
	}
	laput();
}


/*	lafillrcv -- fill the receive ring.
 *	We insure that the dre's for the receive ring are correctly
 *	set.  We'll get mbufs for the buffers later.
 */

lafillrcv()

{
	register struct la_dre *dre;
	register caddr_t rxbp;

	while(La_rcvmarks[Ls.ls_rxin] == 0) {
		dre = &La_rxdrep[Ls.ls_rxin];
		rxbp = &rxbufferp[Ls.ls_rxin * RXBUFSIZE];
		dre->dre_ladr = devaddr(rxbp);
		dre->dre_hadr = devaddr(rxbp) >> WORDSHIFT;
		dre->dre_bcnt = -RXBUFSIZE;
		dre->dre_mcnt = 0;
		dre->dre_stat |= S_OWN;
		La_rcvmarks[Ls.ls_rxin] = 1;
		if(++Ls.ls_rxin >= (1 << MAX_RXDRE)) {
			Ls.ls_rxin = 0;
		}
	}
}


/*	laflushxmit -- flush transmitter ring.
 *	Following a transmit interrupt bit set, analyze any errors.
 */

laflushxmit()

{
	register struct la_dre *dre;
	register unsigned stat;

	while(Ls.ls_txout != Ls.ls_txin) {
		dre = &La_txdrep[Ls.ls_txout];
		if((stat = dre->dre_stat) & S_OWN)
			return;
		if(stat & S_ERR) {
			Ls.ls_if.if_oerrors++;
		} else if(stat & S_MORE) {
			Ls.ls_if.if_collisions += 2;
		} else if(stat & S_ONE) {
			Ls.ls_if.if_collisions++;
		}
		if(stat & S_ENP)
			Ls.ls_if.if_opackets++;
		if(++Ls.ls_txout >= (1 << MAX_TXDRE))
			Ls.ls_txout = 0;
	}
}


/*
 * laget()
 *
 * Given the head of a descriptor chain, retreive the associated mbufs,
 * build them into a chain, and re-allocate mbufs to the descriptors,
 * returning them to lance.  Also, deal with reorganizing the mbufs
 * to allow for trailers.
 */

struct mbuf *
laget(ether_type)

unsigned short *ether_type;
{
	register struct mbuf *m, *m0;
	register struct la_dre *dre;
	struct ether_header *la;
	caddr_t	rxbp;
	int	len, off, resid;
	register stat;


	/* Set Up */
	/* Get address of receive buffer */
again:
	dre = &La_rxdrep[Ls.ls_rxout];
	if(((stat = dre->dre_stat) & S_OWN) == S_OWN 
	   || La_rcvmarks[Ls.ls_rxout] == 0) {
		return(NULL);
	}
	if(stat != (S_ENP | S_STP)) {
		/* Trouble, correct packet not contained in one mbuf */
		Ls.ls_if.if_ierrors++;
		do {
			if(++Ls.ls_rxout >= (1 << MAX_RXDRE))
				Ls.ls_rxout = 0;
			dre = &La_rxdrep[Ls.ls_rxout];
			stat = dre->dre_stat;
		} while((stat & (S_OWN | S_STP)) == 0);
		goto again;
	}

	rxbp = &rxbufferp[Ls.ls_rxout * RXBUFSIZE];
	La_rcvmarks[Ls.ls_rxout] = 0;
	if(++Ls.ls_rxout >= (1 << MAX_RXDRE))
		Ls.ls_rxout = 0;
	if(devaddr(rxbp) != (dre->dre_hadr << WORDSHIFT) + dre->dre_ladr) {
		printf("Lance punting on corrupted descriptor %x %x %x\n",
		       (int)rxbp, (int) dre->dre_hadr, dre->dre_ladr);
		goto again;
	}

	len = dre->dre_mcnt - sizeof(struct ether_header) - 4;
	if(len < ETHERMIN || len > ETHERMTU) {
		goto again;
	}
	/* Assert: MLEN > ETHERMTU */
	if(len > MLEN - MMAXOFF) {
		printf("lance: software bug in MLEN\n");
		goto again;
	}
	la = (struct ether_header *)rxbp;
	*ether_type = ntohs(la->ether_type);
	if (la->ether_type >= ETHERPUP_TRAIL && la->ether_type < 
			ETHERPUP_TRAIL+ETHERPUP_NTRAILER) {
		off = (*ether_type - ETHERPUP_TRAIL) * 512;
		*ether_type = ntohs(*(ushort *)(rxbp + off));
		resid = ntohs(*(ushort *)(rxbp + (off + sizeof(ushort))));
		if (off + resid > len) {
			printf("lance:  Off + resid > len\n");
			goto again;
		}
		if (resid > MLEN) {
			printf("lance: trailing header bigger than mbuf\n");
			goto again;
		}
		m0 = _m_get();
		off += 2 * sizeof(ushort);
		len -= resid + 2 * sizeof(ushort);
		resid -= 2 * sizeof(ushort);
		/* Copy the trailing header to an mbuf */
		bcopy(&rxbp[off], mtod(m0, char *), resid);
		m0->m_len = resid;
	} else {
		m0 = 0;		/* No trailing header here */
	}

	if((m = _m_get()) == NULL) {
		printf("lance: out of receive mbufs\n");
		while(m0) {
			m = m0->m_next;
			_m_freem(m0);
			m0 = m;
		}
		return(NULL);
	}
	m->m_off = MMAXOFF;
	bcopy(rxbp + sizeof(struct ether_header), mtod(m, char *), len);
	m->m_len = len;
	if(m0) {
		m0->m_next = m;
	} else {
		m0 = m;
	}
	return(m0);
} /* laget() */


/****************/
/* set up lance */
/****************/

set_lance(addr)
struct ladevice *addr;
{
	register struct	la_dre *dre;
	register int	i;

	/* Initialise Address of Logical Address Filter */
	Ls.ls_laf = (unsigned char *)LAFILTER;

	XPR2(XPR_ENET,"Setting Up Lance CSRs: addr=0x%x\n",(int)addr);

	/* Set up CSR0-3 */
	addr->csr_rap = CSR0; wbflush();
	addr->csr_rdp = CSR_STOP; wbflush();
	i = 0;
	while(addr->csr_rdp != CSR_STOP) {
		if(++i > 0xf00f00) {
			printf("set_lance: Can't stop lance\n");
			exit(-1);
		}
	}
	if (!la_havemem) {		/* Only allocate memory ONCE! */
		if (!lagetmem()) {
			printf("set_lance: failed to get memory\n");
			exit(-1);
		};
		la_havemem = 1;
	};
	la_Interrupt = 0;
	addr->csr_rap = CSR1; wbflush();
	addr->csr_rdp = devaddr(La_ibp); wbflush();
	addr->csr_rap = CSR2; wbflush();
	addr->csr_rdp = devaddr(La_ibp) >> WORDSHIFT; wbflush();
	addr->csr_rap = CSR3; wbflush();
 	addr->csr_rdp = BSWP; wbflush();		/* Byte Swap */
	addr->csr_rap = CSR0; wbflush();

	/* Initialise Current Descriptor Ring Entry Addresses & Counts */
	Ls.ls_rxin = Ls.ls_txin = 0;
	Ls.ls_rxout = Ls.ls_txout = 0;

	/* Get Station Address */
	getstation();


	/* Set up Initialization Block */

	La_ibp->ib_mode = 0;

	/* XXX is this right or wrong, the swapping here?? */
	for (i = 0; i < STATADDRSIZE; i += 2)
	{
		La_ibp->ib_padr[i] = Ls.ls_enaddr[i+1];
		La_ibp->ib_padr[i+1] = Ls.ls_enaddr[i];
	}

	for (i = 0; i < LAFSIZE; i += 2)
	{
		La_ibp->ib_ladrf[i] = *(Ls.ls_laf + i + 1);
		La_ibp->ib_ladrf[i+1] = *(Ls.ls_laf + i);
	}

	/* set up receive and transmit ring descriptor pointers */
	La_ibp->ib_rdral = devaddr(La_rxdrep);		/* LSP */
	La_ibp->ib_rdram = devaddr(La_rxdrep) >> WORDSHIFT;/* MSP */
	/* NB: length here is log2(number of descriptors) */
	La_ibp->ib_rlen = MAX_RXDRE << 5;			/* len */

	La_ibp->ib_tdral = devaddr(La_txdrep);			/* ibid */
	La_ibp->ib_tdram = devaddr(La_txdrep) >> WORDSHIFT;
	La_ibp->ib_tlen = MAX_TXDRE << 5;

	XPR1(XPR_ENET,"Setting Up Descriptor Rings\n");
	/* Set up Descriptor Rings */
	for (dre = La_rxdrep, i = 0; dre < &La_rxdrep[1 << MAX_RXDRE];
				++dre, ++i)
	{
		dre->dre_stat = 0;
		La_rcvmarks[i] = 0;
	}
	lafillrcv();
	for (dre = La_txdrep; dre < &La_txdrep[1 << MAX_TXDRE]; ++dre)
	{
		dre->dre_stat = 0;
	}
}

/***************/
/* Reset Lance */
/***************/

lareset(addr)
struct ladevice *addr;
{
	int	rdp;
	int	i;

	XPR1(XPR_ENET, "lareset: entry\n");

	/* Set Initialise Bit in CSR0 */
	addr->csr_rap = CSR0; wbflush();
	addr->csr_rdp = CSR_STOP; wbflush();
	i = 0;
	while(addr->csr_rdp != CSR_STOP) {
		if(++i > 0xf00f00) {
			printf("lareset: Can't stop lance\n");
			exit(-1);
		}
	}
	la_Interrupt = 0;	/* If we timeout in arp or protocol code, this
				 * flag can stay set.
				 */
	Ls.ls_txout = Ls.ls_txin = 0;	/* some protective code for _laclose */
	addr->csr_rdp = CSR_INIT; wbflush();

	/* Wait for Initialization Complete */
	XPR1(XPR_ENET,"Waiting for IDON\n");
	while (((rdp = addr->csr_rdp) & CSR_IDON) != CSR_IDON) {
		DELAY(1);
	}
	addr->csr_rdp = CSR_STRT; wbflush();
	/* Clear IDON bit */
	addr->csr_rdp = CSR_IDON; wbflush();  /* No interrupts for standalone */
	XPR1(XPR_ENET, "lareset: complete\n");
}


/************/
/* Watchdog */
/************/

int	lawatch()
{
	XPR1(XPR_ENET, "lawatch entry\n");
	/* Do nothing */
}


/*********************************/
/* Get Station Address from PROM */
/*********************************/

static
getstation()
{
	u_char	stataddr[6];
	u_char	*enetp;
	int i;

	XPR1(XPR_ENET, "Getstation entry\n");
	enetp = (u_char *)MACHDEP(ENETPROM_ADDR) + 3;
	for (i=0; i<6 ; i++)
		stataddr[i] = enetp[4*i];
	bcopy((caddr_t)stataddr, (caddr_t)Ls.ls_enaddr, sizeof(stataddr));
	XPR1(XPR_ENET, "Getstation Complete\n");
}

/**********************/
/* Reinitialise Lance */
/**********************/

reinit()
{
	register volatile struct	ladevice	*addr;
	register int	level;

	XPR1(XPR_ENET, "reinit: entry\n");


	/* Set up for accessing Lance I/O Ports */

	set_lance(Ls.ls_addr);	/* set up lance again */
	lareset(Ls.ls_addr);		/* set STRT bit in lance */
	

	XPR1(XPR_ENET, "reinit: complete\n");
}

/*
 * Standalone entry points
 */

/*
 * _lainit -- setup for STANDALONE
 */
_lainit()
{
	register struct ladevice *addr;

	XPR1(XPR_ENET, "_lainit: entering\n");
	if (IS_RB3125) {
		*((long *) PHYS_TO_K1(CPU_CR_RB3125)) |= CR_LNCRESETB;
	}
	Ls.ls_addr = (struct ladevice *)MACHDEP(LANCE_ADDR);
	Ls.ls_addr->csr_rap = CSR0;	wbflush();
	Ls.ls_addr->csr_rdp = CSR_STOP; wbflush();
	la_Interrupt = 0;
	la_isopen = 0;		    		/* Make us re-initialize */
	_init_arp();
	XPR1(XPR_ENET, "_lainit: complete\n");
}

/*
 * _laopen -- setup LANCE chip and initialize network driver data structs
 *	      for STANDALONE
 */

_laopen(io)
struct iob *io;
{
        register struct la_softc *ls;
	register struct ifnet	*ifp;
	struct sockaddr_in *sin;
	char *cp;
	extern struct in_addr inet_addr();
	extern char *getenv();

	XPR1(XPR_ENET,"_laopen: entering\n");
	if (Ls.ls_addr == NULL) {
	    printf("_laopen: lance non-existent on this machine\n");
	    return(-1);
	}

	io->i_devaddr = (unsigned)Ls.ls_addr;
	cei(io)->ei_registry = -1;	/* socket not bound yet */
	ls = &Ls;
	if (!la_isopen) {
		laattach(io);

		XPR1(XPR_ENET,"_laopen: returned from lareset()\n");
		ifp = &Ls.ls_if;
		sin = (struct sockaddr_in *) &ifp->if_addr;
		sin->sin_family = AF_INET;
		cei(io)->ei_acp = &ls->ls_ac;	/* iob ptr to arpcom */

#if !defined(SABLE)
		cp = getenv("netaddr");
#else
		cp = "0.0.0.0";			/* So SABLE sash will work */
#endif
		if (!cp || !*cp)
			goto bad;
		sin->sin_addr = inet_addr(cp);	/* Form internet address */
		if (*(int *)&sin->sin_addr == -1) {
bad:
			printf("_laopen: $netaddr not set or incorrect\n");
			io->i_errno = EADDRNOTAVAIL;
			return(-1);
		}
		ifp->if_snd.ifq_maxlen = 1;
		lareset(Ls.ls_addr);
		la_isopen = 1;
	} else {
		_init_mbufs();		/* reinit buffer pool */
		reinit();		/* Get things in sync */
	}
	XPR2(XPR_ENET,"_laopen: netaddr %s\n",cp);
	io->i_flgs |= F_SCAN;
	XPR1(XPR_ENET,"_laopen: complete\n");
	return(NULL);
}

/*
 * _lastrategy -- strategy routine used only for standalone.
 */

_lastrategy(io, func)
register struct iob *io;
{
	register struct mbuf *m;
	struct so_table *st;
	int ocnt;

	XPR1(XPR_ENET,"_lastrategy: entering\n");
	if (cei(io)->ei_registry < 0) {
bad:
		printf("_lastrategy: socket not bound\n");
		io->i_errno = EINVAL;
		return (-1);
	}
	st = &_so_table[cei(io)->ei_registry];	/* Find socket */
	if (st->st_count <= 0) {
		printf("_lastrategy: socket screw-up\n");
		goto bad;
	}

	if (func == READ) {
		while ((io->i_flgs & F_NBLOCK) == 0 && st->st_mbuf == NULL)
			_scandevs();		/* Spin until we have data */

		/*
		 * It's all or nothing when reading a packet
		 */
		if (m = _so_remove(st)) {
			ocnt = _min(m->m_len, io->i_cc);
			bcopy(mtod(m, char *), io->i_ma, ocnt);
			bcopy((char *)&m->m_srcaddr,
			    (char *)&cei(io)->ei_srcaddr,
			    sizeof(cei(io)->ei_srcaddr));
			_m_freem(m);
			return(ocnt);
		} else
			return(NULL);
	} else if (func == WRITE) {
		m = _m_get();
		if (m == 0) {
			printf("_lastrategy: out of mbufs\n");
			io->i_errno = EIO;
			return (-1);
		}
		if (io->i_cc > MLEN - MMAXOFF) {
			_m_freem(m);
			printf("_lastrategy: datagram too large\n");
			io->i_errno = EIO;
			return(-1);
		}
		m->m_off = MMAXOFF;		/* Headers less than MMAXOFF */
		bcopy(io->i_ma, mtod(m, char *), io->i_cc);
		m->m_len = io->i_cc;
		ocnt = _udp_output(io, &Ls.ls_if, m);
		return(ocnt < 0 ? -1 : io->i_cc);
	} else
		_io_abort("cmc bad function");
	/*NOTREACHED*/
}

/*
 * _laclose -- standalone version of close of the device.
 */

_laclose(io)
struct iob *io;
{
	int	i;
	XPR1(XPR_ENET,"_laclose: closing\n");
	i = 0;
	while(Ls.ls_txout != Ls.ls_txin) {
		lapoll();		/* Spin until data gone */
		if(++i > 0xf00)
			break;
	}
	Ls.ls_addr->csr_rap = CSR0;	wbflush();
	Ls.ls_addr->csr_rdp = CSR_STOP; wbflush();
	if (cei(io)->ei_registry >= 0)
		_free_socket(cei(io)->ei_registry);
	la_isopen = 0;
}

/*
 * Process an ioctl request.
 */
_laioctl(io, cmd, arg)
struct iob *io;
int cmd, arg;
{
	struct so_table *st;
	struct sockaddr_in sin;
	register struct volume_header *vh;
	void get_netfile();

	switch (cmd) {
	case NIOCBIND:
		/*
		 * scan registry table, add new entry if entry for port
		 * doesn't already exist
		 */
		if (cei(io)->ei_registry >= 0) {
			printf("already bound\n");
			return(-1);
		}
		cei(io)->ei_udpport = arg;
		if (st = _find_socket(arg)) {
			cei(io)->ei_registry = st - _so_table;
			return(NULL);
		}

		/* find an empty slot */
		st = _get_socket();
		if (st == NULL) {
			printf("out of socket buffers\n");
			return(-1);
		}

		XPR4(XPR_ENET,"binding to %d 0x%x %d\n", sin.sin_family,
		    sin.sin_addr, ntohs((u_short)sin.sin_port));

		st->st_udpport = arg;
		cei(io)->ei_registry = st - _so_table;
		break;

	case FIOCSCAN:
		lapoll();
		break;

	case DIOCGETVH:
		vh = (struct volume_header *)arg;
		vh->vh_rootpt = 0;
		(void) get_netfile(vh->vh_bootfile);
		break;
	}
	return(NULL);
}



#define	NEEDED 	7 + sizeof (struct la_dre) * (1 << MAX_RXDRE) + sizeof (struct la_dre) * (1 << MAX_TXDRE) + sizeof *La_ibp + RXBUFSIZE * (1 << MAX_RXDRE) + TXBUFSIZE * (1 << MAX_TXDRE) 

lagetmem()
{
	char	*mem;

	if(IS_RB3125) {
		register long *lp;

		/* XXX use symbolic names */
		mem = (char *)PHYS_TO_K1(0x1f000000);
		for(lp = (long *)mem; lp < (long *)PHYS_TO_K1(0x1f100000); ++lp)
			*lp = 0;
	} else {
		mem = (char *)malloc(NEEDED);
		if(((int)mem & 0x1fffffff) > 0x00ffffff) {
			printf("lance: malloc error, memory addr > 24 bits\n");
			exit(-1);
		}
	}

	/*
	 * La_rxdrep and La_txdrep must be on an 8 byte boundary
	 * the others need only be on an even byte address
	 */
	/* fill in globals with values pointing into above allocated memory */
	mem = (char *)(((int)mem + 7) & ~7);
	La_rxdrep = (struct la_dre *)VtoK1(mem);
	mem += sizeof (struct la_dre) * (1 << MAX_RXDRE);
	La_txdrep = (struct la_dre *)VtoK1(mem);
	mem += sizeof (struct la_dre) * (1 << MAX_TXDRE);
	rxbufferp = mem;
	mem += RXBUFSIZE * (1 << MAX_RXDRE);
	txbufferp = mem;
	mem += TXBUFSIZE * (1 << MAX_TXDRE); 
	La_ibp = (struct la_ib *)VtoK1(mem);
	return(1);
}

/*
 * laput()
 *
 * Given a pointer into the transmit ring, and the head of an mbuf chain,
 * enqueue to data to the LANCE, and set STP and ENP correctly.
 *
 */
laput()

{
	register struct mbuf *m, *n;
	register struct la_dre *dre;
	caddr_t	physaddr;
	register len;
	int avail;

	if (Ls.ls_txin == Ls.ls_txout)
		avail = (1 << MAX_TXDRE) - 1;
	else if (Ls.ls_txin < Ls.ls_txout)
		avail = Ls.ls_txout - Ls.ls_txin - 1;
	else
		avail = (1 << MAX_TXDRE) - (Ls.ls_txin - Ls.ls_txout) - 1;
	if (avail <= 0)
		return;
	if (avail < (1 << MAX_TXDRE) / 2)
		laflushxmit();
	IF_DEQUEUE(&Ls.ls_if.if_snd, m);
	while(m) {
		dre = &La_txdrep[Ls.ls_txin];
		if(dre->dre_stat & S_OWN) {
			printf("lance: software error in xmit buffer handling\n");
bailout:
			while(m) {
				n = m->m_next;
				_m_freem(m);
				m = n;
			}
			return;
		}
		physaddr = &txbufferp[Ls.ls_txin * TXBUFSIZE];
		dre->dre_ladr = devaddr(physaddr);
		dre->dre_hadr = devaddr(physaddr) >> WORDSHIFT;
		len = 0;
		while (m) {
			if(len + m->m_len > ETHERMTU) {
				printf("lance: output packet > ETHERMTU\n");
				goto bailout;
			}
			bcopy(mtod(m, char *), physaddr, m->m_len);
			len += m->m_len;
			n = m->m_next;
			m->m_next = 0;
			_m_freem(m);
			m = n;
		}
		if(len < MINPKTSIZE)
			len = MINPKTSIZE;
		dre->dre_mcnt = 0;
		dre->dre_bcnt = -len;
		dre->dre_stat = S_OWN | S_STP | S_ENP;
		if (++Ls.ls_txin >= (1 << MAX_TXDRE))
			Ls.ls_txin = 0;

		if(--avail <= 0)
			break;
		IF_DEQUEUE(&Ls.ls_if.if_snd, m);
	}

} /* laput() */

/*	m_goodm -- check mbuf in limits.
 *	     if_lance supplies this to all standalone ether drivers.
 */

int
m_goodm(m)
struct mbuf *m;
{
	return ( ( (int)m >= (int)&mbufs[0] ) && 
		 ( (int)m <= (int)&mbufs[NMBUFS] ) );
}
