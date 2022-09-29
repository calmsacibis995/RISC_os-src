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
#ident	"$Header: if_en.c,v 1.13.4.2 90/05/10 04:22:02 wje Exp $"

/* 'Driver' for Silicon Graphics EN1 Enthernet Controller
 */


#include "../tcp-param.h"
#include "sys/sbd.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/errno.h"
#include "sys/uio.h"
#include "sys/edt.h"
#include "sys/debug.h"

#include "../net/soioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#ifdef INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/if_ether.h"
#endif INET

#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif NS

extern  struct ifnet loif;



#define MAXBD 2				/* allow 2 boards */

/* this generates some extra code to check problems in the board */
#define CK_ADDR				/* check descriptor ring ptrs */

/* be noisy about some arbirary things */
#define NOISY


/* This is a bad kludge.  We need 2 addresses per board from the EDT.
 *	Unfortunately, we get only one.
 */
#define BD1_MEM	0xbcc00000		/* 1st board memory */
#define BD1_IO 0xbd003000		/* 1st board I/O */
#define BD2_MEM	0xbc700000
#define BD2_IO 0xbd003100


#define DOG 2				/* watchdog duration */

#define LOG_RMD 7
#define NUM_RMD (1<<LOG_RMD)		/* # of receive buffers */
#define LOG_TMD 5
#define NUM_TMD (1<<LOG_TMD)		/* # of transmit buffers */

#define CRC_LEN 4			/* bytes/ethernet CRC */

/* maximum & minimum packet sizes */
#define MAX_TPKT 1514			/* transmit len--7990 adds cksum */
#define MIN_TPKT 60
#define MAX_RPKT (MAX_TPKT+CRC_LEN)	/* receive length */
#define MIN_RPKT (MIN_TPKT+CRC_LEN)

#define TRAILLEN 512			/* the magic 512 for trailers */


#define EADDR_LEN 6			/* you cannot change this */
typedef struct {
	u_char	b[EADDR_LEN];
} ETHER_ADDR;


/* convert from 32-bit VME address to 24-bit on-board address */
#define LO_ADDR(a) ((u_short)(a) & 0xffff)
#define HI_ADDR(a) ((u_char)((((u_int)(a)) >> 16) & 0xff))

/* combine parts of an on-board address */
#define BD_ADDR(lo, hi) ((lo) + ((hi)<<16))

/* generate VME address from an on-board address */
#define VME_ADDR(lo,hi,base) (BD_ADDR((lo),(hi)) + (u_char*)(base))


/* shape of I/O space of a single board
 */
typedef volatile struct {
	u_short	io_pad1;		/* 0x00 */
	u_short	io_rdp;			/* 0x02 7990 register data port */
	u_short	io_pad2;		/* 0x04 */
	u_short	io_rap;			/* 0x06 7990 register address port */
	u_short	io_pad3;		/* 0x08 */
	u_short	io_intr;		/* 0x0A interrupt type */
	u_short	io_pad4;		/* 0x0C */
	u_short	io_reset;		/* 0x0E board reset */
	u_short io_pad5[8];		/* 0x10-0x1E */
	struct {			/* 0x22-0x36 ethernet address */
		u_short ad_pad;
		u_short ad_byte;	/* only 1 byte of this word is valid */
	} io_ad[EADDR_LEN];
} *ENIO;


#define BASE ((ENMEM)0)			/* start board data structures here */
#define EOM 0x3ffff			/* end of board memory */


/* 7990 'CSR' values */
#define CSR0_ERR	0x8000		/* any error */
#define CSR0_BABL	0x4000		/* babble */
#define CSR0_CERR	0x2000		/* collistion error */
#define CSR0_MISS	0x1000		/* missed packet */
#define CSR0_MERR	0x0800		/* memory error */
#define CSR0_RINT	0x0400		/* receiver interrupt */
#define CSR0_TINT	0x0200		/* transmitter interrupt */
#define CSR0_IDON	0x0100		/* initialization done */
#define CSR0_INTR	0x0080		/* any interrupt */
#define CSR0_INEA	0x0040		/* interrupt enable */
#define CSR0_RXON	0x0020		/* receiver on */
#define CSR0_TXON	0x0010		/* transmitter on */
#define CSR0_TDMD	0x0008		/* transmit demand */
#define CSR0_STOP	0x0004		/* stop everything */
#define CSR0_STRT	0x0002		/* start everything */
#define CSR0_INIT	0x0001		/* initialize */
#define CSR1 LO_ADDR(&BASE->ib)
#define CSR2 HI_ADDR(&BASE->ib)
#define CSR3 0x6			/* BSWP=1 ACON=1 BCON=0 */

/* bits to set in 7990 initialization block mode word */
#define IB_PROM		0x8000		/* promiscuous mode */
#define IB_INTL		0x0040		/* internal loop back */
#define IB_DRTY		0x0020		/* disable retries */
#define IB_COLL		0x0010		/* force collision */
#define IB_DTCR		0x0008		/* disable transmit CRC */
#define IB_LOOP		0x0004		/* enable loopback */
#define IB_DTX		0x0002		/* disable transmitter */
#define IB_DRX		0x0001		/* disable receiver */


/* 7990 initialization block */
struct ib {
	u_short ib_mode;
	ETHER_ADDR ib_padr;		/* shuffled ethernet address */
	u_short ib_ladrf[4];		/* logical address filter */
	u_short ib_lrdra;		/* lsb receive ring address */
	u_int ib_rlen:3;		/* receive ring address */
	u_int ib_rpad:5;
	u_int ib_hrdra:8;		/* msb receive ring address */
	u_short ib_ltdra;		/* lsb transmit ring address */
	u_int ib_tlen:3;		/* transmit ring length */
	u_int ib_tpad:5;
	u_int ib_htdra:8;		/* msb transmit ring address */
};

/* receive descriptors--8byte aligned */
struct rmd {
	u_short rmd_ladr;		/* lo buffer address */
	u_int rmd_own:1;		/* 0x80 0=owned by cpu, 1=7990 */
	u_int rmd_err:1;		/* 0x40 error */
	u_int rmd_fram:1;		/* 0x20 framing error */
	u_int rmd_oflo:1;		/* 0x10 silo overflow */
	u_int rmd_crc:1;		/* 0x08 CRC error */
	u_int rmd_buff:1;		/* 0x04 buffer chaining bug */
	u_int rmd_stp_enp:2;		/* 0x03 start/end of packet */
	u_int rmd_hadr:8;		/* high byte of address */
	u_short rmd_bcnt;		/* negative buffer size */
	u_short rmd_mcnt;		/* message byte count */
};

/* transmit descriptors--8byte aligned */
struct tmd {
	u_short tmd_ladr;		/* lo buffer address */
	u_int tmd_own:1;		/* 0x80 0=owned by cpu, 1=7990 */
	u_int tmd_err:1;		/* 0x40 error */
	u_int tmd_res:1;		/* 0x20 (reserved) */
	u_int tmd_try:2;		/* 0x18 retries required */
	u_int tmd_def:1;		/* 0x04 chip deferred to others */
	u_int tmd_stp_enp:2;		/* 0x03 start/end of packet */
	u_int tmd_hadr:8;		/* high byte of address */
	u_short tmd_bcnt;		/* negative buffer size */
	u_int tmd_buff:1;		/* 0x8000 buffer chaining bug */
	u_int tmd_uflo:1;		/* 0x4000 silo underflow */
	u_int tmd_res2:1;		/* 0x2000 */
	u_int tmd_lcol:1;		/* 0x1000 late collision */
	u_int tmd_lcar:1;		/* 0x0800 loss of carrier */
	u_int tmd_rtry:1;		/* 0x0400 retried but failed */
	u_int tmd_tdr:10;		/* 0x03ff time domain reflectometry */
};


/* shape of local memory on a single board
 */
typedef volatile struct {
	struct ib ib;

	struct rmd rmd[NUM_RMD];

	struct tmd tmd[NUM_TMD];

	u_char rbuf[NUM_RMD][MAX_RPKT];	/* packet buffers */
	u_char tbuf[NUM_TMD][MAX_TPKT];
} *ENMEM;


/* Each interface is represented by a network interface structure,
 * en_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface,
 * its address, ...
 */
struct  en_info {
	struct  arpcom ei_ac;           /* common ethernet structures */
	ETHER_ADDR ei_addr;		/* board ethernet address */
	int	ei_lostintrs;		/* count lost interrupts */

	int	ei_sick;		/* 2=reset board on next watchdog */

	char	*ei_terr;		/* recent transmit error msg or NULL */

	u_int	ei_ract;		/* next active receive buffer */

	u_int	ei_tact;		/* 1st active transmit buffer */
	u_int	ei_tnum;		/* # of active transmit buffers */
	u_int	ei_tfree;		/* next free transmit buffer */

	long	ei_tdr;			/* sum of all TDR samples */
	int	ei_numtdr;		/* # of TDR samples */

	ENIO	ei_io;			/* I/O addresses */
	ENMEM	ei_mem;			/* start of memory */
} en_info[MAXBD];
#define ei_if           ei_ac.ac_if     /* network-visible interface */
#define ei_enaddr       ei_ac.ac_enaddr /* ethernet address */



/* reset all possible boards in the system
 *	rather a kludge, but what can you do?  We really need to do a hardware
 *	reset of the entire VME world during reboot.
 */
static
enreset_all(unit)
register int unit;
{
#define RESET(ad) (void)wbadaddr(&((ENIO)PHYS_TO_K1(ad))->io_reset, \
			       sizeof(((ENIO)PHYS_TO_K1(ad))->io_reset));
	register int s = splimp();

	switch (unit) {
	case 0:
		RESET(BD1_IO);
		break;
	case 1:
		RESET(BD2_IO);
		break;
#if MAXBD != 2
	? ? ? add the additional reset commands;
#endif
	}
	wbflush();
	splx(s);
}

/* Initialize a board
 *	Interrupts must already be off.
 */
static
enbdinit(ei)
register struct en_info *ei;
{
	register int i;
	register u_int baddr;
	register volatile struct rmd *rmd;
	register volatile struct tmd *tmd;
	register ENIO io;
	register ENMEM mem;

	io = ei->ei_io;
	mem = ei->ei_mem;

	io->io_reset = 1; wbflush();	/* reset the board */

	ei->ei_sick = 0;		/* stop watchdog resets */
	ei->ei_terr = NULL;

	ei->ei_ract = 0;		/* start with the 1st buffers */
	ei->ei_tact = 0;
	ei->ei_tnum = 0;
	ei->ei_tfree = 0;

	bzero((char*)&ei->ei_mem->ib, sizeof(ei->ei_mem->ib));
	mem->ib.ib_padr.b[0] = ei->ei_addr.b[1]; /* set initialization block */
	mem->ib.ib_padr.b[1] = ei->ei_addr.b[0];
	mem->ib.ib_padr.b[2] = ei->ei_addr.b[3];
	mem->ib.ib_padr.b[3] = ei->ei_addr.b[2];
	mem->ib.ib_padr.b[4] = ei->ei_addr.b[5];
	mem->ib.ib_padr.b[5] = ei->ei_addr.b[4];
	mem->ib.ib_rlen = LOG_RMD;
	mem->ib.ib_lrdra = LO_ADDR((u_int)&BASE->rmd[0]);
	mem->ib.ib_hrdra = HI_ADDR((u_int)&BASE->rmd[0]);
	mem->ib.ib_tlen = LOG_TMD;
	mem->ib.ib_ltdra = LO_ADDR((u_int)&BASE->tmd[0]);
	mem->ib.ib_htdra = HI_ADDR((u_int)&BASE->tmd[0]);

	bzero((char*)&ei->ei_mem->rmd[0], sizeof(ei->ei_mem->rmd));
	baddr = (u_int)&BASE->rbuf[0][0];
	for (i = 0, rmd = &mem->rmd[0];
	     i < NUM_RMD;
	     i++, rmd++, baddr += sizeof(mem->rbuf[0])) {
		rmd->rmd_ladr = LO_ADDR(baddr);
		rmd->rmd_hadr = HI_ADDR(baddr);
		rmd->rmd_bcnt = -MAX_RPKT;
		rmd->rmd_own = 1;
	}

	bzero((char*)&ei->ei_mem->tmd[0], sizeof(ei->ei_mem->tmd));
	baddr = (u_int)&BASE->tbuf[0][0];
	for (i = 0, tmd = &mem->tmd[0];
	     i < NUM_TMD;
	     i++, tmd++, baddr += sizeof(mem->tbuf[0])) {
		tmd->tmd_ladr = LO_ADDR(baddr);
		tmd->tmd_hadr = HI_ADDR(baddr);
		tmd->tmd_stp_enp = 3;
	}

	io->io_rap = 1; wbflush();	/* start the chip */
	io->io_rdp = CSR1; wbflush();

	io->io_rap = 2; wbflush();
	io->io_rdp = CSR2; wbflush();

	io->io_rap = 3; wbflush();
	io->io_rdp = CSR3; wbflush();

	io->io_rap = 0; wbflush();
	io->io_rdp = CSR0_INEA|CSR0_INIT|CSR0_STRT;
	wbflush();
}


/* reset the board
 */
static
enclear(ei)
register struct en_info *ei;
{
	register int s;

	ASSERT(0 != ei->ei_io);

	s = splimp();
	ei->ei_if.if_timer = 0;		/* turn off watchdog */
	ei->ei_if.if_flags &= ~IFF_RUNNING;

	ei->ei_io->io_reset = 1; wbflush();	/* reset the board */

	splx(s);
}


/* initialize interface
 */
static
enifinit(unit)
int unit;
{
	register struct en_info *ei = &en_info[unit];
	register struct ifnet *ifp = &ei->ei_if;
	int s;

	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;			/* quit if no address */

	if (!(ifp->if_flags & IFF_RUNNING)) {
		s = splimp();
		enbdinit(ei);		/* clear the board */
		ei->ei_if.if_flags |= IFF_RUNNING;
		ei->ei_if.if_timer = DOG;	/* turn on watchdog */
		splx(s);
	}
}


/* Periodically poll the controller for input packets in case
 * an interrupt gets lost.
 */
static
enwdog(unit)
register int unit;
{
	register struct en_info *ei = &en_info[unit];
	int s;

	s = splimp();
	if (if_enintr(unit)) {
		ei->ei_lostintrs++;
	} else if (ei->ei_tnum != 0
		   && ei->ei_sick >= 1) {
		dri_printf("en%d: died and restarted\n", unit);
		enbdinit(ei);
		ei->ei_if.if_oerrors++;
	}

	if (ei->ei_tnum != 0)		/* still have output? */
		ei->ei_sick++;		/* yes, we may be sick */
	else
		ei->ei_sick = 0;
	ei->ei_if.if_timer = DOG;

	splx(s);
}


/* Ethernet interface interrupt.
 *	come here at splimp()
 */
if_enintr(unit)
int unit;
{
	register ENMEM mem;
	register ENIO io;
	register volatile struct rmd *rmd;
	register volatile struct tmd *tmd;
	register struct en_info *ei;
	register int i;
	int found = 0;

	ASSERT(unit >= 0 && unit < MAXBD);
	ei = &en_info[unit];
	mem = ei->ei_mem;
	io = ei->ei_io;

	if (!io) {				/* ignore early interrupts */
		enreset_all(unit);
		dri_printf("en%d: early interrupt\n", unit);
		return 1;
	}

	while ((i = io->io_intr) != 0xfffc) {	/* interrupt pending? */
		found = 1;
		ei->ei_if.if_timer = DOG;	/* yes, postpone watchdog */

		if (i & 2) {
			dri_printf("en%d: memory parity\n", unit);
			enbdinit(ei);
			ei->ei_if.if_oerrors++;
			return 1;
		}

		io->io_rap = 0; wbflush();
		i = io->io_rdp;
		if (!(i & CSR0_INTR)) {
			dri_printf("en%d: missing INTR bit\n", unit);
			enbdinit(ei);
			ei->ei_if.if_oerrors++;
			return 1;
		}

		if (i & (CSR0_RINT|CSR0_TINT)) {	/* note data int. */
			io->io_rdp = CSR0_INEA|CSR0_RINT|CSR0_TINT;
			continue;
		}

		if (i & CSR0_MERR) {
			dri_printf("en%d: memory timeout\n", unit);
			enbdinit(ei);
			ei->ei_if.if_ierrors++;
			return 1;
		}

		if (i & CSR0_BABL) {
			dri_printf("en%d: babbling\n", unit);
			enbdinit(ei);
			ei->ei_if.if_oerrors++;
			return 1;
		}

		if (i & CSR0_MISS) {	/* count dropped packets */
			io->io_rdp = CSR0_INEA|CSR0_MISS;
			ei->ei_if.if_ierrors++;
			continue;
		}

		if (i & CSR0_IDON) {	/* forget about init. int. */
			io->io_rdp = CSR0_INEA|CSR0_IDON;
			continue;
		}

		dri_printf("en%d: unknown interrupt\n", unit);
		enbdinit(ei);
		ei->ei_if.if_oerrors++;
		return 1;
	}

	i = ei->ei_ract;		/* process received packets */
	rmd = &mem->rmd[i];
	for (;;) {
		if (rmd->rmd_own)
			break;
		found = 1;
		if (enrint(ei,mem,rmd)) {
			enbdinit(ei);
			ei->ei_if.if_ierrors++;
			break;
		} 
		rmd->rmd_own = 1;
		rmd++;
		if (++i >= NUM_RMD) {
			i = 0;
			rmd = &mem->rmd[0];
		}
		ei->ei_ract = i;
	}


	i = ei->ei_tact;		/* process completed packets */
	tmd = &mem->tmd[i];
	while (ei->ei_tnum != 0) {
		register char *terr = NULL;

		if (tmd->tmd_own)	/* only completed packets */
			break;

		ei->ei_sick = 0;	/* stop watchdog resets */
		found = 1;
		if (tmd->tmd_buff) {
			dri_printf("en%d: transmit 'buff' error\n", unit);
			enbdinit(ei);
			ei->ei_if.if_oerrors++;
			break;
		}

		if (tmd->tmd_uflo) {
			dri_printf("en%d: transmit underflow\n", unit);
			enbdinit(ei);
			ei->ei_if.if_oerrors++;
			break;
		}

		if (tmd->tmd_err) {
			ei->ei_if.if_oerrors++;
			if (tmd->tmd_rtry) {
				ei->ei_if.if_collisions += 16;
				ei->ei_tdr += tmd->tmd_tdr;
				ei->ei_numtdr++;
			}
			if (tmd->tmd_lcol)
				terr = "en%d: late collision\n";
			else if (tmd->tmd_lcar)
				terr = "en%d: transmit: no carrier\n"; 
			if (terr != NULL && terr != ei->ei_terr)
				dri_printf(terr, unit);
		} else {
			ei->ei_if.if_collisions += tmd->tmd_try;
		}
		ei->ei_terr = terr;

		i++;			/* advance to next ring entry */
		tmd++;
		if (i >= NUM_TMD) {
			tmd = &mem->tmd[0];
			i = 0;
		}
		ei->ei_tact = i;
		ei->ei_tnum--;
	}


	return found;
}


/* Copy a packet from VME memory into mbuf(s)
 */
static struct mbuf *
enget(cp1, len1, cp2, len2, ifp)
register u_char *cp1, *cp2;		/* prepend *cp2 to *cp1 */
register int len1, len2;
struct ifnet *ifp;
{
	struct mbuf *top = 0;
	struct mbuf **mp = &top;
	u_char *mcp;

	for (;;) {
		register struct mbuf *m;
		register int len = len2;
		if (0 == len) {
			if (len1 == 0)
				return top;
			cp2 = cp1;
			len2 = len = len1;
			len1 = 0;
		}

		MGET(m, M_DONTWAIT, MT_DATA);
		if (!m) {
			m_freem(top);
			return 0;
		}

		if (ifp != 0)
			len += sizeof(ifp);
		if (len > MLEN)
			len = MLEN;
		m->m_len = len;
		m->m_off = MMINOFF;
		mcp = mtod(m, u_char *);
		if (ifp) {
			/* Prepend interface pointer to first mbuf */
			*(struct ifnet**)mcp = ifp;
			mcp += sizeof(ifp);
			len -= sizeof(ifp);
			ifp = 0;
		}
		bcopy(cp2, mcp, len);
		cp2 += len;
		len2 -= len;
		*mp = m;
		mp = &m->m_next;
	}
}


/* handle a new input packet
 */
static int				/* 0=ok 1=must reset the board */
enrint(ei,mem,rmd)
register struct en_info *ei;
register ENMEM mem;
register volatile struct rmd *rmd;
{
	register u_char *cp1, *cp2;
	register struct mbuf *m;
	short int len1, len2, enptype;
	register struct ifqueue *inq;

	if ((ei->ei_if.if_flags & (IFF_UP|IFF_RUNNING)) !=
	    (IFF_UP|IFF_RUNNING)) {
		enclear(ei);		/* stray packet from dead bd */
		return 0;
	}

	ei->ei_if.if_ipackets++;	/* count packet if we want it */

	if (3 != rmd->rmd_stp_enp) {	/* part of chain of packets? */
		dri_printf("en%d: receive packet overflow\n",ei->ei_if.if_unit);
		if (rmd->rmd_stp_enp == 2)
			ei->ei_if.if_ierrors++;	/* count error on 1st packet */
		rmd->rmd_stp_enp = 3;
		return 0;
	}

	if (rmd->rmd_buff) {
		dri_printf("en%d: receive 'buff' error\n", ei->ei_if.if_unit);
		return 1;
	}

	if (rmd->rmd_err) {		/* toss packet if it is bad */
		ei->ei_if.if_ierrors++;
		return 0;
	}

	/* Get input data length.
	 * Get pointer to ethernet header (in input buffer).
	 * Deal with trailer protocol: if type is trailer
	 * get true type from first 16-bit word past data.
	 * Remember we have a trailer-packet by setting 'off'
	 */
	len1 = rmd->rmd_mcnt;
	if (len1 > MAX_RPKT || len1 < MIN_RPKT) {
		dri_printf("en%d: bad receive packet length of %d\n",
			   ei->ei_if.if_unit, len1);
		ei->ei_if.if_ierrors++;
		return 0;
	}
	len1 -= CRC_LEN;
	cp1 = VME_ADDR(rmd->rmd_ladr, rmd->rmd_hadr, mem);
#ifdef CK_ADDR
	if (cp1 != &mem->rbuf[ei->ei_ract][0]) {
		dri_printf("en%d: broken rmd buffer ptr=%x; should be=%x\n",
			    ei->ei_if.if_unit,
			    BD_ADDR(rmd->rmd_ladr, rmd->rmd_hadr),
			    &BASE->rbuf[ei->ei_ract][0]);
		return 1;
	}
#endif
	enptype = ((struct ether_header*)(cp1))->ether_type;
	cp1 += sizeof(struct ether_header);

	if ((enptype >= ETHERTYPE_TRAIL) &&
	    (enptype < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER)) {
		register short off;

		off = (enptype - ETHERTYPE_TRAIL) * TRAILLEN;
		if (off >= ETHERMTU) {
			ei->ei_if.if_ierrors++;
			return 0;
		}
		cp2 = cp1 + off;
		enptype = *(u_short*)cp2;
		cp2 += sizeof(u_short);
		len2 = *(u_short*)cp2;
		cp2 += sizeof(u_short);

		if ((off + len2) > len1) {
			ei->ei_if.if_ierrors++;
			return 0;
		}
		len2 -= 2*sizeof(u_short);
		len1 = off + len2;

	} else {
		len2 = 0;
	}

	if (len1 <= 0) {
		return 0;
	}

	m = enget(cp1,len1, cp2,len2,	/* convert to mbuf(s) */
		   &ei->ei_if);
	if (m == 0) {
		return 0;
	}

	switch (enptype) {
#ifdef INET
	case ETHERTYPE_IP:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
#ifdef RARP
	case ETHERTYPE_RARP:
#endif RARP
		arpinput(&ei->ei_ac, m);
		return 0;
#endif INET
#ifdef NS
	case ETHERTYPE_NS:
		schednetisr(NETISR_NS);
		inq = &nsintrq;
		break;

#endif NS
	default:
		m_freem(m);
		return 0;
	}

	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
		return 0;
	}
	IF_ENQUEUE(inq, m);

	return 0;
}



/* Copy from mbuf chain to transmitter buffer in VMEbus memory.
 *	we must be at splimp().
 */
static int				/* 0 or errno */
enput(ei, m0, type, edst, esrc)
register struct en_info *ei;		/* on this device, */
struct mbuf *m0;			/* send this chain */
u_short type;				/* of this type */
u_char *edst, *esrc;			/* with these addresses */
{
	register volatile struct tmd *tmd;
	register ENMEM mem;
	register u_char *bp;
	register unsigned totlen;

	ei->ei_if.if_opackets++;

	mem = ei->ei_mem;

	/* we keep at least 1 free buffer to ensure that we walk around the
	 * ring in step with the 7990. 
	 */
	if (ei->ei_tnum >= NUM_TMD-1) {
#ifdef NOISY
		dri_printf("en%d: dropped packet\n", ei->ei_if.if_unit);
#endif
		ei->ei_if.if_oerrors++;
		IF_DROP(&ei->ei_if.if_snd);
		return ENOBUFS;
	}

	tmd = &mem->tmd[ei->ei_tfree];
	if (tmd->tmd_own) {
		dri_printf("en%d: broken tmd ownership\n", ei->ei_if.if_unit);
		ei->ei_if.if_oerrors++;
		IF_DROP(&ei->ei_if.if_snd);
		return ENOBUFS;
	}

	bp = VME_ADDR(tmd->tmd_ladr, tmd->tmd_hadr, mem);
#ifdef CK_ADDR
	if (bp != &mem->tbuf[ei->ei_tfree][0]) {
		dri_printf("en%d: broken tmd buffer ptr=%x; should be=%x\n",
			    ei->ei_if.if_unit,
			    BD_ADDR(tmd->tmd_ladr, tmd->tmd_hadr),
			    &BASE->tbuf[ei->ei_tfree][0]);
		enbdinit(ei);
		ei->ei_if.if_ierrors++;
		return 0;
	}
#endif

	/* rely on the Mips compiler to strength-reduce this */
	ei->ei_tfree = (ei->ei_tfree + 1) % NUM_TMD;
	ei->ei_tnum++;

#define ENP ((struct ether_header*)bp)
	ENP->ether_type = type;
	bcopy(edst, ENP->ether_dhost, sizeof(ENP->ether_dhost));
	bcopy(esrc, ENP->ether_shost, sizeof(ENP->ether_shost));
	bp += sizeof(*ENP);
	totlen = sizeof(*ENP);

	for (; m0; m0 = m0->m_next) {
		register unsigned len;

		len = m0->m_len;
		if (len == 0)
			continue;
		bcopy(mtod(m0, u_char *), bp, len);
		bp += len;
		totlen += len;
	}
	ASSERT(totlen <= MAX_TPKT);
	if (totlen < MIN_TPKT)
		totlen = MIN_TPKT;
	tmd->tmd_bcnt = 0-totlen;

	wbflush();
	tmd->tmd_own = 1; 		/* finish the packet */

	ei->ei_io->io_rap = 0; wbflush();
	ei->ei_io->io_rdp = CSR0_INEA|CSR0_TDMD;	/* poke the board */
	wbflush();

	return 0;
}


/* Ethernet output routine.
 *	Encapsulate a packet of type family for the local net.
 *	Use trailer local net encapsulation if enough data in first
 *	packet leaves a multiple of 512 bytes of data in remainder.
 *	If destination is this address or broadcast, send packet to
 *	loop device since we cannot talk to ourself.
 */
static int				/* 0 or error # */
enoutput(ifp, m0, dst)
register struct ifnet *ifp;
register struct mbuf *m0;
struct sockaddr *dst;
{
	int s;
	register int error = 0;
	int usetrailers;
	u_short type;
	register u_char *edst;
	ETHER_ADDR eaddr;
	register struct en_info *ei;
	register struct mbuf *mloop = 0;

	ASSERT(ifp->if_unit >= 0 && ifp->if_unit < MAXBD);
	ei = &en_info[ifp->if_unit];
	ASSERT(0 != ei->ei_io && ei->ei_if.if_unit == ifp->if_unit);

	if ((ifp->if_flags & (IFF_UP|IFF_RUNNING)) != (IFF_UP|IFF_RUNNING)) {
		error = EHOSTDOWN;

	} else switch (dst->sa_family) {
#ifdef INET
	case AF_INET:
		{
			struct in_addr idst;
			idst = ((struct sockaddr_in *)dst)->sin_addr;
			if (!arpresolve(&ei->ei_ac, m0, &idst, &eaddr,
					&usetrailers))
				return (0);     /* if not yet resolved */
			if (!bcmp(&eaddr, etherbroadcastaddr, sizeof(eaddr))) {
				mloop = m_copy(m0, 0, M_COPYALL);
				if (!mloop) {
					error = ENOBUFS;
					break;
				}
			}
			edst = (u_char*)&eaddr;
		}

		type = ETHERTYPE_IP;
		if (usetrailers) {
			register int off;

			off = ntohs((u_short)mtod(m0, struct ip *)->ip_len);
			if (off > 0
			    && (off & (TRAILLEN-1)) == 0
			    && m0->m_off >= MMINOFF + 2*sizeof(u_short)) {
				register u_short *sp;
				register struct mbuf *m;

				type = ETHERTYPE_TRAIL + off/TRAILLEN;
				m0->m_off -= 2*sizeof(u_short);
				m0->m_len += 2*sizeof(u_short);
				sp = mtod(m, u_short *);
				*sp = htons((u_short)ETHERTYPE_IP);
				sp++;
				*sp = htons((u_short)m->m_len);

				/* move 1st packet to end of chain. */
				for (m = m0; 0 != m->m_next; m = m->m_next)
					continue;
				m->m_next = m0;
				m = m0->m_next;
				m0->m_next = 0;
				m0 = m;
			}
		}
		break;
#endif INET

#ifdef NS
	case AF_NS:
		edst = &(((struct sockaddr_ns*)dst)->sns_addr.x_host);
		if (!bcmp(edst, (char*)&ns_broadhost, EADDR_LEN)) {
			mloop = m_copy(m0, 0, (int)M_COPYALL);
			if (!mloop) {
				error = ENOBUFS;
				break;
			}
		} else if (!bcmp(edst, (char*)&ns_thishost, EADDR_LEN)) {
			return(looutput(&loif, m0, dst));
		}
		type = ETHERTYPE_NS;
		break;
#endif NS

	case AF_UNSPEC:
		edst = &((struct ether_header*)dst->sa_data)->ether_dhost[0];
		type = ((struct ether_header *)dst->sa_data)->ether_type;
		break;

	default:
		dri_printf("en%d: cannot handle af%d\n",
			   ifp->if_unit, dst->sa_family);
		error = EAFNOSUPPORT;
		break;
	}


	/* Queue message on interface if possible */
	if (!error) {
		s = splimp();
		error = enput(ei, m0, htons(type), edst, ei->ei_enaddr);
		splx(s);
	}

	/* now send to ourself, if necessary */
	if (mloop) {
		if (!error) {
			error = looutput(&loif, mloop, dst);
			mloop = 0;
		} else {
			m_freem(mloop);
		}
	}

	m_freem(m0);
	return (error);
}


/* Process an ioctl request.
 *	This can be called via the "socket" route for SIOCSIFADDR or
 *	by the cdev/inode route for SIOCSIFCCFWR/RD
 */
static
enioctl(ifp, cmd, data)
register struct ifnet *ifp;
int cmd;
caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	struct en_info *ei = &en_info[ifp->if_unit];
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		switch (ifa->ifa_addr.sa_family) {
#ifdef INET
		case AF_INET:
			enifinit(ifp->if_unit);	/* before arpwhohas */
			((struct arpcom *)ifp)->ac_ipaddr =
				IA_SIN(ifa)->sin_addr;
			arpwhohas((struct arpcom*)ifp,
				  &IA_SIN(ifa)->sin_addr);
			break;
#endif INET
#ifdef NS
#ifdef TODO
		case AF_NS:
		    {
			    register struct ns_addr *ina;
			    ina = &(IA_SNS(ifa)->sns_addr);

			if (ns_nullhost(*ina)) {
				ina->x_host=*(union ns_host *)(ei->ei_enaddr);
			} else {
				register ETHLIST *el;
				el = (ETHLIST *)&addr->en_addr;

				/*
				 * add another interface address to the
				 * list and inform controller
				 */
				zzz;
			}
			enifinit(ifp->if_unit);
			break;
		    }
#endif TODO
#endif NS
		default:
			enifinit(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		if (!(ifp->if_flags & IFF_UP)
		    && (ifp->if_flags & IFF_RUNNING)) {
			enclear(ei);
		} else if ((ifp->if_flags & IFF_UP) &&
			   !(ifp->if_flags & IFF_RUNNING)) {
			enifinit(ifp->if_unit);
		}
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}


/* probe for a single board, and attach if present.
 */
if_enedtinit(edtp)
register struct edt *edtp;
{
	register unsigned int unit;
	register struct en_info *ei;
	register struct ifnet *ifp;
	register struct vme_intrs *intp;
	register int i;

	intp = edtp->e_intr_info;
	if (0 == intp
	    || MAXBD <= (unit = intp->v_unit)
	    || 0 != intp[1].v_vintr) {
		dri_printf("en%d: bad EDT entry\n", unit);
		return;
	}

	ei = &en_info[unit];
	if (0 != ei->ei_io) {
		dri_printf("en%d: duplicate EDT entry\n", unit);
		return;
	}

	ei->ei_mem = (ENMEM)PHYS_TO_K1(edtp->e_base);
	switch (edtp->e_base) {
	case BD1_MEM:		/* XXX kludge--need 2nd # in edt */
		ei->ei_io = (ENIO)PHYS_TO_K1(BD1_IO);
		break;
	case BD2_MEM:
		ei->ei_io = (ENIO)PHYS_TO_K1(BD2_IO);
		break;
	default:
		dri_printf("en%d: bad address in EDT\n", unit);
		return;
	}

	if (badaddr(&ei->ei_io->io_rap, sizeof(ei->ei_io->io_rap))) {
		dri_printf("en%d: missing\n", unit);
		return;
	}

	mbinit();		/* start the mbufs */

	enbdinit(ei);		/* initialize the board */

	for (i = 0;			/* get ethernet address from board, */
	     i < sizeof(ei->ei_addr);	/* 1 byte at a time */
	     i++)
		ei->ei_addr.b[i] = ei->ei_io->io_ad[i].ad_byte;

	if (showconfig)
		dri_printf("en%d: hardware address %s\n",
		           unit, ether_sprintf(&ei->ei_addr));

	if (!bcmp(&ei->ei_addr, etherbroadcastaddr,
		  sizeof(ei->ei_addr))) {
		dri_printf("en%d: bad ethernet address\n", unit);
		enclear(ei);
		return;
	}

	bcopy(&ei->ei_addr, ei->ei_enaddr, sizeof(ei->ei_enaddr));

	ifp = &ei->ei_if;
	ifp->if_unit = unit;
	ifp->if_name = "en";
	ifp->if_mtu = ETHERMTU;
	ifp->if_flags = IFF_BROADCAST;
	ifp->if_init = enifinit;
	ifp->if_output = enoutput;
	ifp->if_ioctl = enioctl;
	ifp->if_watchdog = enwdog;
	if_attach(ifp);
}
