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
#ident	"$Header: if_la_rb3125.c,v 1.1.2.1.1.1.1.2 90/10/16 12:02:49 beacker Exp $"
/*
 * AMD 7990 "Lance" Ethernet Controller Driver
 *
 * Modified for Intrepid by mdove, himel, djl.
 */

#define	SYSVR2			/* system V release 2 */


#define LANCEPROBLEM		/* Print when problems with production LANCE */
				/* requiring re-initialisation */

extern int LANCE_ADDR[];	/* See saio/machaddr.c for definition */
extern int ENETPROM_ADDR[];     /* See saio/machaddr.c for definition */


#include "../tcp-param.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/errno.h"
#include "sys/uio.h"
#include "sys/edt.h"
#include "sys/sysmacros.h"
#include "sys/immu.h"
#include "sys/debug.h"
#include "bsd43/sys/syslog.h"

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

#define	if_addr	if_addrlist->ifa_addr

#include "sys/ladefs.h"
#include "sys/lastruct.h"

/*	LABUFSIZE -- we divide lance private memory into a group of these
 *	buffers.  The size is arbitrarily a power of two to allow for ease
 *	in locating the start of a buffer given an address within the buffer.
 */

#define	LABUFSIZE	2048
#define LABUFMASK	(2048-1)


#define Mvtop(addr)	(ctob(kvtokptbl(addr)->pgm.pg_pfn) | \
			 ((int)addr & POFFMASK))
#define VtoK1(addr)	(PHYS_TO_K1(Mvtop(addr)))

/* get 24 bit address for device */
#define	devaddr(addr)	((long)(addr) & 0x00ffffff)

/* Procedures called from Kernel */
int	laintr();
int	laedtinit(), la_init();

/* External Procedures Called (some indirectly via macros) */
extern	int	spl0(), splx(), timeout();


/* Declarations of Procedures returning non-integers */
struct	mbuf *laget();

/**************************/
/* Structure Declarations */
/**************************/

/* Lance Network Interface (basically struct ifnet but more) */
extern struct	la_softc Ls;

/* Initialization Block */
extern struct	la_ib *La_ibp;

/***********************************************************************
   Note that 1 more descriptor ring entry than required is specified to
   ensure that the descriptor ring entries start on a quad word boundary
   as required by the LANCE (see address calculations later using QUAD)
***********************************************************************/


/* Receive Descriptor Ring */
extern struct	la_dre *La_rxdrep;

/* Transmit Descriptor Ring */
extern struct	la_dre *La_txdrep;

/* Pointers into Lance memory */
caddr_t	La_rcvmem;
caddr_t	La_xmitmem;

/* Globally Accessible (within this module) Variables */

struct eth_low	laeth_low;

extern int showconfig;


struct ladevice	*laprobe();
int set_lance();
unsigned short la_swap();
unsigned char la_get_stat();
unsigned char la_get_hadr();
int la_set_stat();
int la_set_hadr();
caddr_t labuf_rb3125();
int	lagetmem_rb3125();
int	larcopy_rb3125();
int	laTxCopy_rb3125();

/*************/
/* laedtinit */
/*************/

/*
 * Make Lance available by filling in network interface record.  System 
 * will initialize the interface when it is ready to accept packets
 */

laedtinit(edtp)
struct edt *edtp;
{
	register struct eth_low	*elp;
	register struct	ifnet		*ifp;
	register struct	sockaddr_in	*sin;

	XPR1(XPR_ENET,"laattach Entry\n");

	elp = &laeth_low;
	elp->e_low_probe = laprobe;
	elp->e_low_set_lance = set_lance;
	elp->e_low_intr = laintr;
	elp->e_low_swap = la_swap;
	elp->e_low_g_stat = la_get_stat;
	elp->e_low_s_stat = la_set_stat;
	elp->e_low_g_hadr = la_get_hadr;
	elp->e_low_s_hadr = la_set_hadr;
	elp->e_low_bufaddr = labuf_rb3125;
	elp->e_low_rcvcopy = larcopy_rb3125;
	elp->e_low_getmem = lagetmem_rb3125;
	elp->e_low_TxCopy = laTxCopy_rb3125;
	e_low_ptr = elp;

	set_cpu_con(CON_LANCERESET | CON_LANCEMEM,
		CON_LANCERESET | CON_LANCEMEM);
	return(la_edtinit(edtp));
}

struct ladevice	*
laprobe() {
	register struct	ladevice	*addr;

	/* Set up for accessing Lance I/O Ports */
	addr = (struct ladevice *)MACHDEP(LANCE_ADDR);

	/* Check if Lance installed */

	if (badaddr(&addr->csr_rap, sizeof(addr->csr_rap))) {
		return NULL;
	};

	/* turn off lance before initializing mbufs */
	addr->csr_rap = CSR0; wbflush();
	addr->csr_rdp = CSR_STOP; wbflush();

	return addr;
}

/****************/
/* set up lance */
/****************/

set_lance(addr)
struct ladevice *addr;
{
	register struct	la_dre *dre;
	struct la_vtop *vp;
	register int	i;

	/* Initialise Address of Logical Address Filter */
	if (!Ls.laf_set)
		Ls.ls_laf = (unsigned char *)LAFILTER;

	/* Initialise Current Descriptor Ring Entry Addresses & Counts */
	Ls.ls_rxin = Ls.ls_txin = 0;
	Ls.ls_rxout = Ls.ls_txout = 0;

	XPR2(XPR_ENET,"Setting Up Lance CSRs: addr=0x%x\n",(int)addr);
	/* Set up CSR0-3 */
	if(lastop() < 0)
		return(-1);

	addr->csr_rap = CSR1; wbflush();
	addr->csr_rdp = devaddr(La_ibp); wbflush();
	addr->csr_rap = CSR2; wbflush();
	addr->csr_rdp = devaddr(La_ibp) >> WORDSHIFT; wbflush();
	addr->csr_rap = CSR3; wbflush();
 	addr->csr_rdp = BSWP; wbflush();		/* Byte Swap */
	addr->csr_rap = CSR0; wbflush();

	/* Get Station Address */
	getstation();

	if (showconfig) {
		extern short prt_where;
		short old_prt_where = prt_where;
		prt_where = PRW_CONS;
		log(BSD43_LOG_INFO, "set_lance: Ethernet Address: ");
		for (i = 0; i < STATADDRSIZE; i++)
			log(BSD43_LOG_INFO, "%x ", Ls.ls_enaddr[i] & 0xFF);
		log(BSD43_LOG_INFO, "\n"); 
		prt_where = old_prt_where;
	}

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
	for(dre = La_rxdrep, vp = R_vtop;
		dre < &La_rxdrep[1 << MAX_RXDRE]; ++dre, ++vp)
	{
		if(vp->virtual) {
			m_free(vp->virtual);
			vp->virtual = 0;
		}
		dre->dre_stat = 0;
	}
	lafillrcv();
	for(dre = La_txdrep, vp = T_vtop;
		dre < &La_txdrep[1 << MAX_TXDRE]; ++dre, ++vp)
	{
		if(vp->virtual) {
			m_freem(vp->virtual);
			vp->virtual = 0;
		}
		dre->dre_stat = 0;
	}
}


/***************/
/* Reset Lance */
/***************/

lareset(addr)
struct ladevice *addr;
{
	int	rdp, retries;

	XPR1(XPR_ENET, "lareset: entry\n");
	for(retries = 0; retries < 4; ++retries) {
		if(lastop() < 0)
			return(-1);

		/* Set Initialise Bit in CSR0 */
		addr->csr_rdp = CSR_INIT; wbflush();

		/* Wait for Initialization Complete */
		XPR1(XPR_ENET,"Waiting for IDON\n");
		while (((rdp = addr->csr_rdp) & CSR_IDON) != CSR_IDON) {
			DELAY(1);
		}
		addr->csr_rdp = CSR_STRT; wbflush();
		/* Clear IDON bit */
		addr->csr_rdp = CSR_INEA + CSR_IDON; wbflush();
		if ((addr->csr_rdp & (CSR_TXON | CSR_RXON))
		    == (CSR_TXON | CSR_RXON))
			break;
		log(BSD43_LOG_ERR, "lareset: Retry required\n");
	}
	if(retries >= 4) {
		log(BSD43_LOG_ERR, "lareset: All retries failed\n");
		Ls.ls_if.if_flags &= ~IFF_RUNNING;
		return(-1);
	}
	XPR1(XPR_ENET, "lareset: complete\n");
	return(0);
}


/********************************/
/* Ethernet interface Interrupt */
/********************************/

/* If received packet examine packet to determine type.  If can't determine 
   length from type, then have to drop packet.  Otherwise decapsulate packet 
   based on type and pass to type specific higher-level input routine.  */

int
laintr()
{
	register struct	ladevice *addr;
	register short	rdp;
	
	XPR1(XPR_ENET, "Lance Interrupt\n");

	/* Check if any notice of Interrupts to be taken */

	addr = Ls.ls_addr;
	ASSERT(addr);		/* Might fail early in boot, if lance
				 * is left running by standalone startup. */

	/* First check for errors */
	if (((rdp = addr->csr_rdp) & CSR_INTR) != CSR_INTR)
	{
		log(BSD43_LOG_INFO, "***Spurious Interrupt from Lance***\n");
		return;
	}
	lastats.lasInterrupts++;
	while ((rdp & (CSR_INTR | CSR_TXON)) != CSR_TXON) {
		XPR2(XPR_ENET,"Lance RDP=%x\n",rdp);
		if ((rdp & CSR_BABL) != 0) {
			addr->csr_rdp = CSR_BABL; wbflush();
			lastats.lasBabl++;
			log(BSD43_LOG_INFO, "***BABBLE PACKET from Lance***\n");
		}
		if ((rdp & CSR_MERR) != 0) {
			addr->csr_rdp = CSR_MERR; wbflush();
			lastats.lasMerr++;
			log(BSD43_LOG_INFO, "***MEMORY ERROR from Lance***\n");
		}
		if ((rdp & CSR_MISS) != 0)
		{
			XPR1(XPR_ENET, "Lance Missed Packet\n");
			log(BSD43_LOG_ERR, "Lance Missed Packet: pkts %d\n",
					Ls.ls_if.if_ipackets);

			/* Clear the MISS bit */
			addr->csr_rdp = CSR_MISS; wbflush();
			lastats.lasMiss++;
			laread();
			rdp = addr->csr_rdp;
		}
		if ((rdp & CSR_CERR) != 0)
		{
			XPR1(XPR_ENET,"Lance No Heartbeat\n"); 
			if (showconfig)
				log(BSD43_LOG_INFO, "Lance No Heartbeat\n");
			lastats.lasHeartBeat++;
			/* Clear the CERR bit */
			addr->csr_rdp = CSR_CERR; wbflush();
			rdp = addr->csr_rdp;
		}
		if ((rdp & CSR_IDON) == CSR_IDON) {
			/* Clear the IDON bit */
			addr->csr_rdp = CSR_IDON; wbflush();
			rdp = addr->csr_rdp;
		}

		/* Now process expected Interrupts */
		if ((rdp & CSR_TINT) == CSR_TINT)
		{
			/* Clear the TINT bit */
			addr->csr_rdp = CSR_TINT; wbflush();
			laflushxmit(0);
			lastats.lasXmitInts++;
			if(Ls.ls_if.if_snd.ifq_head && (rdp & CSR_TXON))
				lastart();
		}

		if ((rdp & CSR_RINT) == CSR_RINT)
		{
			/* Clear the RINT bit */
			addr->csr_rdp = CSR_RINT; wbflush();
			lastats.lasRcvInts++;
			laread();
		}
		rdp = addr->csr_rdp;
		if (!(rdp & CSR_TXON)) {
			log(BSD43_LOG_NOTICE, "Lance has lost TXON %x\n", rdp);
			lastats.lasLostTxon++;
			reinit();
		}
	}
	addr->csr_rdp = CSR_INEA; wbflush();
}

/*********************************/
/* Get Station Address from PROM */
/*********************************/

static
getstation()
{
	u_char	stataddr[6];
	u_char  *enetp;
	int i;

	XPR1(XPR_ENET, "Getstation entry\n");
	if (!Ls.enaddr_set) {
	        enetp = (u_char *)MACHDEP(ENETPROM_ADDR) + 3;
		for (i=0; i<6 ; i++)
			stataddr[i] = enetp[4*i];
		bcopy((caddr_t)stataddr, (caddr_t)Ls.ls_enaddr, sizeof(stataddr));
	}
	Ls.ls_if.if_physaddrlen = 6;
	bcopy ((caddr_t)Ls.ls_enaddr, (caddr_t)Ls.ls_if.if_physaddr, 6);
	XPR1(XPR_ENET, "Getstation Complete\n");
}

unsigned short
la_get_rdp(addr)
register struct ladevice	*addr;
{
	return addr->csr_rdp;
}


unsigned short
la_swap(x)
unsigned int	x;
{
	return (unsigned short)(x);
}

int
la_set_stat(cp, st)
unsigned char *cp;
unsigned char st;
{
	*cp = st;
	return st;
}

unsigned char
la_get_stat(cp)
unsigned char *cp;
{
	return *cp;
}

int
la_set_hadr(cp, st)
unsigned char *cp;
unsigned char st;
{
	*cp = st;
	return st;
}

unsigned char
la_get_hadr(cp)
unsigned char *cp;
{
	return *cp;
}

lastop()

{
	register struct ladevice *addr;
	int	i;

	addr = Ls.ls_addr;
	addr->csr_rap = CSR0; wbflush();
	addr->csr_rdp = CSR_STOP; wbflush();
	i = 0;
	while(addr->csr_rdp != CSR_STOP) {
		if(++i > 0xf00f00) {
			log(BSD43_LOG_ERR, "lastop:  Can't stop lance");
			return(-1);
		}
	}
	return(0);
}

/*	Locate the hardware buffer address.
 *	buffers are indexed 0 - 127.
 *	The returned address is in K1 kernel space.
 */

caddr_t
labuf_rb3125(index, m, isRcv)

int	index;
struct mbuf *m;
int	isRcv;
{
	if(isRcv) {
		return((caddr_t)&La_rcvmem[index * LABUFSIZE + 2]);
	} else {
		return((caddr_t)&La_xmitmem[index * LABUFSIZE]);
	}
}


/*	larcopy_rb3125 -- copy data from receive memory to mbufs.
 *	The destination of the copy is always an mbuf we've reserved,
 *	so we make assumptions about the alignment.
 */

larcopy_rb3125(m, ind, len)

struct mbuf *m;		/* cluster mbuf describing the data */
int	ind;		/* index of DRE data in rcv ring */
int	len;		/* length of data */
{

	if(len <= ((MLEN-2))) {	/* 2 used to align ifp and IP header */
		mclput(m);
		m->m_off = MMINOFF + 2;
	}
	/* Copy is rounded out to an integral number of words. */
	ASSERT((MLEN & 3) == 0);
	bcopy(R_vtop[ind].physical-2, mtod(m, caddr_t)-2, (len+2+3)&~3);
}


/*	lagetmem_rb3125 -- initialize common memory.
 *	Called at startup time and in response to hardware memory errors.
 *	Note: lamemerr assumes this routine will not touch the chip.
 */

lagetmem_rb3125()

{
	register long *lp;
	register int i;
	register caddr_t avail;

	/* Init memory buffer parity bits */
	for(lp = (long *)PHYS_TO_K1(LANCE_MEM_RB3125);
	    lp < (long *) PHYS_TO_K1(LANCE_MEM_RB3125+LANCE_MEMSIZE_RB3125);
	    ++lp) {
		*lp = 0;
	}
	
	avail = (caddr_t)PHYS_TO_K1(LANCE_MEM_RB3125);
	La_rxdrep = (struct la_dre *)avail;
	avail += (1 << MAX_RXDRE) * sizeof(struct la_dre);
	La_txdrep = (struct la_dre *)avail;
	avail += (1 << MAX_TXDRE) * sizeof(struct la_dre);
	La_ibp = (struct la_ib *)avail;
	avail = (caddr_t) ((int) (avail + LABUFSIZE - 1) & ~LABUFMASK);
	La_rcvmem = avail;
	avail += (1 << MAX_RXDRE) * LABUFSIZE;
	La_xmitmem = avail;
	avail += (1 << MAX_TXDRE) * LABUFSIZE;
	return(1);
}

/*	lamemerr -- lance memory error.
 *	Following a parity error in lance memory, the lance chip is
 *	stopped and this routine is called.
 */

lamemerr()

{
	int	s;

	s = splhi();

	set_cpu_con(CON_LANCERESET | CON_LANCEMEM, 0);
	/* ASSERT: lagetmem will not touch the chip which is held in reset */
	lagetmem_rb3125();
	DELAY(100);
	set_cpu_con(CON_LANCERESET | CON_LANCEMEM,
		CON_LANCERESET | CON_LANCEMEM);
	reinit();
	log(BSD43_LOG_ERR, "Lance Memory Buffer Parity Error.");
	splx(s);
}


/*	laTxCopy_rb3125 -- Copy mbuf to lance memory.
 */

laTxCopy_rb3125(m, ind)

struct mbuf *m;
int	ind;
{
	register caddr_t cp;
	int	off;

	/* Align copies to 4 byte boundary */
	off= mtod(m, int) & 3;
	cp = &La_xmitmem[ind * LABUFSIZE];
	bcopy(mtod(m, caddr_t)-off, cp, (m->m_len+off+3) & ~3);
	return((int)cp+off);
}
