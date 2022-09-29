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
#ident	"$Header: if_lacomm.c,v 1.5.1.8.1.4.1.2 90/10/16 12:03:16 beacker Exp $"
/*
 * AMD 7990 "Lance" Ethernet Controller Driver (combined)
 *
 * Modified for Intrepid by mdove, himel, djl, beacker.
 *
 */

#define	SYSVR2			/* system V release 2 */


#define LANCEPROBLEM		/* Print when problems with production LANCE */
				/* requiring re-initialisation */

extern int LANCE_ADDR[];	/* See saio/machaddr.c for definition */

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

#include "sys/iop.h"

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

#include "../sys/time.h"

extern struct timeval boottime;

struct eth_low *e_low_ptr;


#define Mvtop(addr)	(ctob(kvtokptbl(addr)->pgm.pg_pfn) | \
			 ((int)addr & POFFMASK))
#define VtoK1(addr)	(PHYS_TO_K1(Mvtop(addr)))

#define NON_CACHE	0

/*
 * remove and add high byte address modifier for access to VME 32 space
 * by cpu
 */

/* remove modifier */
#define	devaddr(addr)	((long)(addr) & 0x00ffffff)
/* add modifier */
#define busmemaddr(addr)	(0x07000000 | ((long)(addr) & 0xffffff))

/* Procedures called from Kernel */
int	laoutput(), lawatch();
int	laioctl(), if_laedtinit(), la_init();

/* External Structures Referenced */
extern	struct	ifnet loif;

/* External Procedures Called (some indirectly via macros) */
extern	int	spl0(), splx(), timeout();
extern 	int	if_attach();

extern	struct	in_addr	if_makeaddr();
extern	int	nulldev();
extern	int	if_rtinit(), iocheck(), looutput();
extern	int	blt();

/* Declarations of Procedures returning non-integers */
struct	mbuf *laget();
struct ladevice	*laprobe();

/**************************/
/* Structure Declarations */
/**************************/

/* Lance Network Interface (basically struct ifnet but more) */
struct	la_softc Ls;

/* Initialization Block */
struct	la_ib *La_ibp;

/***********************************************************************
   Note that 1 more descriptor ring entry than required is specified to
   ensure that the descriptor ring entries start on a quad word boundary
   as required by the LANCE (see address calculations later using QUAD)
***********************************************************************/


/* Receive Descriptor Ring */
struct	la_dre *La_rxdrep;

/* Transmit Descriptor Ring */
struct	la_dre *La_txdrep;

/*
 * These structs point to the data area of the mbuf used to store the
 * network data.
 */

struct la_vtop R_vtop[1 << MAX_RXDRE], T_vtop[1 << MAX_TXDRE];

struct	lastats	lastats;

char	lastatstrings[] =
"Receive Framing Errors		%d\n\0\
Receive Silo Overflow Errors	%d\n\0\
Receive CRC Errors		%d\n\0\
Receive Buf Owner Error		%d\n\0\
Transmit Buffer Owner Error	%d\n\0\
Transmit Underflow Error	%d\n\0\
Transmit Late Collision		%d\n\0\
Transmit Carrier Dropped	%d\n\0\
Transmit Retry Exceeded		%d\n\0\
Babble Interrupt		%d\n\0\
No Heart Beat			%d\n\0\
Missed Packet Interrupt		%d\n\0\
Memory Error			%d\n\0\
Interrupts Total		%d\n\0\
Receive Packet Interrupts	%d\n\0\
Transmit Packet Interrupts	%d\n\0\
Lance Lost TXON			%d\n\0\
Unrecovered Underflow Errors	%d\n\0\
";

extern int showconfig;


/*******************/
/* la_edtinit      */
/*******************/

/*
 * make Lance available by filling in network interface record.  System
 * will initialize the interface when it is ready to accept packets
 */

la_edtinit(edtp)
struct edt *edtp;
{
	register struct	ifnet		*ifp;
	register struct	sockaddr_in	*sin;
	register int	level;

	if (!lagetmem())
		return;		/* lagetmem() failed to allocate mem */

	ifp = &Ls.ls_if;
	ifp->if_unit = 0;		/* Only one Lance */

	if ((Ls.ls_addr = laprobe(ifp->if_unit)) == NULL) {
		log(BSD43_LOG_ERR, "laattach: No Lance on this machine!\n");
		return;
	}
	ifp->if_name = "la";
	ifp->if_mtu = ETHERMTU;		/* Maximum Transmission Unit (1500) */

	/* Network Management */

	ifp->if_description = "native lance chip";
	ifp->if_type = 6;		/* ethernet-csmacd */
	ifp->if_speed = 10000000;	/* 10 megabits */
	

	mbinit();			/* Start the mbufs */

	level = splimp();
	LOW_ETHER(0,set_lance)(Ls.ls_addr);

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
	ifp->if_init = la_init;
	ifp->if_output = laoutput;
	ifp->if_ioctl = laioctl;
	ifp->if_watchdog = lawatch;
	ifp->if_mtu = ETHERMTU;
	ifp->if_flags = IFF_BROADCAST;
	splx(level);

	/* Attach Network Interface */
	if_attach(ifp);

	XPR1(XPR_ENET,"laattach: complete\n");
}


/*******************************/
/* Initialization of interface */
/*******************************/

la_init()
{
	register struct	ifnet		*ifp = &Ls.ls_if;
	register int			level;

	XPR1(XPR_ENET, "lainit: entering\n");
	if (ifp->if_addrlist == 0) {	/* address still unknown */
		ifp->if_flags &= ~IFF_RUNNING;
		return;
	}

	if ((Ls.ls_if.if_flags & IFF_RUNNING) == 0) {

		/* Reset Lance */
		level = splimp();
		lareset(Ls.ls_addr);
	
		Ls.ls_if.if_flags |= IFF_RUNNING;

		/* Start any, unlikely, pending output */
		if(Ls.ls_if.if_snd.ifq_head)
			lastart();
		splx(level);
	}
	XPR1(XPR_ENET, "lainit: complete\n");
}


/*******************/
/* Receive Packets */
/*******************/


laread()
{
	unsigned short ether_type;
	register struct	mbuf	*m;
	register struct	ifqueue	*inq;
	extern struct ifqueue	ipintrq;

	
 	XPR1(XPR_ENET,"laread: entering\n");

	for (;;)	/* keeping pulling packets off until we are out */
	{
		m = laget(&ether_type);
		if (!m) {
			lafillrcv();
			return;
		}
		/* Bump number of packets received */
		Ls.ls_if.if_ipackets++;

		/*
		 * By the time we get here, we have a complete mbuf chain 
		 * the la->ether_type is correctly set, and the descriptors 
		 * have been returned to LANCE
		 */

		switch (ether_type) {

		case ETHERTYPE_IP:
			schednetisr(NETISR_IP);
			inq = &ipintrq;
			break;

		case ETHERTYPE_ARP:
		case ETHERTYPE_RARP:

			arpinput(&Ls.ls_ac, m);
			continue;

		case ETHERTYPE_DECRESV0:
		case ETHERTYPE_MOPDL:
		case ETHERTYPE_MOPRC:
		case ETHERTYPE_DECIV:
		case ETHERTYPE_LAT:
		case ETHERTYPE_DECDIAG:
		case ETHERTYPE_DECCUSTOMER:
		case ETHERTYPE_LAVC:
		case ETHERTYPE_DECRESV1:
		case ETHERTYPE_DECRESV2:
			/* If eth_io_main is a dummy it will return a */
			/* non-zero value and we will have to free the */
			/* mbuf ourselves. */
			if (eth_io_main(m)) {
				Ls.ls_if.if_iunknownprotos++;
				m_freem(m);
			}
			continue;

		default:
			Ls.ls_if.if_iunknownprotos++;
			m_freem(m);
			continue;
		}
		if (IF_QFULL(inq))
		{
			IF_DROP(inq);
			if (m_goodm(m))
				m_freem(m);
			else
				log(BSD43_LOG_WARNING, 
				    "laread: attempt to free bad mbuf(4)\n");
			continue;
		}
		IF_ENQUEUE(inq, m);
	}
}


/********************/
/* Transmit Packets */
/********************/

/*
 * Encapsulate a packet of type family for the local net. Use trailer local 
 * net encapsulation if enough data in first packet leaves a multiple of 512 
 * bytes of data in remainder. If destination is this address or broadcast, 
 * send packet to loop device (unless ECHO defined)
 */

laoutput(ifp, m0, dst)
register struct	ifnet		*ifp;
register struct	mbuf		*m0;
register struct	sockaddr	*dst;
{
	register int	type, error;
	u_char		edst[6];
	struct in_addr	idst;
	register struct	mbuf	*m = m0;
	register struct	ether_header	*la;
	register struct	mbuf	*mcopy = (struct mbuf *) 0;	/* Null */
	char		*ptr;
	int		level;
	extern int mb16_drops;
	int usetrailers;

	XPR1(XPR_ENET,"laoutput: entering\n");

	if((ifp->if_flags & (IFF_UP|IFF_RUNNING)) != (IFF_UP|IFF_RUNNING)) {
		error = ENETDOWN;
		goto bad;
	}
	switch (dst->sa_family)
	{
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;

		if (!arpresolve(&Ls.ls_ac, m, &idst, edst, &usetrailers))
			return(NULL);	/* if not yet resolved */

		if (!bcmp(edst, etherbroadcastaddr,sizeof(edst))) {
			mcopy = m_copy(m, 0, M_COPYALL);
			if (!mcopy) {
				error = ENOBUFS;
				goto bad;
			}
		}
/*
		if (in_lnaof(idst) == INADDR_ANY)
			mcopy = m_copy(m, 0, (int)M_COPYALL);
 */
		else if (idst.s_addr == ((struct sockaddr_in *)
				&Ls.ls_if.if_addr)->sin_addr.s_addr)
		{
			mcopy = m;
			XPR1(XPR_ENET,"goto gotlocal\n");
			goto gotlocal;
		}

		type = ETHERTYPE_IP;
                XPR1(XPR_ENET,"goto gottype\n");
		goto gottype;

	case AF_UNSPEC:
		XPR1(XPR_ENET,"AF_UNSPEC\n");
		la = (struct ether_header *)dst->sa_data;
		bcopy((caddr_t)la->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = la->ether_type;
		goto gottype;

	default:
		log(BSD43_LOG_ERR, "la%d: can't handle af%d\n", ifp->if_unit,
		    dst->sa_family);
		error = EAFNOSUPPORT;
		goto bad;
	}

gottype:
	/*
         * Add local net header.  If no space in first mbuf, allocate 
         * another.
         */
	if (m->m_off > MMAXOFF || MMINOFF +
		sizeof (struct ether_header) > m->m_off)
	{
		MGET(m, M_DONTWAIT, MT_DATA);

		if (m == 0)
		{
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct ether_header);
	} 
	else
	{
		m->m_off -= sizeof (struct ether_header);
		m->m_len += sizeof (struct ether_header);
	}

	la = mtod(m, struct ether_header *);
	bcopy((caddr_t)edst, (caddr_t)la->ether_dhost, sizeof(edst));
	if (edst[0] & 0x1)
		Ls.ls_if.if_onucastpackets++;
	la->ether_type = htons((u_short)type);
	bcopy((caddr_t)Ls.ls_enaddr, (caddr_t)la->ether_shost, 6);

	if (m->m_next)
		m = (struct mbuf *)m_fillup(m, 100, NON_CACHE);
	if (!m) {
		log(BSD43_LOG_WARNING, "m_pullup failed!\n");
		m0 = m;
		goto bad;
	}

	/* Do a check for legal mbufs on m/120 or m20*/
	if (IS_R2400 || IS_R3030) {
		struct mbuf *mlast = NULL;

		m0 = m;
		while (m != NULL) {
			struct mbuf *m1;
			char *m1ptr;
			int ret;

			ptr = mtod(m, char *);
			if (((int)Mvtop(ptr) & 0x1fffffff) > 0x00ffffff) {
				mb16_drops++;
				/* since just dropping the packet is not nice,
				   copy it into a good buffer */
again:
				MGET(m1, M_DONTWAIT, MT_DATA);
				if (m1 == NULL) {
					log(BSD43_LOG_ERR, "laoutput: MGET failed\n");
					goto bad;
				}
				ret = mclget(m1);
				if (ret == 0) {
					log(BSD43_LOG_ERR, "laoutput: mclget failed\n");
					goto bad;
				}
				m1ptr = mtod(m1, char *);
				if (((int)Mvtop(m1ptr) & 0x1fffffff)>0x00ffffff){
					/* m_free will really free a bad one */
					m_free(m1);
					goto again;
				}
				/* we know that each mbuf is MTU size or less */
				bcopy(ptr, mtod(m1, char *), m->m_len);
				m1->m_len = m->m_len;
				m1->m_next = m->m_next;
				m_free(m);
				m = m1;
				if (mlast != NULL)
					mlast->m_next = m;
				else
					m0 = m;
			}
			mlast = m;
			m = m->m_next;
		}
		m = m0;
	}

/* Queue message on interface and start output */

	level = splimp();
	if (IF_QFULL(&ifp->if_snd))
	{
		IF_DROP(&ifp->if_snd);
		Ls.ls_if.if_odiscards++;
		error = ENOBUFS;
		lastart();
		m0 = m;
		splx(level);
		goto bad;
	}
	IF_ENQUEUE(&ifp->if_snd, m);

	lastart();
	splx(level);

gotlocal:
        XPR1(XPR_ENET,"laoutput: complete (ok)\n");
	return(mcopy ? looutput(&loif, mcopy, dst) : 0);

bad:
	if(m0)
		m_freem(m0);
	if(mcopy)
		m_freem(mcopy);
	return(error);
}


/*************************/
/* Start output on Lance */
/*************************/

/* lastart -- start transmitter activity.
 * 
 * Caller must insure interrupts are disabled.
 */

lastart()
{

	XPR1(XPR_ENET,"lastart: entering\n");
	laput();
	XPR1(XPR_ENET, "lastart: complete\n");
}


/*	lafillrcv -- fill the receive ring.
 *	Obtain mbufs from the system, and install these buffers
 *	in the receive ring.
 */

lafillrcv()

{
	register struct la_vtop *vp;
	register struct mbuf *m;
	register struct	la_dre *dre;
	
	while(1) {
		vp = &R_vtop[Ls.ls_rxin];
		if(vp->virtual) {
			return;		/* All done */
		}
		MGET(m, M_DONTWAIT, MT_DATA);
		if(m == NULL) {
			return;
		}
		if(!mclget(m)) {
			m_free(m);
			return;
		}
		m->m_off += 2;	/* +2 aligns packet header */
		m->m_len -= 2;
		vp->virtual = (int)m;
		vp->physical = (int)LOW_ETHER(0,bufaddr)(Ls.ls_rxin, m, 1);
		if(vp->physical == 0) {
			log(BSD43_LOG_ERR,
			"lance: mbuf outside 24 bit address range\n");
			m_free(m);
			vp->virtual = 0;
			return;
		}
		dre = &La_rxdrep[Ls.ls_rxin];
		dre->dre_ladr = LA_SWAP(devaddr(vp->physical));
		SET_HADR(dre->dre_hadr, devaddr(vp->physical) >> WORDSHIFT);
		SET_STAT(dre->dre_stat, S_OWN);
		dre->dre_bcnt = LA_SWAP(-m->m_len);
		dre->dre_mcnt = 0;
		if(++Ls.ls_rxin >= (1 << MAX_RXDRE)) {
			Ls.ls_rxin = 0;
		}
	}
}


/* laflushxmit -- flush xmit ring.
 *      Following a transmit interrupt analyze errors, and free mbufs.
 */

laflushxmit(saveuflo)

int	saveuflo;		/* If true, do not free an mbuf with an
				 * underflow error */
{
	register struct la_vtop *vp;
	register struct	la_dre *dre;
	register unsigned stat;
	int	ebits;

	while(Ls.ls_txout != Ls.ls_txin) {
		vp = &T_vtop[Ls.ls_txout];
		dre = &La_txdrep[Ls.ls_txout];
		if((stat = GET_STAT(dre->dre_stat)) & S_OWN)
			return;
		if(stat & S_ERR) {
			Ls.ls_if.if_oerrors++;
			ebits = LA_SWAP(dre->dre_mcnt) >> 8;
			if(ebits & S_RTRY)
				lastats.lasXmitRtry++;
			if(ebits & S_LCAR)
				lastats.lasXmitLcar++;
			if(ebits & S_LCOL)
				lastats.lasXmitLate++;
			if(ebits & S_UFLO) {
				lastats.lasXmitUflo++;
				if(saveuflo)
					return;
				lastats.lasUfloDrop++;
			}
			if(ebits & S_TBUFF)
				lastats.lasXmitBuf++;
		} else if(stat & S_MORE) {
			Ls.ls_if.if_collisions += 2;
		} else if(stat & S_ONE) {
			Ls.ls_if.if_collisions++;
		}
		if (stat & S_ENP)
			Ls.ls_if.if_opackets++;
		if(vp->virtual) {
			m_freem(vp->virtual);
			vp->virtual = 0;
		}
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
	register struct	la_dre *dre;
	struct ether_header *la;
	int len, off, resid, ind;
	register stat;
	int physaddr;
	
again:
	dre = &La_rxdrep[Ls.ls_rxout];
	if(((stat = GET_STAT(dre->dre_stat)) & S_OWN) == S_OWN) {
		return(NULL);
	}
	if ((m = (struct mbuf *)R_vtop[Ls.ls_rxout].virtual) == NULL) {
		if(Ls.ls_rxin != Ls.ls_rxout) {
			log(BSD43_LOG_NOTICE,
			    "laget: possible bug with null mbuf\n");
		}
		return(NULL);
	}
	if (stat != (S_ENP | S_STP)) {
		/* Trouble, correct packet not contained in one mbuf */
		Ls.ls_if.if_ierrors++;
		if(stat & S_ERR) {
			if ((stat & (S_FRAM | S_ENP | S_OFLO))
			   == (S_FRAM | S_ENP)) 
				lastats.lasRcvFram++;
			if ((stat & (S_OFLO | S_ENP)) == S_OFLO)
				lastats.lasRcvOflo++;
			if ((stat & (S_CRC | S_ENP | S_OFLO))
			   == (S_CRC | S_ENP))
				lastats.lasRcvCRC++;
			if (stat & S_BUFF)
				lastats.lasRcvBuf++;
			
		} else if((stat & (S_ENP | S_STP)) != (S_ENP | S_STP)) {
			log(BSD43_LOG_ERR, 
				"Lance packet > buffer size, missing ENP\n");
		}
		do {
			R_vtop[Ls.ls_rxout].virtual = 0;
			m_free(m);
			if(++Ls.ls_rxout >= 1 << MAX_RXDRE)
				Ls.ls_rxout = 0;
			dre = &La_rxdrep[Ls.ls_rxout];
			stat = GET_STAT(dre->dre_stat);
			m = (struct mbuf *) R_vtop[Ls.ls_rxout].virtual;
		} while((stat & (S_OWN | S_STP)) == 0 && m);
		goto again;
	}
		
	physaddr = R_vtop[Ls.ls_rxout].physical;
	R_vtop[Ls.ls_rxout].virtual = 0;
	ind = Ls.ls_rxout;
	if(++Ls.ls_rxout >= (1 << MAX_RXDRE))
		Ls.ls_rxout = 0;

	/* Validate the buffer address from the descriptor */ 

	if (devaddr(physaddr) != 
	   ((GET_HADR(dre->dre_hadr)<<WORDSHIFT) + LA_SWAP(dre->dre_ladr)) ) {
		log(BSD43_LOG_ERR, 
		    "Lance punting on corrupted descriptor %x %x %x\n",
		       physaddr, GET_HADR(dre->dre_hadr),
		       LA_SWAP(dre->dre_ladr));
		m_free(m);
		goto again;
	}
	len = LA_SWAP(dre->dre_mcnt) -
		sizeof (struct ether_header) - 4;	/* 4 is sizeof CRC */
	if (len < ETHERMIN || len > ETHERMTU) {
		log(BSD43_LOG_ERR, 
		    "Lance punting on dre 0x%x, bogus input data length 0x%x\n",
		       (dre - La_rxdrep), len);
		m_free(m);
		goto again;
	}
	m->m_len = len;
	m->m_next = NULL;

	LOW_ETHER(0,rcvcopy)(m, ind, len + sizeof(struct ether_header));

	la = mtod(m, struct ether_header *);
	m->m_off += sizeof(struct ether_header);
	if (la->ether_dhost[0] & 0x1)
		Ls.ls_if.if_inucastpackets++;
	*ether_type = ntohs(la->ether_type);

	/* Deal with trailer protocol: if type is trailer get true 
	   type from first 16-bit word past data.
	*/
	if (*ether_type >= ETHERTYPE_TRAIL && *ether_type < 
			ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER)
	{
		MGET(m0, M_DONTWAIT, MT_DATA);
		if (m0 == NULL) {
			log(BSD43_LOG_ERR, "laget: ifp mget failed!\n");
			m_free(m);
			goto again;
		}
		*(mtod(m0, struct ifnet **)) = &Ls.ls_if;
		m0->m_next = m;
		m0->m_len = sizeof(struct ifnet **);

		/* got a trailer */
		off = (*ether_type - ETHERTYPE_TRAIL) * 512; /* len of data */

		*ether_type = ntohs(*(ushort *)(mtod(m, int) + off));
		resid =	ntohs(*(ushort *)(mtod(m, int) + off +
						(int)sizeof(ushort)));
		if (off + resid > len) {
			/* Release Descriptor Ring Entry */
			log(BSD43_LOG_ERR, "ERROR - off+resid > len\n");
			m_freem(m0);
			goto again;
		}
		if(resid > MLEN - sizeof(struct ifnet **)) {
			log(BSD43_LOG_NOTICE,
				"laget: trailing header bigger than mbuf\n");
			m_freem(m0);
			Ls.ls_if.if_idiscards++;
			goto again;
		}

		bcopy((char *)(mtod(m, int) + off + 2*sizeof(ushort)), 
			mtod(m0, char *) + sizeof(struct ifnet **),
			(resid - 2*sizeof(ushort)));

		m->m_len -= resid;
		m0->m_len += resid - 2 * sizeof(ushort);
	} else {
		m->m_len += 4;
		m->m_off -= 4;	/* This space reserved by lafillrcv() */
		*(mtod(m, struct ifnet **)) = &Ls.ls_if;
		m0 = m;
	}
	Ls.ls_if.if_ioctets += len;
	return(m0);
} /* laget() */


/************/
/* Watchdog */
/************/

int	lawatch()
{
	XPR1(XPR_ENET, "lawatch entry\n");
	/* Do nothing */
}


/**********************/
/* Reinitialise Lance */
/**********************/

reinit()
{
	register struct ladevice *addr;
	register int	level;

	XPR1(XPR_ENET, "reinit: entry\n");
	level = splimp();

	/* Set up for accessing Lance I/O Ports */

	addr = Ls.ls_addr;

	LOW_ETHER(0,set_lance)(addr);	/* set up lance again */
	lareset(addr);		/* set STRT bit in lance */
	if ((Ls.ls_if.if_flags & IFF_RUNNING) && Ls.ls_if.if_snd.ifq_head)
		lastart();
	
	splx(level);
	XPR1(XPR_ENET, "reinit: complete\n");
}


lagetmem()
{
	long	needed;
	char	*mem;

	if(LOW_ETHER(0,getmem)) {
		return(LOW_ETHER(0,getmem)());
	}
	/* allocate memory for structures accessible by lance */
	/* compute needed amount */
	needed = 7;
	needed += sizeof (struct la_dre) * (1 << MAX_RXDRE);
	needed += sizeof (struct la_dre) * (1 << MAX_TXDRE);
	needed += sizeof *La_ibp;

	mem = kern_malloc(needed);		/* Hmmm */

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
	La_ibp = (struct la_ib *)VtoK1(mem);
	XPR3(XPR_ENET,"La_rxdrep 0x%x La_txdrep 0x%x\n",La_rxdrep,La_txdrep);
	XPR2(XPR_ENET,"La_ibp 0x%x\n",La_ibp);
	return(1);
}


/*
 * laput()
 *
 * Given a pointer into the transmit ring, and the head of an mbuf chain,
 * enqueue to data to the LANCE, and set STP and ENP correctly.
 *
 * Caller of this routine must insure interrupts are off.
 */
laput()

{
	register struct mbuf *m, *n, *firstmb;
	register struct la_vtop *vp;
	register struct la_dre *fdre, *dre;
	int	(*txCopy)();
	int totlen, bcnt;
	int avail, physaddr;

	if (Ls.ls_txin == Ls.ls_txout)
		avail = (1 << MAX_TXDRE) - 1;
	else if(Ls.ls_txin < Ls.ls_txout)	/* in wraps before out */
		avail = Ls.ls_txout - Ls.ls_txin - 1;
	else
		avail = (1 << MAX_TXDRE) - (Ls.ls_txin - Ls.ls_txout) - 1;
	firstmb = 0;
	txCopy = LOW_ETHER(0,TxCopy);
	IF_DEQUEUE(&Ls.ls_if.if_snd, m);
	while(m) {
		n = m;
		--avail;
		while(n->m_next) {
			n = n->m_next;
			--avail;
		}
		if (avail <= 0) {	/* No room on xmit ring */
			IF_PREPEND(&Ls.ls_if.if_snd, m);
			break;
		}
		fdre = &La_txdrep[Ls.ls_txin];	/* first dre */
		dre = 0;
		totlen = 0;
		if(!txCopy) {
			firstmb = m;
		}
		while(m != NULL) {
			dre = &La_txdrep[Ls.ls_txin];
			if(GET_STAT(dre->dre_stat) & S_OWN) {
				log(BSD43_LOG_ERR,
					"laput: Fatal Lance software error\n");
bailout:
				lastop();
				Ls.ls_if.if_flags &= ~IFF_RUNNING;
				return;
			}
			vp = &T_vtop[Ls.ls_txin];
			if(txCopy) {
				physaddr = txCopy(m, Ls.ls_txin);
			} else {
				vp->virtual = 0;
				physaddr = Mvtop(mtod(m, char *));
				if ((physaddr & 0x1fffffff) > 0x00ffffff 
				     && !IS_RB3125) {
					log(BSD43_LOG_ERR,
						"laput: Fatal Lance software error addr > 16 Meg\n");
					goto bailout;
				}
			}
			dre->dre_ladr = LA_SWAP(devaddr(physaddr));
			SET_HADR(dre->dre_hadr, devaddr(physaddr) >> WORDSHIFT);
			dre->dre_mcnt = 0;
			totlen += m->m_len;
			bcnt = m->m_len;
			dre->dre_bcnt = LA_SWAP(-bcnt);
			if(dre != fdre) {
				if(m->m_next)
					SET_STAT(dre->dre_stat, S_OWN);
				else
					SET_STAT(dre->dre_stat, S_OWN | S_ENP);
			}
			if (++Ls.ls_txin >= (1 << MAX_TXDRE))
				Ls.ls_txin = 0;
			if(txCopy) {
				/* If we copy the contents, no need to save
				 * the mbuf.
				 */
				MFREE(m, n);
				m = n;
			} else {
				m = m->m_next;
			}
		}
		/* If mbufs are kept, last Ring location of the packet
		 * contains pointer to chain of mbufs.
		 */
		vp->virtual = (int) firstmb;
		if(totlen < MINPKTSIZE) {
			bcnt += MINPKTSIZE - totlen;
			totlen = MINPKTSIZE;
			dre->dre_bcnt = LA_SWAP(-bcnt);
		} else if(totlen & 1) {
			++totlen;
			++bcnt;
			dre->dre_bcnt = LA_SWAP(-bcnt);
		}
		Ls.ls_if.if_ooctets += totlen;
		if(dre == fdre)  {
			SET_STAT(fdre->dre_stat, S_OWN | S_STP | S_ENP);
		} else {
			SET_STAT(fdre->dre_stat, S_OWN | S_STP);
		}
		IF_DEQUEUE(&Ls.ls_if.if_snd, m);
	}
	return;
} /* laput() */


/*	larcvcopy -- default mbuf copy routine.
 *	The default routine does not have to copy any data.
 */

larcvcopy(m, ind, len)

struct mbuf *m;
int	ind;
int	len;

{

	clean_dcache(PHYS_TO_K0(Mvtop(mtod(m, int))), len);
}


/*	laioctl -- process ioctl from higher layers.
 */

laioctl(ifp, cmd, data)

register struct ifnet *ifp;
int cmd;
caddr_t data;
{
	static void makestring();
	register struct	ladevice *addr;
	register struct ifreq *ifr = (struct ifreq *)data; /* XXX */
	register struct ifaddr *ifa = (struct ifaddr *)data;
	int s = splimp(), error = 0;
	int	i;
	char buffer[SIOCTLRSIZE];

	switch (cmd) {

	case SIOCSIFADDR:
		if (!(ifp->if_flags & IFF_UP)) {
			ifp->if_lastchange =
				(time.tv_sec - boottime.tv_sec) * 100 + 
				(time.tv_usec - boottime.tv_usec) / 10000;
		}
		ifp->if_flags |= IFF_UP | IFF_NOTRAILERS;
		switch (ifa->ifa_addr.sa_family) {

			case AF_INET:
				la_init();
				((struct arpcom *)ifp)->ac_ipaddr =
					IA_SIN(ifa)->sin_addr;
				arpwhohas((struct arpcpm *)ifp,
					&IA_SIN(ifa)->sin_addr);
				break;
			case AF_UNSPEC:
				Ls.enaddr_set = 1;
				bcopy((caddr_t)ifa->ifa_addr.sa_data, (caddr_t)Ls.ls_enaddr, sizeof(Ls.ls_enaddr));
				if (ifp->if_flags & IFF_RUNNING) {
					reinit();
				} else {
					LOW_ETHER(0,set_lance)(Ls.ls_addr);
					la_init();
				}
				break;
			default:
				log(BSD43_LOG_ERR,
				    "laioctl: bad family type\n");
				error = EINVAL;
		}
		break;

	case SIOCSIFFLAGS:
		addr = Ls.ls_addr;
		ifp->if_flags |= IFF_NOTRAILERS;
		if (!(ifp->if_flags & IFF_UP)
		    && (ifp->if_flags & IFF_RUNNING)) {
			LOW_ETHER(0,set_lance)(addr);	/* set up lance again */
			lastop();
			log(BSD43_LOG_INFO, "laioctl: SIOCIFF reset\n");
			Ls.ls_if.if_flags &= ~IFF_RUNNING;
		} else if ((ifp->if_flags & IFF_UP) &&
			   !(ifp->if_flags & IFF_RUNNING)) {
			LOW_ETHER(0,set_lance)(addr);
			log(BSD43_LOG_INFO, "laioctl: SIOCIFF lainit\n");
			la_init();
		}
		break;


        case SIOCSLAF:
		Ls.laf_set = 1;
		bcopy (ifr->ifr_data, (caddr_t)Ls.ls_laf, 8);
		reinit();
		break;

        case SIOCGLAF:
		bcopy ((caddr_t)Ls.ls_laf, ifr->ifr_data, 8);
		break;

        case SIOCGCTLR:
		makestring(ifp,buffer);
		copyout((caddr_t)buffer, ifr->ifr_data, sizeof(buffer));
		break;

#ifdef	SIOCGSTATS
	case SIOCGSTATS: {
		register struct ifreqstats *ifstats;

		ASSERT(sizeof(struct ifreqstats) <= sizeof(struct ifreq));
		ifstats = (struct ifreqstats *)ifr;
		if(ifstats->ifs_statstruct) {
			i = MIN(ifstats->ifs_structlen, sizeof(struct lastats));
			copyout((caddr_t) &lastats,
				(caddr_t)ifstats->ifs_statstruct, i);
			ifstats->ifs_structlen = i;
		}
		if(ifstats->ifs_statnames) {
			i = MIN(ifstats->ifs_namelen, sizeof(lastatstrings));
			copyout((caddr_t) lastatstrings,
				(caddr_t)ifstats->ifs_statnames, i);
			ifstats->ifs_namelen = i;
		}
		break;
	}
#endif
		
	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}


/*	larecoverxmit -- recover transmit ring.
 *	After the lance has stopped.  The transmit ring is examined, and
 *	any packets remaining on the ring are placed back in the trasmit
 *	queue.  Note that rather than drop packets, the transmit queue
 *	is filled without regard to ifq_maxlen.
 */

larecoverxmit()

{
	register struct la_vtop *vp, *evp;

	evp = vp = &T_vtop[Ls.ls_txin];
	do {
		if (vp->virtual) {
			IF_PREPEND(&Ls.ls_if.if_snd,
					(struct mbuf *)vp->virtual)
			vp->virtual = 0;
		}
		if (--vp < T_vtop)
			vp = &T_vtop[(1 << MAX_TXDRE) - 1];
	} while(vp != evp);
}


static void
makestring(ifp,buf)
register struct ifnet *ifp;
char *buf;
{
	strcpy( buf, ifp->if_name);
	btoa( ifp->if_unit, &buf[strlen(buf)], 16);
	strcat( buf, " - ");
	strcat( buf, ifp->if_description);
	strcat( buf, " - ");
	btoa( Ls.ls_enaddr[0], &buf[strlen(buf)], 16);
	strcat( buf, ":");
	btoa( Ls.ls_enaddr[1], &buf[strlen(buf)], 16);
	strcat( buf, ":");
	btoa( Ls.ls_enaddr[2], &buf[strlen(buf)], 16);
	strcat( buf, ":");
	btoa( Ls.ls_enaddr[3], &buf[strlen(buf)], 16);
	strcat( buf, ":");
	btoa( Ls.ls_enaddr[4], &buf[strlen(buf)], 16);
	strcat( buf, ":");
	btoa( Ls.ls_enaddr[5], &buf[strlen(buf)], 16);
}

static
btoa(n, str, base)
        u_char n;
        char *str;
{
        char prbuf[11];
        register char *cp;

        cp = prbuf;
	if( base == 16 ){
            do {
                *cp++ = "0123456789ABCDEF"[n%16];
                n >>= 4;
            } while (n);
	
	} else{
            do {
                *cp++ = "0123456789"[n%base];
                n /= base;
            } while (n);
	}

        do {
                *str++ = *--cp;
        } while (cp > prbuf);
	*str = NULL;
        return;
}
