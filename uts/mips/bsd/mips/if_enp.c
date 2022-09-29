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
#ident	"$Header: if_enp.c,v 1.28.1.9 90/05/10 04:22:10 wje Exp $"

/* 'Driver' CMC ENP-10 driver
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
#include "sys/vmereg.h"

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

/*
#include "bsd43/sys/types.h"
*/
#include "bsd43/sys/ioctl.h"
#include "bsd43/sys/socket.h"
#include "bsd43/mipsif/if_enp.h"

#include "../sys/time.h"

void (*enet_attach_hook)();
void (*enet_detach_hook)();
int (*enet_filter_hook)();
int (*enet_filter_all_hook)();

extern struct timeval boottime;

extern  struct ifnet loif;



#ifndef IF_ENP_MAXBD
#define IF_ENP_MAXBD 2				/* allow 2 boards */
#endif IF_ENP_MAXBD

#define DOG 2				/* check this often after alive */
#define R_DOG 10			/* seconds reset/restart may take */
#define R_CLK 4				/* check reset/restart this often */


#define CRC_LEN 4			/* bytes/ethernet CRC */

/* maximum & minimum packet sizes */
#define MAX_TPKT 1514			/* transmit len--7990 adds cksum */
#define MIN_TPKT 60
#define MAX_RPKT (MAX_TPKT+CRC_LEN)	/* receive length */
#define MIN_RPKT (MIN_TPKT+CRC_LEN)

#define TRAILLEN 512			/* the magic 512 for trailers */


/*
 * Definitions to force 16 bit references
 */
typedef volatile struct {
	u_short hi;
	u_short lo;
} BD_INT;
#define	STO_BD_INT(i, val) { \
	register BD_INT *sto_tmpp = &(i); \
	register u_int sto_tmp = (u_int)(val); \
	sto_tmpp->hi = sto_tmp>>16; wbflush(); \
	sto_tmpp->lo = sto_tmp; wbflush(); \
}

#define	FET_BD_INT(type, dest, i) { \
	register BD_INT *fet_tmp = &(i); \
	(dest) = (type)((fet_tmp->hi << 16) | fet_tmp->lo); \
}


#define EADDR_LEN 6			/* you cannot change this */
typedef struct {
	u_char	b[EADDR_LEN];
} ETHER_ADDR;

typedef struct {
	BD_INT	e_listsize;		/* active addr entries */
	ETHER_ADDR e_baseaddr;		/* addr lance is working with */
	ETHER_ADDR e_addrs[16];		/* possible addresses */
} ETHLIST;


#define RING_SIZE 32			/* entries/ring */
typedef volatile struct {
	short	r_rdidx;
	short	r_wrtidx;
	short	r_size;
	short	r_pad;
	BD_INT	r_slot[RING_SIZE];
} RING;


#define RAM_SIZE (123*1024+512)		/* size of visible RAM */

/* ENP Ram data layout
 */
typedef volatile struct {
	char	enp_ram_rom[4*1024];	/* either RAM or ROM */
	union {
		char	enp_ram[RAM_SIZE];	/* RAM */
		struct {
			unsigned short reg_go;	/* 0x8080 to start board */
			unsigned short reg_csr;	/* board control/status reg */
			BD_INT	reg_pstart;
		} regs;
		struct {			/* link-level structures */
			char	pad1[0x100];
			u_short	e_enpstate;	/* =S_ENPRUN after init */
			short	e_enpmode;
			BD_INT	e_enpbase;	/* =&enp_u.enp_ram[0] */
			short	e_enprun;	/* 'I am alive' counter */
			short	e_intvector;	/* alternate interrupt */

			RING	h_toenp;
			RING	h_hostfree;
			RING	e_tohost;
			RING	e_enpfree;

			BD_INT e_xmit_ok;	/* Successful xmits */
			BD_INT e_mult_retry;	/* multiple retries on xmit */
			BD_INT e_one_retry;	/* single retries */
			BD_INT e_fail_retry;	/* too many retries */
			BD_INT e_deferrals;	/* xmit delayed--cable busy */
			BD_INT e_xmit_buff_err;	/* impossible */
			BD_INT e_silo_underrun;	/* transmit underrun */
			BD_INT e_late_coll;	/* collision after xmit */
			BD_INT e_lost_carrier;
			BD_INT e_babble;	/* xmit length > 1518 */
			BD_INT e_no_heartbeat;	/* transceiver mismatch */
			BD_INT e_xmit_mem_err;
			BD_INT e_rcv_ok;	/* good receptions */
			BD_INT e_rcv_missed;	/* no recv buff available */
			BD_INT e_crc_err;	/* checksum failed */
			BD_INT e_frame_err;
			BD_INT e_rcv_buff_err;	/* impossible */
			BD_INT e_silo_overrun;	/* receive overrun */
			BD_INT e_rcv_mem_err;

			ETHLIST	e_netaddr;
		} iface;
	} enp_u;

	char	pad2[255];
	char	enp_interrupt;		/* R or W interrupts ENP */
	char	pad3[255];
	char	enp_reset;		/* R or W resets ENP */
} ENPDEVICE;

#define enp_go		enp_u.regs.reg_go
#define enp_state	enp_u.iface.e_enpstate
#define enp_mode	enp_u.iface.e_enpmode
#define enp_base	enp_u.iface.e_enpbase
#define enp_toenp	enp_u.iface.h_toenp
#define enp_hostfree	enp_u.iface.h_hostfree
#define enp_tohost	enp_u.iface.e_tohost
#define enp_enpfree	enp_u.iface.e_enpfree
#define enp_addr	enp_u.iface.e_netaddr


/* enp_u.regs.reg_csr bits
 */
#define	ENPCSR_ONLINE	0x2		/* entered application firmware */
#define	ENPCSR_RDY	0x4		/* reset complete */
#define	ENPCSR_OVPROM	0x8		/* don't transfer to prom on reset */
#define	ENPCSR_IE	0x40		/* interrupt enable for bus debug */
#define	ENPCSR_ERR	0x8000		/* enp detected error */

/* state bits in enp_state
 */
#define S_ENPRESET	01		/* enp is in reset state */
#define S_ENPRUN	02		/* enp is in run state */

/*
 * mode bits in enp_mode
 */
#define E_SWAP16	0x1		/* swap two octets within 16 */
#define E_SWAP32	0x2		/* swap 16s within 32 */
#define E_SWAPRD	0x4		/* swap on read */
#define E_SWAPWRT	0x8		/* swap on write */
#define E_DMA		0x10		/* enp does data moving */

#define E_EXAM_LIST	0x8000		/* enp should examine addr list */


/* The ENP Data Buffer Structure
 */
typedef volatile struct bcb {
	struct bcb *b_link;
	short	b_stat;
	short	b_len;
	BD_INT	b_addr;
	short	b_msglen;
	short	b_reserved;
} BCB;

/* BCB b_stat field defines
 */
#define	BCB_DONE	0x8000		/* buffer complete */
#define	BCB_ERR		0x4000		/* error summary */
#define	BCB_FRAME	0x2000		/* framing error */
#define	BCB_OFLO	0x1000		/* silo overflow */
#define	BCB_CRC		0x0800		/* crc error */
#define	BCB_RXBUF	0x0400		/* rx buffer err */
#define	BCB_STP		0x0200		/* start of packet */
#define	BCB_ENP		0x0100		/* end of packet */
#define	BCB_MEMERR	0x0002		/* memory error */
#define	BCB_MISSED	0x0001		/* missed packet */


/* Ethernet info per interface.
 */
struct  enp_info {
	struct  arpcom ei_ac;           /* common ethernet structures */
	u_char enaddr_set;		/* Has the ethernet address be set by an ioctl */
	ETHER_ADDR ei_boardaddr;	/* board ethernet address */
	int	ei_lostintrs;		/* count lost interrupts */

	u_char	ei_vec;			/* vme interrupt vector */

	u_char	ei_sick;		/* 2=reset board on next watchdog */
	u_char	ei_rdidx;		/* recent output ring index */

	u_char	ei_state;		/* reset state */
#define EI_IDLE 0			/*	unknown */
#define EI_START1 1			/*	reseting in order to start */
#define EI_START2 2			/*	starting */
#define EI_OK 3				/*	started */
#define EI_SICK 99			/*	sick board */
	u_char	ei_timer;		/* reset and start timer */
	int	ei_timer_id;

	struct {			/* error statistics */
		int e_mult_retry;
		int e_one_retry;
		int e_fail_retry;
		int e_deferrals;
		int e_no_heartbeat;
		int e_rcv_missed;
		int e_crc_err;
		int e_frame_err;
	} ei_stat;

	ENPDEVICE *ei_addr;		/* physical address */
} enp_info[IF_ENP_MAXBD];

#define ei_if           ei_ac.ac_if     /* network-visible interface */
#define ei_enaddr       ei_ac.ac_enaddr /* hardware ethernet address */


/* handle ENP-10 LINK-10 'rings'
 */
#define ringempty(rp) ((rp)->r_rdidx == (rp)->r_wrtidx)

#define ringfull(rp) ((((rp)->rwrtidx+1) & ((rp)->r_size-1)) == (rp)->r_rdidx)

static
ringput(rp,bcbp)
register RING *rp;
register BCB *bcbp;
{
	register int idx;

	idx = (rp->r_wrtidx + 1) & (rp->r_size-1);
	if (idx != rp->r_rdidx) {
		STO_BD_INT(rp->r_slot[rp->r_wrtidx], bcbp);
		rp->r_wrtidx = idx;
		wbflush();
		if( (idx -= rp->r_rdidx) < 0 )
			idx += rp->r_size;
		return (idx);			/* num ring entries */
	}
	return (0);
}

static BCB *
ringget(rp)
register RING *rp;
{
	register BCB *p;

	if (rp->r_rdidx != rp->r_wrtidx) {
		FET_BD_INT(BCB*, p, rp->r_slot[rp->r_rdidx]);
		rp->r_rdidx = (rp->r_rdidx + 1) & (rp->r_size-1);
		wbflush();
	} else {
		p = 0;
	}
	return p;
}



/* reset the board
 */
static
enpreset(ei)
register struct enp_info *ei;
{
	register ENPDEVICE *addr;
	register int s = splimp();

	addr = ei->ei_addr;
	ASSERT(0 != addr);

	addr->enp_u.regs.reg_csr = 0; wbflush();
	addr->enp_state = 0; wbflush();
	addr->enp_reset = 1; wbflush();

	ei->ei_sick = 0;		/* stop watchdog resets */
	ei->ei_if.if_timer = 0;		/* turn off watchdog */

	if (ei->ei_timer_id != 0)
		untimeout(ei->ei_timer_id);
	wakeup((caddr_t)&ei->ei_state);
	ei->ei_state = EI_IDLE;

	splx(s);
}


/* see if the board has started itself yet
 */
static
enpstart2(ei)
register struct enp_info *ei;
{
	register int s = splimp();

	if (ei->ei_addr->enp_state == S_ENPRUN) {
		ei->ei_state = EI_OK;
		wakeup((caddr_t)&ei->ei_state);

	} else if (++ei->ei_timer >= R_DOG*HZ/R_CLK) {
		dri_printf("if_enp%d: firmware failed to start\n",
			   ei->ei_if.if_unit);
		ei->ei_state = EI_SICK;
		wakeup((caddr_t)&ei->ei_state);

	} else {
		ei->ei_timer_id = timeout(enpstart2,(caddr_t)ei,R_CLK);
	}
	splx(s);
}


/* see if board has reset itself yet
 */
static
enpstart1(ei)
register struct enp_info *ei;
{
	register u_short stat;
	register ENPDEVICE *addr;
	register int s = splimp();
	int idx;

	addr = ei->ei_addr;
	stat = addr->enp_u.regs.reg_csr;
	if (stat & ENPCSR_ERR) {
		dri_printf("if_enp%d: detected error on reset\n",
			   ei->ei_if.if_unit);
		ei->ei_state = EI_SICK;
		wakeup((caddr_t)&ei->ei_state);

	} else if (stat & ENPCSR_RDY) {
		if (ei->enaddr_set) {
			for (idx = 0; idx < 6; idx++) {
				addr->enp_addr.e_baseaddr.b[idx] = ei->ei_enaddr[idx];
				wbflush();
			}
			addr->enp_mode = E_EXAM_LIST;
			wbflush();
		} else {
			addr->enp_mode = 0; wbflush();	/* we will do DMA */
		}

		addr->enp_u.iface.e_intvector = ei->ei_vec;
		wbflush();			/* set interrupt vector */

		/* let controller know where its "all_ram" region
		 * begins in our address space so it can present
		 * addresses correctly to us.
		 */
		STO_BD_INT(addr->enp_base,
			   (u_int)&addr->enp_u.enp_ram[0]);

		addr->enp_state = 0; wbflush();	/* start firmware */
		addr->enp_go = 0x8080; wbflush();

		ei->ei_state = EI_START2;
		ei->ei_timer_id = timeout(enpstart2,(caddr_t)ei,R_CLK);

	} else if (++ei->ei_timer >= R_DOG*HZ/R_CLK) {
		dri_printf("if_enp%d: timed out waiting for reset\n",
			   ei->ei_if.if_unit);
		ei->ei_state = EI_SICK;
		wakeup((caddr_t)&ei->ei_state);

	} else {
		ei->ei_timer_id = timeout(enpstart1,(caddr_t)ei,R_CLK);
	}

	splx(s);
}


/* initialize the board
 */
static
enpbdinit(ei)
register struct enp_info *ei;
{
	register int s = splimp();

	enpreset(ei);			/* reset it */

	ei->ei_timer = 0;		/* then start it */
	ei->ei_state = EI_START1;
	ei->ei_timer_id = timeout(enpstart1,(caddr_t)ei,R_CLK);

	bzero(&ei->ei_stat,		/* clear error counters	*/
	      sizeof(ei->ei_stat));

	splx(s);
}


/* wait until the board has initialized itself
 */
static int				/* 0 or error number */
enp_winit(ei)
register struct enp_info *ei;
{
	register ENPDEVICE *addr;

	while (ei->ei_state == EI_START1
	       || ei->ei_state == EI_START2) {
		(void)sleep((caddr_t)&ei->ei_state,PZERO);
	}

	/* get ethernet address */
	hwcpin(&ei->ei_addr->enp_addr.e_baseaddr, &ei->ei_boardaddr,
	       sizeof(ei->ei_boardaddr));
	if (!ei->enaddr_set) {
		bcopy(&ei->ei_boardaddr, ei->ei_enaddr, sizeof(ei->ei_enaddr));
	}
	ei->ei_if.if_physaddrlen = 6;
	bcopy ((caddr_t)ei->ei_enaddr, (caddr_t)ei->ei_if.if_physaddr, 6);

	return (ei->ei_state == EI_OK ? 0 : EIO);
}


/* initialize the interface
 */
static
enpifinit(unit)
int unit;
{
	register struct enp_info *ei = &enp_info[unit];
	register struct ifnet *ifp = &ei->ei_if;

	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		register int s = splimp();
		enpbdinit(ei);
		ei->ei_if.if_flags |= IFF_RUNNING;
		ei->ei_if.if_timer = DOG;	/* turn on watchdog */
		splx(s);
	}
}


/* Periodically poll the controller in case an interrupt gets lost.
 */
static
enpwdog(unit)
int unit;
{
	register struct enp_info *ei;
	register ENPDEVICE *addr;
	register int i;
	register int s;

	ASSERT(unit >= 0 && unit < IF_ENP_MAXBD);
	ei = &enp_info[unit];
	addr = ei->ei_addr;
	ASSERT(0 != addr);

	s = splimp();
	if (ei->ei_state == EI_OK) {
		if (if_enpintr(unit))
			ei->ei_lostintrs++;

		if (addr->enp_hostfree.r_rdidx != ei->ei_rdidx) {
			ei->ei_rdidx = addr->enp_hostfree.r_rdidx;
			ei->ei_sick = 0;	/* notice successful output */

		} else if (ei->ei_sick >= 1) {
			dri_printf("if_enp%d: died and restarted\n", unit);
			enpbdinit(ei);
			ei->ei_if.if_oerrors++;

		} else if (!ringempty(&addr->enp_toenp)) { /* have output? */
			ei->ei_sick++;		/* yes, we may be sick */
		}

		/* collect error statistics */
#define FET_STAT(nm) FET_BD_INT(int, i, addr->enp_u.iface.nm);
#define ADD_STAT(nm,cnt) {FET_STAT(nm); \
		ei->ei_if.cnt += (i - ei->ei_stat.nm); \
		ei->ei_stat.nm = i; \
		}
#define MUL_STAT(nm,cnt,mul) {FET_STAT(nm); \
		ei->ei_if.cnt += (i - ei->ei_stat.nm)*(mul); \
		ei->ei_stat.nm = i; \
		}
		MUL_STAT(e_mult_retry,if_collisions,2);
		ADD_STAT(e_one_retry, if_collisions);
		ADD_STAT(e_deferrals, if_collisions);
		MUL_STAT(e_fail_retry, if_collisions,16);
		ADD_STAT(e_no_heartbeat, if_oerrors);
		ADD_STAT(e_rcv_missed, if_ierrors);
		ADD_STAT(e_crc_err, if_ierrors);
		ADD_STAT(e_frame_err, if_ierrors);
	}

	ei->ei_if.if_timer = DOG;
	splx(s);
}


/* Ethernet interface interrupt.
 *	come here at splimp()
 */
int					/* 0=did nothing, 1=found work */
if_enpintr(unit)
register int unit;
{
	register BCB *bcbp;
	register struct enp_info *ei;
	register ENPDEVICE *addr;
	register int found;

	ASSERT(unit >= 0 && unit < IF_ENP_MAXBD);
	ei = &enp_info[unit];
	addr = ei->ei_addr;
	found = 0;

	if (!addr) {
		dri_printf("enp%d: stray interrupt\n", unit);
		return found;
	}

	if (ei->ei_state == EI_START2)	/* force wait for reset to complete */
		enpstart2(ei);
	if (ei->ei_state != EI_OK) {	/* kill dead but interrupting board */
		enpreset(ei);
		return found;
	}

	while ((bcbp = ringget(&addr->enp_tohost)) != 0) {
		found = 1;
		if ((ei->ei_if.if_flags & (IFF_UP|IFF_RUNNING))
		    == (IFF_UP|IFF_RUNNING))
			enpread(&enp_info[unit], bcbp);
		ringput(&addr->enp_enpfree, bcbp);
	}

	return found;
}


/* Copy a packet from ENP into mbuf(s)
 */
static struct mbuf *
enpget(cp1, len1, cp2, len2, ifp)
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
		hwcpin(cp2, mcp, len);
		cp2 += len;
		len2 -= len;
		*mp = m;
		mp = &m->m_next;
	}
}


/*
 * receive packet from interface
 */
static
enpread(ei, bcbp)
register struct enp_info *ei;
register BCB *bcbp;
{
	register u_char *cp1, *cp2;
	register struct mbuf *m;
	short int len1, len2;
	u_short enptype;
	register struct ifqueue *inq;
	u_char *cp_ehd;
	struct mbuf *ehd_m;

	ei->ei_if.if_ipackets++;

	if (bcbp->b_stat & BCB_ERR) {	/* skip bad packets */
		ei->ei_if.if_ierrors++;
		return;
	}

	/* Get input data length.
	 * Get pointer to ethernet header (in input buffer).
	 * Deal with trailer protocol: if type is trailer
	 * get true type from first 16-bit word past data.
	 * Remember we have a trailer-packet by setting 'off'
	 */
	len1 = bcbp->b_msglen;
	if (len1 > MAX_RPKT || len1 < MIN_RPKT) {
		dri_printf("enp%d: bad receive packet length of %d\n",
			   ei->ei_if.if_unit, len1);
		ei->ei_if.if_ierrors++;
		return;
	}

	ei->ei_if.if_ioctets += len1;

	FET_BD_INT(u_char *, cp1, bcbp->b_addr);
	cp_ehd = cp1;
	if (((struct ether_header *)(cp1))->ether_dhost[0] & 0x1)
		ei->ei_if.if_inucastpackets++;
	enptype = ((struct ether_header*)(cp1))->ether_type;
	cp1 += sizeof(struct ether_header);
	len1 -= (CRC_LEN + sizeof(struct ether_header));

	if ((enptype >= ETHERTYPE_TRAIL) &&
	    (enptype < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER)) {
		register short off;

		off = (enptype - ETHERTYPE_TRAIL) * TRAILLEN;
		if (off >= ETHERMTU) {
			ei->ei_if.if_ierrors++;
			return;
		}
		cp2 = cp1 + off;
		enptype = *(u_short*)cp2;
		cp2 += sizeof(u_short);
		len2 = *(u_short*)cp2;
		cp2 += sizeof(u_short);

		if ((off + len2) > len1) {
			ei->ei_if.if_ierrors++;
			return;
		}
		len2 -= 2*sizeof(u_short);
		len1 = off + len2;

	} else {
		len2 = 0;
	}

	if (len1 <= 0)
		return;

	m = enpget(cp1,len1, cp2,len2,	/* convert to mbuf(s) */
		   &ei->ei_if);
	if (m == 0) {
		ei->ei_if.if_idiscards++;
		return;
	}
	if (enet_filter_all_hook != NULL) {
		MGET(ehd_m, M_DONTWAIT, MT_DATA);
		if (ehd_m != NULL) {
			hwcpin(cp_ehd, mtod(ehd_m,u_char *), 
				sizeof(struct ether_header));
			ehd_m->m_len = sizeof(struct ether_header);
			if ((*enet_filter_all_hook)(ehd_m,m,
				sizeof(struct ether_header) + 
					len1 + len2,1))
				return;
			m_freem(ehd_m);
		};
	};

	switch (enptype) {
#ifdef INET
	case ETHERTYPE_IP:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
	case ETHERTYPE_RARP:
		arpinput(&ei->ei_ac, m);
		return;
#endif INET
#ifdef NS
	case ETHERTYPE_NS:
		schednetisr(NETISR_NS);
		inq = &nsintrq;
		break;

#endif NS

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
		/* If eth_io_main is a dummy it will return a non-zero */
		/* value and we will have to free the mbuf ourselves. */
		if (! eth_io_main(m)) 
			return;
		/* fall through */
	default:
		if (enet_filter_hook != NULL &&
		    enet_filter_all_hook == NULL) {
			MGET(ehd_m, M_DONTWAIT, MT_DATA);
			if (ehd_m != NULL) {
				hwcpin(cp_ehd, mtod(ehd_m,u_char *), 
					sizeof(struct ether_header));
				ehd_m->m_len = sizeof(struct ether_header);
				if ((*enet_filter_hook)(ehd_m,m,
					sizeof(struct ether_header) + 
						len1 + len2,0))
					return;
				m_freem(ehd_m);
			};
		};

		ei->ei_if.if_iunknownprotos++;
		m_freem(m);
		return;
	}

	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
		ei->ei_if.if_idiscards++;
		ei->ei_if.if_ierrors++;
		return;
	}
	IF_ENQUEUE(inq, m);
	return;
}


/* copy from mbuf chain to transmitter buffer in ENP memory.
 */
static int				/* 0 or errno */
enpput(ei, m0, type, edst, esrc)
register struct enp_info *ei;		/* on this device, */
struct mbuf *m0;			/* send this chain */
u_short type;				/* of this type */
u_char *edst, *esrc;			/* with these addresses */
{
	register BCB *bcbp;
	register ENPDEVICE *addr;
	register u_char *bp;
	register u_int totlen;

	ei->ei_if.if_opackets++;

	if (ei->ei_state != EI_OK) {
		ei->ei_if.if_oerrors++;
		IF_DROP(&ei->ei_if.if_snd);
		return EIO;
	}

	addr = ei->ei_addr;

	bcbp = ringget(&addr->enp_hostfree);
	if (!bcbp) {
		ei->ei_if.if_oerrors++;
		IF_DROP(&ei->ei_if.if_snd);
		return ENOBUFS;
	}

	FET_BD_INT(u_char *, bp, bcbp->b_addr);

#define ENP ((struct ether_header*)bp)
	ENP->ether_type = type;
	hwcpout(edst, ENP->ether_dhost, sizeof(ENP->ether_dhost));
	hwcpout(esrc, ENP->ether_shost, sizeof(ENP->ether_shost));
	bp += sizeof(*ENP);
	totlen = sizeof(*ENP);

	for (; m0; m0 = m0->m_next) {
		register u_int len;

		len = m0->m_len;
		if (len == 0)
			continue;
		ei->ei_if.if_ooctets += len;
		hwcpout(mtod(m0, u_char *), bp, len);
		bp += len;
		totlen += len;
	}
	ASSERT(totlen <= MAX_TPKT);
	if (totlen < MIN_TPKT)
		totlen = MIN_TPKT;
	bcbp->b_len = totlen;
	wbflush();

	if (ringput(&addr->enp_toenp, bcbp) == 1)
		addr->enp_interrupt = 0xff;	/* interrupt the board */

	return 0;
}


/* Ethernet output routine.
 *	Encapsulate a packet of type family for the net.
 *	Use trailer local net encapsulation if enough data in first
 *	packet leaves a multiple of 512 bytes of data in remainder.
 *	If destination is this address or broadcast, send packet to
 *	loop device since we cannot talk to ourself.
 */
static int				/* 0 or error # */
enpoutput(ifp, m0, dst)
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
	register struct enp_info *ei;
	register struct mbuf *mloop = 0;

	ASSERT(ifp->if_unit >= 0 && ifp->if_unit < IF_ENP_MAXBD);
	ei = &enp_info[ifp->if_unit];
	ASSERT(0 != ei->ei_addr && ei->ei_if.if_unit == ifp->if_unit);

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
			if (!bcmp(&eaddr, etherbroadcastaddr,sizeof(eaddr))) {
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
			off -= m0->m_len;
			if (off > 0
			    && (off & (TRAILLEN-1)) == 0
			    && m0->m_off >= MMINOFF + 2*sizeof(u_short)) {
				register u_short *sp;
				register struct mbuf *m;

				type = ETHERTYPE_TRAIL + off/TRAILLEN;
				m0->m_off -= 2*sizeof(u_short);
				m0->m_len += 2*sizeof(u_short);
				sp = mtod(m0, u_short *);
				*sp = htons((u_short)ETHERTYPE_IP);
				sp++;
				*sp = htons((u_short)m0->m_len);

				/* move 1st packet of control information
				 * to the end of chain. */
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
		dri_printf("enp%d: cannot handle af%d\n",
			   ifp->if_unit, dst->sa_family);
		error = EAFNOSUPPORT;
		break;
	}

	/* Queue message on interface if possible */
	if (!error) {
		if (edst[0] & 0x1)
			ei->ei_if.if_onucastpackets++;
		s = splimp();
		error = enpput(ei, m0, htons(type), edst, ei->ei_enaddr);
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


/* Process an ioctl request
 *    this can be called via the "socket" route for SIOCSIFADDR
 *
 */
static
enpioctl(ifp, cmd, data)
register struct ifnet *ifp;
int cmd;
caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	struct enp_info *ei = &enp_info[ifp->if_unit];
	int s = splimp(), error = 0;
	ENPDEVICE *addr = ei->ei_addr;
	char tmpbuf[SIOCTLRSIZE];

	switch (cmd) {

	  case SIOCGCTLR:
	    enpgetctlr(ifp,tmpbuf);
	    error = copyout(tmpbuf,
		  	((struct ifreq *)data)->ifr_data, sizeof(tmpbuf));

	    break;
	case SIOCSIFADDR:
		if (!(ifp->if_flags & IFF_UP)) {
			ifp->if_lastchange =
				(time.tv_sec - boottime.tv_sec) * 100 + 
				(time.tv_usec - boottime.tv_usec) / 10000;
		}
		ifp->if_flags |= IFF_UP;
		switch (ifa->ifa_addr.sa_family) {
#ifdef INET
		case AF_INET:
			enpifinit(ifp->if_unit);	/* before arpwhohas */
			if (!(error = enp_winit(ei))) {
				((struct arpcom *)ifp)->ac_ipaddr =
					IA_SIN(ifa)->sin_addr;
				arpwhohas((struct arpcom *)ifp,
					  &IA_SIN(ifa)->sin_addr);
			}
			break;
#endif INET
#ifdef NS
#ifdef TODO
		case AF_NS: {
			register struct ns_addr *ina;
			ina = &(IA_SNS(ifa)->sns_addr);

			if (ns_nullhost(*ina)) {
				ina->x_host=*(union ns_host *)(ei->ei_enaddr);
			} else {
				register ETHLIST *el;
				el = (ETHLIST *)&addr->enp_addr;

				/*
				 * add another interface address to the
				 * list and inform controller
				 */
				if (++el->e_listsize == 16)
					el->e_listsize = 0;
				zzz;
				addr->enp_mode |= E_EXAM_LIST;

			}
			enpifinit(ifp->if_unit);
			error = enp_winit(ei);
			break;
			}
#endif TODO
#endif NS
		case AF_UNSPEC:
			ei->enaddr_set = 1;
			bcopy ((caddr_t)ifa->ifa_addr.sa_data, (caddr_t)ei->ei_enaddr, sizeof (ei->ei_enaddr));
			if (ifp->if_flags & IFF_RUNNING) {
				enpbdinit (ei);
			} else {
				enpifinit(ifp->if_unit);
			}
			error = enp_winit(ei);
			break;

		default:
			enpifinit(ifp->if_unit);
			error = enp_winit(ei);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		if (!(ifp->if_flags & IFF_UP)
		    && (ifp->if_flags & IFF_RUNNING)) {
			enpreset(ei);
			ei->ei_if.if_flags &= ~IFF_RUNNING;
		} else if ((ifp->if_flags & IFF_UP) &&
			   !(ifp->if_flags & IFF_RUNNING)) {
			enpifinit(ifp->if_unit);
			error = enp_winit(ei);
		}
		break;

	case BSD43_SIOCGIFSTATS:
		{
		BSD43_ENPSTAT stat;
		struct bsd43_es_stats estat;

		hwcpin(&addr->enp_u.iface.e_xmit_ok, &stat, sizeof(BSD43_ENPSTAT));
		error = copyout(&stat, ((struct ifreq *)data)->ifr_data,
			sizeof(BSD43_ENPSTAT));
		bzero(&estat,sizeof(estat)); /* XXX: temporary */
		error = copyout(&estat,
			((struct ifreq *)data)->ifr_data + sizeof(BSD43_ENPSTAT),
			sizeof(struct bsd43_es_stats));
		break;
		}

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}


/* probe and attach a single board
 */
if_enpedtinit(edtp)
register struct edt *edtp;
{
	register u_int unit;
	register struct enp_info *ei;
	register struct ifnet *ifp;
	register struct vme_intrs *intp;
	register ENPDEVICE *addr;

	if (0 == (intp = edtp->e_intr_info)
	    || IF_ENP_MAXBD <= (unit = intp->v_unit)
	    || 0 != intp[1].v_vintr) {
		dri_printf("if_enp%d: bad enp EDT entry\n", unit);
		return;
	}

	ei = &enp_info[unit];
	addr = (ENPDEVICE*)PHYS_TO_K1(edtp->e_base);

	if (IOBADADDR(&addr->enp_u.enp_ram[0], sizeof(short))) {
		if (showconfig)
			dri_printf("if_enp%d: controller not available\n",unit);
		return;
	}
	ei->ei_addr = addr;	/* say the board is here */
	ei->ei_vec = intp->v_vec; /* remember interrupt vector */

	ifp = &ei->ei_if;
	ifp->if_unit = unit;
	ifp->if_name = "enp";
	ifp->if_mtu = ETHERMTU;
	ifp->if_flags = IFF_BROADCAST;
	ifp->if_init = enpifinit;
	ifp->if_output = enpoutput;
	ifp->if_ioctl = enpioctl;

	ifp->if_description = "CMC enp-10";
	ifp->if_type = 6;		/* ethernet-csmacd */
	ifp->if_speed = 10000000;	/* 10 megabits */

	enpbdinit(ei);		/* try to start the board */
	(void)enp_winit(ei);	/* wait for board to act alive (or dead) */

	mbinit();		/* start the mbufs */

	if (showconfig)
		dri_printf("if_enp%d: hardware address %s\n",
			   unit, ether_sprintf(ei->ei_enaddr));

	ifp->if_watchdog = enpwdog;
	if_attach(ifp);
	if (enet_attach_hook != NULL)
		(*enet_attach_hook)(ifp,sizeof(struct ether_header),
				    etherbroadcastaddr);
}
enpgetctlr(ifp, buf)
struct ifnet *ifp;
char *buf;
{
	register u_int unit;
	register struct enp_info *ei;

	unit = ifp->if_unit;
	ei = &enp_info[unit];
	strcpy( buf, ifp->if_name);
        btoa( unit, &buf[strlen(buf)], 16);
	strcat( buf, " - ");
	strcat( buf, ifp->if_description);
	strcat( buf, " - ");
        btoa( ei->ei_enaddr[0], &buf[strlen(buf)], 16);
        strcat( buf, ":");
        btoa( ei->ei_enaddr[1], &buf[strlen(buf)], 16);
        strcat( buf, ":");
        btoa( ei->ei_enaddr[2], &buf[strlen(buf)], 16);
        strcat( buf, ":");
        btoa( ei->ei_enaddr[3], &buf[strlen(buf)], 16);
        strcat( buf, ":");
        btoa( ei->ei_enaddr[4], &buf[strlen(buf)], 16);
        strcat( buf, ":");
        btoa( ei->ei_enaddr[5], &buf[strlen(buf)], 16);
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

