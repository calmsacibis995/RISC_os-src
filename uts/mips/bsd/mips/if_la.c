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
#ident	"$Header: if_la.c,v 1.11.1.7.1.2.1.2 90/10/16 12:02:29 beacker Exp $"
/*
 * AMD 7990 "Lance" Ethernet Controller Driver
 *
 * Modified for Intrepid by mdove, himel, djl.
 */

#define	SYSVR2			/* system V release 2 */

#undef ECHO

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
#include "bsd43/mips/hwconf.h"

#include "../net/soioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "sys/if_lareg.h"
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

/* Procedures called from Kernel */
int	if_laintr();
int	if_laedtinit(), la_init();
struct ladevice	*laprobe();


/* External Procedures Called (some indirectly via macros) */
extern	int	spl0(), spl6(), splx(), timeout();

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

/* Globally Accessible (within this module) Variables */
static volatile int Interrupt;		/* Says if Interrupt being processed */
static int La_running;	 		/* True if lance on and running */

extern int showconfig;

#include "sys/if_laiocb.h"

#define	LA_NRETRY	5
#define	NLA		1

struct la_m12 {
	u_int	*command;
	int	rcv_index;
	int	xmt_index;
	int	xmt_last;
	int	xmt_count;
	int	stat;			/* return stat, last command */
#ifdef	TIMEOUT
	int	timeout_id;
#endif
} la_m12;

struct eth_low	la_eth_low;

int la_set_lance();
unsigned short la_swap();
unsigned char get_stat();
unsigned char get_hadr();
int set_stat();
int set_hadr();
caddr_t labuf_i2000();
int larcvcopy();

if_laedtinit(edtp)
struct edt *edtp;
{
	register struct eth_low	*elp;

	elp = &la_eth_low;
	elp->e_low_probe = laprobe;
	elp->e_low_set_lance = la_set_lance;
	elp->e_low_intr = if_laintr;
	elp->e_low_swap = la_swap;
	elp->e_low_g_stat = get_stat;
	elp->e_low_s_stat = set_stat;
	elp->e_low_g_hadr = get_hadr;
	elp->e_low_s_hadr = set_hadr;
	elp->e_low_bufaddr = labuf_i2000;;
	elp->e_low_rcvcopy = larcvcopy;

	e_low_ptr = elp;

	edtp->e_intr_info->v_vintr = if_laintr;

	return la_edtinit(edtp);
}

/* Probe for device */
struct ladevice *
laprobe(unit)
	int unit;
{
	register struct la_m12	*ls = &la_m12;
	register int			i;
	register int			retry = 0;

	if ((ls->command = (u_int *) iop_alloc(LANCEIOCB, LAIOCBMAX *NLA)) == 0)
		panic("laprobe");
	laflush(unit);
	ls->stat = 0;
	*ls->command = DO_INTERRUPT;
again:
	if (lasend(unit, LA_PROBE, LANOWAIT, (caddr_t) 0) < 0) {
		log(BSD43_LOG_ERR, "   LA%d  probe: can't poke iop\n", unit);
		return 0;
	}
#ifdef  SABLE
	for (i = 100; i; i--)
#else   SABLE
	for (i = 10000; i; i--)
#endif  SABLE
	{
		if (ls->stat == LA_PROBE)
			break;
		DELAY(200);
	}
	laclear(unit);
	if (ls->stat == LA_ERROR) {
		log(BSD43_LOG_ERR, "   LA%d  cannot probe the lance\n", unit);
		return 0;
	} else if (ls->stat != LA_PROBE) {
		if (++retry == LA_NRETRY) {
			log(BSD43_LOG_ERR, "   LA%d  timeout\n", unit);
			return 0;
		}
		goto again;
	}
	if (!Ls.enaddr_set) {
		bcopy(ls->command, Ls.ls_enaddr, sizeof(Ls.ls_enaddr));
	}
	Ls.ls_if.if_physaddrlen = 6;
	bcopy ((caddr_t)Ls.ls_enaddr, (caddr_t)Ls.ls_if.if_physaddr, 6);
	return (struct ladevice *)1;
}

int ladebug = 0;

/****************/
/* set up lance */
/****************/

int
la_set_lance(addr)
struct ladevice *addr;
{
	register struct la_dre *dre;
	register struct la_m12	*ls = &la_m12;
	struct la_vtop *vp;
	register int	i;
	int	retry = 0;
	int	stat;
	int	unit = Ls.ls_if.if_unit;

	/* Initialise Address of Logical Address Filter */
	if (!Ls.laf_set)
		Ls.ls_laf = (unsigned char *)LAFILTER;

	/* Initialise Current Descriptor Ring Entry Addresses & Counts */
	Ls.ls_rxin = Ls.ls_txin = 0;
	Ls.ls_rxout = Ls.ls_txout = 0;

	/* stop the lance chip */
	if(lastop() < 0)
		return(-1);

	/* the following is for the 2030 which has no serial number. */
	/* therefore we use the format Z#### where the 4 numbers are */
	/* the last 4 HEX digits of the ethernet hardware id */
        if (IS_I2000)
	{
		extern struct bsd43_hw_config bsd43_hwconf;
		register int i;
		register char *p = &bsd43_hwconf.cpubd_snum[0];
		char *nums = "0123456789ABCDEF";
		*p++ = 'Z';
		i = (Ls.ls_enaddr[4] & 0xF0) >> 4;
		*p++ = nums[i];
		i = (Ls.ls_enaddr[4] & 0xF);
		*p++ = nums[i];
		i = (Ls.ls_enaddr[5] & 0xF0) >> 4;
		*p++ = nums[i];
		i = (Ls.ls_enaddr[6] & 0xF);
		*p++ = nums[i];
	}
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

	/* set up ethernet address */
	bcopy(Ls.ls_enaddr, La_ibp->ib_padr, STATADDRSIZE);

	/* setup address filter */
	bcopy(Ls.ls_laf, La_ibp->ib_ladrf, LAFSIZE);

	/* set up receive and transmit ring descriptor pointers */
	La_ibp->ib_rdral = LA_SWAP(devaddr(La_rxdrep));	/* LSP */
	SET_HADR(La_ibp->ib_rdram,  devaddr(La_rxdrep) >> WORDSHIFT); /* MSP */
	/* NB: length here is log2(number of descriptors) */
	SET_LEN(La_ibp->ib_rlen, MAX_RXDRE << 5);

	La_ibp->ib_tdral = LA_SWAP(devaddr(La_txdrep));	/* ibid */
	SET_HADR(La_ibp->ib_tdram,  devaddr(La_txdrep) >> WORDSHIFT); /* MSP */
	SET_LEN(La_ibp->ib_tlen, MAX_TXDRE << 5);
	XPR1(XPR_ENET,"Setting Up Descriptor Rings\n");
	/* Set up Descriptor Rings */
	for(dre = La_rxdrep, vp = R_vtop;
		dre < &La_rxdrep[1 << MAX_RXDRE]; ++dre, ++vp)
	{
		if(vp->virtual) {
			m_free(vp->virtual);
			vp->virtual = 0;
		}
		SET_STAT(dre->dre_stat,0);
	}
	lafillrcv();
	for(dre = La_txdrep, vp = T_vtop;
		dre < &La_txdrep[1 << MAX_TXDRE]; ++dre, ++vp)
	{
		if(vp->virtual) {
			m_freem(vp->virtual);
			vp->virtual = 0;
		}
		SET_STAT(dre->dre_stat,0);
	}
	*ls->command = K0_TO_PHYS((u_int)La_ibp);

	while (lasend(unit, LA_INIT, LASPIN, (caddr_t) 0) != LA_INIT) {
		if (++retry == LA_NRETRY) {
			log(BSD43_LOG_ERR, "lainit: could not init lance\n");
			return -1;
		}
	}

	retry = 0;

#ifdef  SABLE
	for (i = 1000000; i; i--)
#else   SABLE
	for (i = 10000; i; i--)
#endif  SABLE
	{
		if (iop_wait(LANCEIOCB + unit, IOPB_NOWAIT, &stat, 0) < 0) {
			DELAY(200);
		} else {
			if (stat == LA_INIT_DONE)
				break;
			laclear(unit);
		}
	}
	ls->stat = stat;
	laclear(unit);
	if (stat != LA_INIT_DONE) {
		log(BSD43_LOG_ERR, "   LA%d  init did not complete\n", unit);
		return -1;
	}
	La_running = 1;
	return(0);	/* XXX return status never checked by callers */
}

/********************************/
/* Ethernet interface Interrupt */
/********************************/

#define	LAMISS_INC	50
int	la_miscnt;
int	la_misnext;

int
if_laintr()
{
	register volatile struct la_m12	*ls = &la_m12;
	int	stat;
	int	unit = Ls.ls_if.if_unit;

	lastats.lasInterrupts++;
	if (iop_wait(LANCEIOCB + unit, IOPB_NOWAIT, &stat, 0) < 0) {
		return;
	}
	if (stat < 0) {
		log(BSD43_LOG_INFO, "if_laintr: iop comm failure");
		laclear(unit);
		return;
	}
	switch (stat) {

	    case LA_RECV:
		laclear(unit);
		/* Allow only one invocation of laread() at any time */
		if (!Interrupt && La_running)
		{
			Interrupt = 1;
			lastats.lasRcvInts++;
			laread();
			Interrupt = 0;
		}
		/* check to see if we need to update the miss msg counter */
		break;

	    case LA_MISS:
		lastats.lasMiss++;
		laclear(unit);
		/* Allow only one invocation of laread() at any time */
		if (!Interrupt && La_running)
		{
			Interrupt = 1;
			laread();
			Interrupt = 0;
		}
		/* should we put out some messages? */
		if (la_miscnt >= la_misnext) {
			log(BSD43_LOG_ERR,
			    "if_laintr: Lance Missed packet, missed %d\n",
			    la_miscnt);
			/* have we put out too many? */
			la_misnext += LAMISS_INC;
		}
		break;

	    case LA_RESET:
		log(BSD43_LOG_ERR, "if_laintr: reset by iop\n");
		laclear(unit);
		reinit(unit);
		break;

	    case LA_XMIT_DONE:
		laclear(unit);
#ifdef	TIMEOUT	
		if (ls->timeout_id) {
			untimeout(ls->timeout_id);
			ls->timeout_id = 0;
		}
#endif
		if(La_running) {
			lastats.lasRcvInts++;
			laflushxmit(0);
			if(Ls.ls_if.if_snd.ifq_head)
				lastart();
		}
		break;

	    case LA_STAT:
		laclear(unit);
#if 0
		lastat(ls);
#endif
		break;

	    case LA_STOP:
	    case LA_STRT:
	    case LA_INIT:
	    case LA_PROBE:
	    case LA_INIT_DONE:
	    case LA_DBG_ON:
	    case LA_DBG_OFF:
		break;

	    case LA_ERROR:		/* unexpected */
		log(BSD43_LOG_ERR, "if_laintr: error status returned\n");
		break;

	    case LA_XMIT:
	    default:
		panic("if_laintr: invalid status");
		break;
	}
	ls->stat = stat;
}

la_get_rdp(addr)
struct ladevice *addr;
{
	return CSR_TXON | CSR_STRT;
#if 0
	/* XXX */
	return la_m12.stat;
#endif
}

laclear(unit)
int 	unit;
{
	(void) iop_clear(LANCEIOCB + unit);
}

lareset(addr)
struct ladevice *addr;
{
	register struct la_m12		*ls = &la_m12;
	register struct iop_device	*iop;
	int	unit = Ls.ls_if.if_unit;
	register int			retry = 0;

#ifdef	SVR3
	if (unit >= NLA)
#else	SVR3
	if (unit >= NLA || (iop = ladinfo[unit]) == 0 || iop->ii_alive == 0)
#endif	SVR3
		return -1;
	Ls.ls_if.if_timer = 0;			/* turn off watchdog */
	Ls.ls_if.if_flags &= ~IFF_RUNNING;

	if(lastop() < 0)
		return(-1);
	if (ladebug)
		lasend (0, LA_DBG_OFF, LASPIN, (caddr_t) 0);

	while (lasend(unit, LA_STRT, LASPIN, (caddr_t) 0) != LA_STRT) {
		if (++retry == LA_NRETRY) {
			log(BSD43_LOG_ERR, "lainit: could not start lance\n");
			return -1;
		}
	}
	if (ladebug)
		(void) lasend(unit, LA_DBG_ON, LASPIN, (caddr_t) 0);

	return 0;	/* XXX return status never checked by callers */
}

/*
 * send a command to the iop
 * if flag == NOWAIT return result of iop_poke
 * else (if flag == SPIN || SLEEP), wait and return status_sem
 */

lasend(unit, val, flag, sleepaddr)
	int 	unit;
	int	val;
	int	flag;
	caddr_t	sleepaddr;

{
	register int		iopflag;
	register struct la_m12	*ls = &la_m12;
	int			stat = 0;

	switch (flag) {
	    case LANOWAIT:	iopflag = IOPB_NOWAIT;	break;
	    case LASPIN:	iopflag = IOPB_SPIN;	break;
	    case LASLEEP:	iopflag = IOPB_SLEEP;	break;
	}
	wbflush();
	if (iop_poke(LANCEIOCB + unit, iopflag, val, sleepaddr) < 0) 
		return LA_ERROR;
	if (flag == LANOWAIT)
		return 0;
	(void) iop_wait(LANCEIOCB + unit, iopflag, &stat, 0);
	laclear(unit);
	if (flag == LASPIN)
		return stat;
	else
		return ls->stat;
}

laflush(unit)
	int 	unit;
{
	int	stat;

	while (iop_wait(LANCEIOCB + unit, IOPB_NOWAIT, &stat, 0) >= 0) {
		laclear(unit);
		DELAY(4000);
	}
}

unsigned short
la_swap(x)
unsigned int	x;
{
	return (unsigned short)((((x)&0xff)<<8)|(((x)&0xff00)>>8));
}

int
set_stat(cp, st)
unsigned char *cp;
unsigned char st;
{
	cp[1] = st;
	return st;
}

unsigned char
get_stat(cp)
unsigned char *cp;
{
	return cp[1];
}

int
set_hadr(cp, st)
unsigned char *cp;
unsigned char st;
{
	cp[-1] = st;
	return st;
}

unsigned char
get_hadr(cp)
unsigned char *cp;
{
	return cp[-1];
}

/*	Stop the lance
 */

lastop()

{
	int unit;
	int retry;

	unit = Ls.ls_if.if_unit;
	retry = 0;
	while (lasend(unit, LA_STOP, LASPIN, (caddr_t) 0) != LA_STOP) {
		if (++retry == LA_NRETRY) {
			log(BSD43_LOG_ERR, "cannot stop lance\n");
			return -1;
		}
	}
	return 0;
}


/*	Return the address of the DMA buffer.
 *	For this machine, it is the physical address of the mbuf contents.
 */

caddr_t
labuf_i2000(index, m, isRcv)

int	index;
struct mbuf *m;
int	isRcv;
{

	return((caddr_t)Mvtop(mtod(m, caddr_t)));
}
