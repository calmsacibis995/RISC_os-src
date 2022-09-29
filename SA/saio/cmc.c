#ident "$Header: cmc.c,v 1.10 90/01/16 15:43:01 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * cmc.c -- CMC ENP-10 ethernet driver for standalone system
 * Hacked from UNIX driver obtained from CMC.
 */

#ifndef PROM
/*  #define	DEBUG	1		enable debugging prints */
#endif

#define	CMCWAIT		15000		/* increments of about 1000 msec */

#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "sys/errno.h"
#include "sys/param.h"
#include "mipsif/if_enp.h"
#include "mipsvme/vmereg.h"
#include "netinet/in.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "saio/socket.h"
#include "saio/arp.h"
#include "saio/ei.h"
#include "saio/mbuf.h"

#define cei(x)	((struct ether_info *)(x->i_ino_dir))

#define	NOFLUSH	0	/* no wbflush on hwcopy */
#define	FLUSH	1	/* wbflush on hwcopy */

static ENPDEVICE *enpstd[] = { (ENPDEVICE *)0xde0000, (ENPDEVICE *)0xe00000 };

#define	NCTLRS	(sizeof(enpstd)/sizeof(enpstd[0]))

static struct enp_softc {
	int es_isopen;
	struct arpcom es_ac;
	ETHADDR es_boardaddr;
	volatile ENPDEVICE *es_devaddr;
} enp_softc[NCTLRS+1];		/* last slot reserved for dynamic csr stuff */

#ifdef DEBUG
static struct reg_desc enpmode_desc[] = {
	/*	mask		shift	name		format	values */
	{	E_SWAP16,	0,	"swap16",	NULL, 	NULL	},
	{	E_SWAP32,	0,	"swap32",	NULL,	NULL	},
	{	E_SWAPRD,	0,	"swaprd",	NULL,	NULL	},
	{	E_DMA,		0,	"enp_dma",	NULL,	NULL	},
	{	0,		0,	NULL,		NULL,	NULL	}
};

static struct reg_values ethtype_values[] = {
	{ ETHERPUP_IPTYPE,	"ip_type" },
	{ ETHERPUP_ARPTYPE,	"arp_type" },
	{ ETHERPUP_PUPTYPE,	"pup_type" },
	{ 0,			NULL }
};

static struct reg_desc ethtype_desc[] = {
	/*	mask		shift	name		format	values */
	{	-1,		0,	"eth type",	"%d",	ethtype_values},
	{	0,		0,	NULL,		NULL,	NULL	}
};
#endif DEBUG

static struct reg_desc bcbstat_desc[] = {
	/*	mask		shift	name		format	values */
	{ BCB_DONE,		0,	"DONE",		NULL,	NULL },
	{ BCB_ERR,		0,	"ERR",		NULL,	NULL },
	{ BCB_FRAME,		0,	"FRAME",	NULL,	NULL },
	{ BCB_OFLO,		0,	"OFLO",		NULL,	NULL },
	{ BCB_RXBUF,		0,	"RXBUF",	NULL,	NULL },
	{ BCB_STP,		0,	"STP",		NULL,	NULL },
	{ BCB_ENP,		0,	"ENP",		NULL,	NULL },
	{ BCB_MEMERR,		0,	"MEMERR",	NULL,	NULL },
	{ BCB_MISSED,		0,	"MISSED",	NULL,	NULL },
	{ 0,			0,	NULL,		NULL,	NULL }
};

#define	es_if		es_ac.ac_if
#define	es_enaddr	es_ac.ac_enaddr

extern struct in_addr inet_makeaddr();
static struct mbuf *enp_get();
static int enp_output();
static void enp_scan();
static void enp_read();
static void enp_put();
static void enp_getaddr();

#define ENPSTART	0xf02000	/* standard enp start addr */

/*
 * _cmcinit -- cleanup globals, also cleanup arp tables
 */
_cmcinit()
{
	if (sizeof (struct ether_info) > IOB_INODE)
		_io_abort ("bad size in iob for cmc");
	bzero(enp_softc, sizeof(enp_softc));
	_init_arp();
}

/*
 * _cmcopen -- setup CMC board and initialize network driver data structs
 */
_cmcopen(io)
struct iob *io;
{
	register struct enp_softc *es;
	struct sockaddr_in *sin;
	volatile ENPDEVICE *addr;
	char *cp;
	extern struct in_addr inet_addr();
	extern char *getenv();

	if (io->i_ctlr >= (int)NCTLRS) {
		printf("cmc bad controller number\n");
		return (-1);
	}
	if (io->i_ctlr >= 0)
		addr = (volatile ENPDEVICE *)
		  (IS_R6300 ? find_r6000_controller( enpstd[io->i_ctlr]
						    ,0x1000
						    ,sizeof(short) )
			    : PHYS_TO_K1(VMENPA24_TO_PHYS(enpstd[io->i_ctlr])));
	else {
		addr = (volatile ENPDEVICE *)
		  (IS_R6300 ? find_r6000_controller( -io->i_ctlr
						    ,0x1000
						    ,sizeof(short))
			    : PHYS_TO_K1(VMENPA24_TO_PHYS(-io->i_ctlr)));
		io->i_ctlr = NCTLRS;
		enp_softc[NCTLRS].es_isopen = 0;
	}
	io->i_devaddr = (unsigned)addr;
	cei(io)->ei_registry = -1;	/* mark not bound yet */
	es = &enp_softc[io->i_ctlr];
	if (!es->es_isopen) {
		if (enp_init(io, addr) < 0)
			return(-1);

		es->es_if.if_unit = io->i_ctlr;
		es->es_if.if_mtu = ETHERMTU;
		es->es_if.if_output = enp_output;
		es->es_devaddr = addr;
		cei(io)->ei_acp = &es->es_ac;	/* iob ptr to arpcom */

		enp_getaddr(io->i_ctlr);
		bcopy(&es->es_boardaddr, es->es_enaddr, sizeof(es->es_enaddr));

		sin = (struct sockaddr_in *)&es->es_if.if_addr;
		bzero(sin, sizeof(struct sockaddr_in));
		sin->sin_family = AF_INET;
		cp = getenv("netaddr");
		if (!cp || !*cp)
			goto bad;
		sin->sin_addr = inet_addr(cp);
		if (*(int *)&sin->sin_addr == -1) {
bad:
			printf("$netaddr not set or incorrect\n");
			io->i_errno = EADDRNOTAVAIL;
			return(-1);
		}
		es->es_isopen = 1;
	}
	io->i_flgs |= F_SCAN;
	return(0);
}

/*
 * _cmcioctl -- network control operations
 */
_cmcioctl(io, cmd, arg)
struct iob *io;
int cmd;
int arg;
{
	struct so_table *st;
	struct sockaddr_in sin;

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
			return(0);
		}

		/* find an empty slot */
		st = _get_socket();
		if (st == NULL) {
			printf("out of socket buffers\n");
			return(-1);
		}

#ifdef DEBUG
		printf("binding to %d 0x%x %d\n", sin.sin_family,
		    sin.sin_addr, ntohs((u_short)sin.sin_port));
#endif

		st->st_udpport = arg;
		cei(io)->ei_registry = st - _so_table;
		break;

	case FIOCSCAN:
		enp_scan(io->i_ctlr);
		break;
	}
	return(0);
}

/*
 * _cmcstrategy -- performs io
 */
_cmcstrategy(io, func)
register struct iob *io;
{
	register struct mbuf *m;
	struct so_table *st;
	int ocnt;

	if (cei(io)->ei_registry < 0) {
bad:
		printf("socket not bound\n");
		io->i_errno = EINVAL;
		return (-1);
	}
	st = &_so_table[cei(io)->ei_registry];
	if (st->st_count <= 0) {
		printf("socket screw-up\n");
		goto bad;
	}

	if (func == READ) {
		while ((io->i_flgs & F_NBLOCK) == 0 && st->st_mbuf == NULL)
			_scandevs();

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
			return(0);
	} else if (func == WRITE) {
		m = _m_get();
		if (m == 0) {
			printf("out of mbufs\n");
			io->i_errno = EIO;
			return (-1);
		}
		if (io->i_cc > MLEN - MMAXOFF) {
			_m_freem(m);
			printf("datagram too large\n");
			io->i_errno = EIO;
			return(-1);
		}
		m->m_off = MMAXOFF;
		bcopy(io->i_ma, mtod(m, char *), io->i_cc);
		m->m_len = io->i_cc;
		ocnt = _udp_output(io, &enp_softc[io->i_ctlr].es_if, m);
		return(ocnt < 0 ? -1 : io->i_cc);
	} else
		_io_abort("cmc bad function");
}

/*
 * _cmcclose -- release any socket that's being held
 */
_cmcclose(io)
struct iob *io;
{
	if (cei(io)->ei_registry >= 0)
		_free_socket(cei(io)->ei_registry);
}

enp_init(io, addr)
register struct iob *io;
register volatile ENPDEVICE *addr;
{
	register i;
#ifdef DEBUG
	unsigned enpbase;
#endif

	if (badaddr(addr->enp_ram, sizeof(short))) {
		printf("no cmc controller at 0x%x \n", addr);
		goto cmcerror;
	}

#ifdef DEBUG
	printf("resetting cmc\n");
#endif DEBUG
	/*
	 * reset the enp10 processor and wait
	 * for it to come ready
	 */
	addr->enp_csr = 0;
	wbflush();
	addr->enp_iow.hst2enp_reset = 01;
	wbflush();
	i = 0;
	while (i++ < 200		/* give the CMC at least 10-15 secs */
	    && (addr->enp_csr & (ENPCSR_RDY|ENPCSR_ERR)) == 0)
		DELAY(131072);	/* wait about 130 msec -- no rush! */
	if (addr->enp_csr & ENPCSR_ERR) {
		printf("cmc detected error on reset\n");
		goto cmcerror;
	}
	if (i >= 200) {
		printf("timed out waiting for cmc to reset\n");
		goto cmcerror;
	}
#ifdef DEBUG
	printf("cmc reset done\n");
#endif

	/*
	 * no swaps, we do the dma
	 */
	addr->enp_mode = 0;
	wbflush();

	/*
	 * let enp10 firmware know where its "all_ram" region
	 * begins in our address space so it can present addresses
	 * correctly to us.
	 */
	UINT_uint(addr->enp_base, (unsigned)(addr->enp_ram));

#ifdef DEBUG
	printf("cmc start applic firmware\n");
#endif
	/*
	 * transfer control from enp kernel to application firmware
	 */
	addr->enp_state = 0;
	wbflush();
	addr->enp_go = 0x8080;
	wbflush();
	i = 0;
	while (i++ < CMCWAIT && addr->enp_state != S_ENPRUN)
		DELAY(1024);

	if (i >= CMCWAIT) {
		printf("cmc application firmware failed\n");
cmcerror:
		io->i_errno = ENXIO;
		return(-1);
	}

#ifdef DEBUG
	printf("cmc applic firmware running\n");
	uint_UINT(unsigned, enpbase, addr->enp_base);
	printf("cmc base: 0x%x\n", enpbase);
	printf("cmc mode: %R\n", addr->enp_mode, enpmode_desc);
#endif
	return(0);
}

/*
 * enp_output -- raw access to transmit a packet
 */
static
enp_output(ifp, m0, dst)
struct ifnet *ifp;
struct mbuf *m0;
struct sockaddr *dst;
{
	ETHADDR edst;
	struct in_addr idst;
	int unit = ifp->if_unit;
	struct enp_softc *es = &enp_softc[unit];
	struct ether_header *eh;
	int off, i, type;

#ifdef DEBUG
	printf("sending to %d 0x%x %d, cc=%d\n", dst->sa_family,
	    ((struct sockaddr_in *)dst)->sin_addr,
	    ntohs(((struct sockaddr_in *)dst)->sin_port), m0->m_len);
#endif

	switch (dst->sa_family) {

	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;
		_arpresolve(&es->es_ac, &idst, &edst);
		type = ETHERPUP_IPTYPE;
		off = 0;
		break;

	case AF_UNSPEC:
		eh = (struct ether_header *)dst->sa_data;
		bcopy(eh->ether_dhost, &edst, sizeof(edst));
		type = eh->ether_type;
		break;

	default:
		/* EAFNOSUPPORT */
		_m_freem(m0);
		printf("no support for address family %d\n",
		    dst->sa_family);
		return(-1);
	}

	/*
         * Add local net header.  If no space in mbuf, drop it (shouldn't
	 * happen!)
         */
	if (m0->m_off < sizeof(struct ether_header)
	    || m0->m_off + m0->m_len > MLEN) {
		printf("cmc_output -- no space in mbuf\n");
		_m_freem(m0);
		return(-1);
	}

	m0->m_off -= sizeof(struct ether_header);
	m0->m_len += sizeof(struct ether_header);
	eh = mtod(m0, struct ether_header *);
	eh->ether_type = htons((u_short)type);
	bcopy(&edst, eh->ether_dhost, sizeof(edst));
	bcopy(es->es_enaddr, eh->ether_shost, sizeof(es->es_enaddr));

	/*
	 * Queue message on interface
	 */
	enp_put(unit, m0);
	return(0);
}

/*
 * enp_put -- Routine to copy from mbuf chain to transmitter
 * buffer in VMEbus memory.
 */
static void
enp_put(unit, m)
int unit;
struct mbuf *m;
{
	volatile ENPDEVICE *addr;
	volatile BCB *bcbp;
	char *b_addr;

	addr = enp_softc[unit].es_devaddr;

	while (ringempty(&addr->enp_hostfree))
		_scandevs();

	bcbp = (BCB *)ringget(&addr->enp_hostfree);
	uint_UINT(char *, b_addr, bcbp->b_addr);
	hwcopy(mtod(m, char *), b_addr, m->m_len, FLUSH);
	bcbp->b_len = _max(MINPKTSIZE, m->m_len);
	wbflush();

	if (ringput(&addr->enp_toenp, bcbp) == 1)
		INTR_ENP(addr);
	_m_freem(m);
}

/*
 * enp_scan -- look for recv'd packets
 */
static void
enp_scan(unit)
{
	register volatile ENPDEVICE *addr;
	register struct enp_softc *es;
	register volatile BCB *bcbp;

	es = &enp_softc[unit];
	addr = es->es_devaddr;

	while ((bcbp = (BCB *)ringget(&addr->enp_tohost)) != 0) {
		enp_read(es, bcbp);
		ringput(&addr->enp_enpfree, bcbp);
	}
}

/*
 * enp_read -- pull packet of interface and forward to appropriate
 * protocol handler
 */
static void
enp_read(es, bcbp)
struct enp_softc *es;
volatile BCB *bcbp;
{
	register struct mbuf *m;
	volatile struct ether_header *eh;
	int len, off, resid, type, i;
	char *b_addr;
	extern Verbose;

	/*
	 * Get input data length.
	 * Get pointer to ethernet header (in input buffer).
	 * Deal with trailer protocol: if type is PUP trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */

	if (bcbp->b_stat & BCB_ERR) {
		if (Verbose)
			printf("cmc: recvd bad packet, stat %R\n",
			    bcbp->b_stat, bcbstat_desc);
		return;
	}
	len = bcbp->b_msglen;
	uint_UINT(struct ether_header *, eh, bcbp->b_addr);

#define enpdataaddr(eh, off, type)       ((type)(((caddr_t)((eh)+1)+(off))))

	type = eh->ether_type;
	if (type >= ETHERPUP_TRAIL
	    && type < ETHERPUP_TRAIL+ETHERPUP_NTRAILER) {
		off = (type - ETHERPUP_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;			/* sanity */
		type = *enpdataaddr(eh, off, u_short *);
		resid = *enpdataaddr(eh, off+2, u_short *);

		if (off + resid > len)
			return;			/* sanity */
		len = off + resid;
	} else
		off = 0;

	if( len == 0 )
		return;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; enp_get will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = enp_get(eh, len, off);
	if (m == NULL)
		return;

	if (off) {
		m->m_off += 2 * sizeof(u_short);
		m->m_len -= 2 * sizeof(u_short);
	}

#ifdef DEBUG
	printf("ether type = %R\n", type, ethtype_desc);
#endif
	switch (type) {
	case ETHERPUP_IPTYPE:
		_ip_input(&es->es_if, m);
		break;

	case ETHERPUP_ARPTYPE:
		_arpinput(&es->es_ac, m);
		break;

	default:
		_m_freem(m);
	}
}

/*
 * enp_get -- Routine to copy from VMEbus memory into mbufs.
 * Basically deals with trailers
 */
static struct mbuf *
enp_get(eh, totlen, off0)
volatile struct ether_header *eh;
int totlen, off0;
{
	register struct mbuf *m;
	char *cp;
	int len;

	if (totlen > MLEN || totlen == 0)
		return(0);

	totlen -= (sizeof(struct ether_header) + 4); /* remember the CRC! */
	m = _m_get(M_DONTWAIT, MT_DATA);
	if (m == 0)
		return(0);

	len = totlen - off0;
	m->m_off = MMAXOFF;
	while (1) {
		/*
		 * If this is a trailer packet (off0 != 0), this picks up the
		 * trailing header and puts it at the beginning of the mbuf;
		 * if this is a regular packet (off0 == 0), this gets the
		 * entire packet.
		 */
		cp = enpdataaddr(eh, off0, char *);
		hwcopy(cp, mtod(m, char *), len, NOFLUSH);
		if (off0 == 0)	/* no trailer, so done */
			break;

		/*
		 * This is a trailer packet, now go around and pick up
		 * rest of data and put after the header by temporarily
		 * setting m_off to skip the header that we've already
		 * copied.
		 */
		m->m_off += len;
		len = off0;
		off0 = 0;
	}
	m->m_off = MMAXOFF;
	m->m_len = totlen;
	return(m);
}

/*
 * enp_getaddr -- Get the ethernet addr, store it and print it
 */
static void
enp_getaddr(unit)
int unit;
{
	struct enp_softc *es = &enp_softc[unit];
	volatile ENPDEVICE *addr = es->es_devaddr;
	
	hwcopy(&addr->enp_addr.e_baseaddr, &es->es_boardaddr,
	    sizeof(es->es_boardaddr), NOFLUSH);
	return;
}

/*
 * ringempty -- buffer ring empty predicate
 */
static
ringempty(rp)
volatile RING *rp;
{
	return(rp->r_rdidx == rp->r_wrtidx);
}

/*
 * ringput -- insert buffer in ring
 */
static
ringput(rp, v)
register volatile RING *rp;
register volatile BCB  *v;
{
	register int idx;

	idx = (rp->r_wrtidx + 1) & (rp->r_size-1);
	if (idx != rp->r_rdidx) {
		UINT_uint(rp->r_slot[rp->r_wrtidx], v);
		rp->r_wrtidx = idx;
		wbflush();
		if ((idx -= rp->r_rdidx) < 0 )
			idx += rp->r_size;
		return(idx);			/* num ring entries */
	}
	return(0);
}

/*
 * ringget -- remove buffer from ring
 */
static
ringget(rp)
register volatile RING *rp;
{
	int i = 0;

	if (rp->r_rdidx != rp->r_wrtidx) {
		uint_UINT(int, i, rp->r_slot[rp->r_rdidx]);
		rp->r_rdidx = (rp->r_rdidx + 1) & (rp->r_size-1);
		wbflush();
	}
	return(i);
}

#ifdef notdef
/*
 * enpram device
 * (This stuff is for downloadable CMC firmware)
 */
_enpr_strategy(io, func)
struct iob *io;
{
	if (func == READ)
		return(enpr_read(io, io->i_ma, io->i_cc));
	else if (func == WRITE)
		return (enpr_write(io, io->i_ma, io->i_cc));
	else {
		io->i_errno = ENXIO;
		return(-1);
	}
}

_enpr_ioctl(io, cmd, arg)
struct iob *io;
caddr_t *arg;
{
	register volatile ENPDEVICE *addr;

	addr = (volatile ENPDEVICE *)io->i_devaddr;

	switch (cmd) {
		case ENPIOGO:
#ifndef ENPPROM
			addr->enp_base = (int)addr;
			ENP_GO(addr,ENPSTART);
			DELAY(1024);
			enpinit(dev);
#endif ENPPROM
			break;

		case ENPIORESET:
			RESET_ENP(addr);
			break;
	}
	return( 0 );
}

static
enpr_read(io, buf, cnt)
struct iob *io;
char *buf;
int cnt;
{
	volatile ENPDEVICE *addr;

	addr = (volatile ENPDEVICE *)io->i_devaddr;
	hwcopy(&addr->enp_ram[io->i_offset], buf, cnt, NOFLUSH);
	return(cnt);
}

static
enpr_write(io, buf, cnt)
struct iob *io;
char *buf;
int cnt;
{
	volatile ENPDEVICE *addr;

	addr = (ENPDEVICE *)io->i_devaddr;
	hwcopy(buf, &addr->enp_ram[ io->i_offset], cnt, FLUSH);
	return(cnt);
}
#endif

hwcopy(src, dst, bcnt, do_wbflush)
register unsigned src;
register unsigned dst;
register unsigned bcnt;
register do_wbflush;
{
	register unsigned qwcnt;

	if (((src ^ dst) & 1) == 0) {	/* can align */
		if (src & 1) {
			*(char *)dst = *(char *)src;
			dst++; src++; bcnt--;
		}
		if (do_wbflush != NOFLUSH)
			wbflush();
		qwcnt = bcnt >> 4;
		bcnt &= 0xf;
		while (qwcnt) {
			/*
			 * This nonsense is to avoid byte gathering
			 * in the write buffer which would convert the
			 * halfword writes in to full words writes
			 * which the cmc isn't prepared to recognize
			 */
			*(volatile short *)dst = *(short *)src;
			*(volatile short *)(dst+8) = *(short *)(src+8);
			*(volatile short *)(dst+12) = *(short *)(src+12);
			*(volatile short *)(dst+4) = *(short *)(src+4);

			*(volatile short *)(dst+10) = *(short *)(src+10);
			*(volatile short *)(dst+2) = *(short *)(src+2);
			*(volatile short *)(dst+14) = *(short *)(src+14);
			*(volatile short *)(dst+6) = *(short *)(src+6);

			dst += 16; src += 16; qwcnt--;
		}
	} else {
		qwcnt = bcnt >> 4;
		bcnt &= 0xf;
		while (qwcnt) {
			*(volatile char *)dst = *(char *)src;
			*(volatile char *)(dst+8) = *(char *)(src+8);
			*(volatile char *)(dst+12) = *(char *)(src+12);
			*(volatile char *)(dst+4) = *(char *)(src+4);

			*(volatile char *)(dst+13) = *(char *)(src+13);
			*(volatile char *)(dst+1) = *(char *)(src+1);
			*(volatile char *)(dst+5) = *(char *)(src+5);
			*(volatile char *)(dst+9) = *(char *)(src+9);

			*(volatile char *)(dst+2) = *(char *)(src+2);
			*(volatile char *)(dst+10) = *(char *)(src+10);
			*(volatile char *)(dst+14) = *(char *)(src+14);
			*(volatile char *)(dst+6) = *(char *)(src+6);

			*(volatile char *)(dst+11) = *(char *)(src+11);
			*(volatile char *)(dst+15) = *(char *)(src+15);
			*(volatile char *)(dst+7) = *(char *)(src+7);
			*(volatile char *)(dst+3) = *(char *)(src+3);
			dst += 16; src += 16; qwcnt--;
		}
	}
	if (do_wbflush != NOFLUSH)
		wbflush();
	/*
	 * move remaining bytes
	 */
	while (bcnt) {
		*(char *)dst = *(char *)src;
		dst++; src++; bcnt--;
		if (do_wbflush != NOFLUSH)
			wbflush();
	}
}
