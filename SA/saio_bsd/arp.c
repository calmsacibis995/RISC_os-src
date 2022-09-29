#ident "$Header: arp.c,v 1.2 89/12/04 08:58:25 unixexp Exp $"
/*	%Q%	%I%	%M%	*/

/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

/*
 * Ethernet address resolution protocol.
 * Swiped from 4.2BSD kernel code.
 */

#undef KERNEL
#include "sys/types.h"
#include "netinet/in.h"
#include "saio/setjmp.h"
#include "saio/socket.h"
#include "saio/arp.h"		/* should just use standard files */
#include "saio/mbuf.h"

#define	NULL	0

/*
 * Internet to ethernet address resolution table.
 */
struct	arptab {
	struct	in_addr at_iaddr;	/* internet address */
	u_char	at_enaddr[6];		/* ethernet address */
	u_char	at_flags;		/* flags */
};
/* at_flags field values */
#define	ATF_INUSE	1		/* entry in use */
#define ATF_COM		2		/* completed entry (enaddr valid) */

#define	ARPTAB_BSIZ	5		/* bucket size */
#define	ARPTAB_NB	19		/* number of buckets */
#define	ARPTAB_SIZE	(ARPTAB_BSIZ * ARPTAB_NB)

static struct arptab arptab[ARPTAB_SIZE];
static struct arptab *arptnew();
static void arptfree();

#define	ARPTAB_HASH(a) \
	((short)((((a) >> 16) ^ (a)) & 0x7fff) % ARPTAB_NB)

#define	ARPTAB_LOOK(at,addr) { \
	register n; \
	at = &arptab[ARPTAB_HASH(addr) * ARPTAB_BSIZ]; \
	for (n = 0 ; n < ARPTAB_BSIZ ; n++,at++) \
		if (at->at_iaddr.s_addr == addr) \
			break; \
	if (n >= ARPTAB_BSIZ) \
		at = 0; }

static u_char etherbroadcastaddr[6] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };

#ifdef notdef
/*
 * Local addresses in the range oldmap to infinity are
 * mapped according to the old mapping scheme.  That is,
 * mapping of Internet to Ethernet addresses is performed
 * by taking the high three bytes of the network interface's
 * address and the low three bytes of the local address part.
 * This only allows boards from the same manufacturer to
 * communicate unless the on-board address is overridden
 * (not possible in many manufacture's hardware).
 *
 * NB: setting oldmap to zero completely disables ARP
 *     (i.e. identical to setting IFF_NOARP with an ioctl).
 */
static int	oldmap = 1024;
#endif notdef

/*
 * _init_arp -- cleanup routing table
 */
_init_arp()
{
	bzero(arptab, sizeof(arptab));
}

/*
 * arpclose -- free arp table entries at net device close
 */
static
arpclose()
{
	register struct arptab *at;
	register i;

	at = &arptab[0];
	for (i = 0; i < ARPTAB_SIZE; i++, at++) {
		if (at->at_flags == 0)
			continue;
		arptfree(at);
	}
}

/*
 * Broadcast an ARP packet, asking who has addr on interface ac.
 */
static
arpwhohas(ac, addr)
struct arpcom *ac;
struct in_addr *addr;
{
	register struct mbuf *m;
	register struct ether_header *eh;
	register struct ether_arp *ea;
	register struct arptab *at;
	volatile int tries;
	struct sockaddr sa;
	jmp_buf arp_buf;
	int *old_jmpbuf;
	int old_timer;
	extern int *_timer_jmpbuf;

	/*
	 * must save currently running timer since bfs has one running
	 * when arp is called
	 */
	old_jmpbuf = _timer_jmpbuf;
	old_timer = old_jmpbuf ? _set_timer(0) : 0;

	/*
	 * set timer and try 5 times
	 */
	tries = 0;
	if (setjmp(arp_buf)) {
		if (++tries >= ARP_TRIES) {
			arpclose();
			_io_abort("ARP couldn't resolve network address");
		}
	}

	if ((m = _m_get(M_DONTWAIT, MT_DATA)) == NULL)
		_io_abort("out of mbufs");
	m->m_len = sizeof *ea + sizeof *eh;
	m->m_off = MMAXOFF - m->m_len;
	ea = mtod(m, struct ether_arp *);
	eh = (struct ether_header *)sa.sa_data;
	bzero((caddr_t)ea, sizeof (*ea));
	bcopy((caddr_t)etherbroadcastaddr, (caddr_t)eh->ether_dhost,
	   sizeof (etherbroadcastaddr));
	eh->ether_type = ETHERPUP_ARPTYPE;	/* if_output will swap */
	ea->arp_hrd = htons(ARPHRD_ETHER);
	ea->arp_pro = htons(ETHERPUP_IPTYPE);
	ea->arp_hln = sizeof ea->arp_sha;	/* hardware address length */
	ea->arp_pln = sizeof ea->arp_spa;	/* protocol address length */
	ea->arp_op = htons(ARPOP_REQUEST);
	bcopy((caddr_t)ac->ac_enaddr, (caddr_t)ea->arp_sha,
	   sizeof (ea->arp_sha));
	bcopy((caddr_t)&((struct sockaddr_in *)&ac->ac_if.if_addr)->sin_addr,
	   (caddr_t)ea->arp_spa, sizeof (ea->arp_spa));
	bcopy((caddr_t)addr, (caddr_t)ea->arp_tpa, sizeof (ea->arp_tpa));
	sa.sa_family = AF_UNSPEC;
	(void) (*ac->ac_if.if_output)(&ac->ac_if, m, &sa);

	_set_timer(0);
	_timer_jmpbuf = arp_buf;
	_set_timer(ARP_TIME);
	while (1) {
		_scandevs();
		ARPTAB_LOOK(at, addr->s_addr);
		if (at && (at->at_flags & ATF_COM))
			break;
	}
	_set_timer(0);
	_timer_jmpbuf = old_jmpbuf;
	_set_timer(old_timer);
}

/*
 * Resolve an IP address into an ethernet address.  If success, 
 * desten is filled in and 1 is returned.  If there is no entry
 * in arptab, set one up and broadcast a request 
 * for the IP address;  return 0.
 */
_arpresolve(ac, destip, desten)
struct arpcom *ac;
struct in_addr *destip;
u_char *desten;
{
	register struct arptab *at;
	register struct ifnet *ifp;
	register int lna;

#ifdef DEBUG
	printf("arpresolve 0x%x\n", destip->s_addr);
#endif
	lna = inet_lnaof(*destip);
	if ( in_broadcast(*destip) ) {
		bcopy((caddr_t)etherbroadcastaddr, (caddr_t)desten,
		   sizeof (etherbroadcastaddr));
#ifdef DEBUG
		printf("arpresolve done, broadcast\n");
#endif
		return;
	}

	ifp = &ac->ac_if;
	/* if for us, then error */
	if (destip->s_addr ==
	    ((struct sockaddr_in *)&ifp->if_addr)-> sin_addr.s_addr)
		_io_abort("net destination is local host");

#ifdef notdef
	if (lna >= oldmap) {
		bcopy((caddr_t)ac->ac_enaddr, (caddr_t)desten, 3);
		desten[3] = (lna >> 16) & 0x7f;
		desten[4] = (lna >> 8) & 0xff;
		desten[5] = lna & 0xff;
#ifdef DEBUG
		printf("arpresolve done, no map\n");
#endif
		return;
	}
#endif notdef

	ARPTAB_LOOK(at, destip->s_addr);
	if (at == 0)
		at = arptnew(destip);
	if ((at->at_flags & ATF_COM) == 0)
		arpwhohas(ac, destip);
	bcopy((caddr_t)at->at_enaddr, (caddr_t)desten, 6);
#ifdef DEBUG
	printf("arpresolve done %x.%x.%x.%x.%x.%x\n", desten[0], desten[1],
	    desten[2], desten[3], desten[4], desten[5]);
#endif
}

/*
 * Called from network device recv interrupt routines when ether
 * packet type ETHERPUP_ARP is received.  Algorithm is exactly that
 * given in RFC 826.  In addition, a sanity check is performed on the
 * sender protocol address, to catch impersonators.
 */
_arpinput(ac, m)
struct arpcom *ac;
struct mbuf *m;
{
	register struct ether_arp *ea;
	struct ether_header *eh;
	struct arptab *at = 0;  /* same as "merge" flag */
	struct sockaddr_in sin;
	struct sockaddr sa;
	struct in_addr isaddr,itaddr,myaddr;

#ifdef DEBUG
	printf("arpinput\n");
#endif
	if (m->m_len < sizeof *ea)
		goto out;
	myaddr = ((struct sockaddr_in *)&ac->ac_if.if_addr)->sin_addr;
	ea = mtod(m, struct ether_arp *);
	if (ntohs(ea->arp_pro) != ETHERPUP_IPTYPE)
		goto out;
	bcopy(ea->arp_spa, &isaddr.s_addr, sizeof(isaddr.s_addr));
	bcopy(ea->arp_tpa, &itaddr.s_addr, sizeof(itaddr.s_addr));
	if (!bcmp((caddr_t)ea->arp_sha, (caddr_t)ac->ac_enaddr,
	  sizeof (ac->ac_enaddr)))
		goto out;	/* it's from me, ignore it. */
	if (isaddr.s_addr == myaddr.s_addr) {
		printf("duplicate IP address!! sent from ethernet address: ");
		printf("%x %x %x %x %x %x\n", ea->arp_sha[0], ea->arp_sha[1],
		    ea->arp_sha[2], ea->arp_sha[3],
		    ea->arp_sha[4], ea->arp_sha[5]);
		if (ntohs(ea->arp_op) == ARPOP_REQUEST)
			goto reply;
		goto out;
	}
	ARPTAB_LOOK(at, isaddr.s_addr);
	if (at) {
		bcopy((caddr_t)ea->arp_sha, (caddr_t)at->at_enaddr,
		   sizeof (ea->arp_sha));
		at->at_flags |= ATF_COM;
	}
	if (itaddr.s_addr != myaddr.s_addr)
		goto out;	/* if I am not the target */
	if (at == 0) {		/* ensure we have a table entry */
		at = arptnew(&isaddr);
		bcopy((caddr_t)ea->arp_sha, (caddr_t)at->at_enaddr,
		   sizeof (ea->arp_sha));
		at->at_flags |= ATF_COM;
	}
	if (ntohs(ea->arp_op) != ARPOP_REQUEST)
		goto out;
reply:
#ifdef DEBUG
	printf("arpinput replying\n");
#endif
	bcopy((caddr_t)ea->arp_sha, (caddr_t)ea->arp_tha,
	   sizeof (ea->arp_sha));
	bcopy((caddr_t)ea->arp_spa, (caddr_t)ea->arp_tpa,
	   sizeof (ea->arp_spa));
	bcopy((caddr_t)ac->ac_enaddr, (caddr_t)ea->arp_sha,
	   sizeof (ea->arp_sha));
	bcopy((caddr_t)&myaddr, (caddr_t)ea->arp_spa,
	   sizeof (ea->arp_spa));
	ea->arp_op = htons(ARPOP_REPLY);
	eh = (struct ether_header *)sa.sa_data;
	bcopy((caddr_t)ea->arp_tha, (caddr_t)eh->ether_dhost,
	   sizeof (eh->ether_dhost));
	eh->ether_type = ETHERPUP_ARPTYPE;
	sa.sa_family = AF_UNSPEC;
	(*ac->ac_if.if_output)(&ac->ac_if, m, &sa);
	return;
out:
	_m_freem(m);
	return;
}

/*
 * Free an arptab entry.
 */
static void
arptfree(at)
struct arptab *at;
{
	at->at_iaddr.s_addr = 0;
}

/*
 * Enter a new address in arptab, pushing out the oldest entry 
 * from the bucket if there is no room.
 */
static int lastused_at;

static struct arptab *
arptnew(addr)
struct in_addr *addr;
{
	register n;
	register struct arptab *at, *ato;

	ato = at = &arptab[ARPTAB_HASH(addr->s_addr) * ARPTAB_BSIZ];
	for (n = 0 ; n < ARPTAB_BSIZ ; n++,at++) {
		if (at->at_flags == 0)
			goto out;	 /* found an empty entry */
	}
	if (n >= ARPTAB_BSIZ) {
		at = &ato[lastused_at];
		lastused_at = (lastused_at + 1) % ARPTAB_BSIZ;
		arptfree(at);
	}
out:
	at->at_iaddr = *addr;
	at->at_flags = ATF_INUSE;
	return (at);
}
in_broadcast(iaddr)
struct in_addr iaddr;
{
	unsigned hmask;

	if (IN_CLASSA(iaddr.s_addr)) {
		hmask = IN_CLASSA_HOST;
	} else if (IN_CLASSB(iaddr.s_addr)) {
		hmask = IN_CLASSB_HOST;
	} else if (IN_CLASSC(iaddr.s_addr)) {
		hmask = IN_CLASSC_HOST;
	}
	if ( ((iaddr.s_addr&hmask) == (INADDR_ANY&hmask)) ||
		((iaddr.s_addr&hmask) == (INADDR_BROADCAST&hmask)) ) {
		return(1);
	}
	return(0);
}
