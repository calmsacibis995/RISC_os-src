#ident "$Header: mbuf.c,v 1.4 90/01/16 17:16:42 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * mbuf.c -- prom message buffer routines
 * (Knocked together so kernel network protocol code could be used
 * with minor mods.)
 */

#include "sys/types.h"
#include "saio/socket.h"
#include "saio/arp.h"
#include "saio/mbuf.h"

static struct mbuf *mbuf_free;
/* static struct mbuf mbufs[NMBUFS]; */

struct mbuf *mbufs;			/* Bunch of mbufs */

/*
 * _init_mbufs -- put all mbufs on free list
 */
_init_mbufs()
{
	register i;

	/*
	 * initialize mbuf free list
	 */
	if (!mbufs) {
		if (sizeof(struct mbuf) & 0x3) {
			printf("_init_mbufs: sizeof(mbuf) not long aligned\n");
		return(-1);
		}
	
		mbufs=(struct mbuf *)align_malloc(sizeof(struct mbuf)*NMBUFS,4);
		if (mbufs == 0) {
			printf("_init_mbufs: not enough memory for mbufs\n");
			return(-1);
		}
	}
	mbuf_free = 0;
	for (i = 0; i < NMBUFS; i++)
		_m_freem(&mbufs[i]);
}

/*
 * _m_get -- get a free mbuf
 */
struct mbuf *
_m_get()
{
	register struct mbuf *m;

	if (m = mbuf_free) {
		mbuf_free = m->m_act;
		m->m_act = 0;
		m->m_next = 0;
		m->m_len = 0;
	}
	return (m);
}

/*
 * _m_freem -- put a mbuf on free list
 */
_m_freem(m)
struct mbuf *m;
{
	m->m_act = mbuf_free;
	mbuf_free = m;
}
