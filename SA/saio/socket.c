#ident "$Header: socket.c,v 1.5 90/03/27 18:12:13 zach Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * socket.c -- prom socket routines
 */

#include "sys/types.h"
#include "saio/socket.h"
#include "saio/arp.h"
#include "saio/mbuf.h"

struct so_table _so_table[NSO_TABLE];

#define	NULL	0

_init_sockets()
{
	bzero(_so_table, sizeof(_so_table));
}

struct so_table *
_get_socket()
{
	register struct so_table *st;

	for (st = _so_table; st < &_so_table[NSO_TABLE]; st++)
		if (st->st_count <= 0) {
			st->st_mbuf = 0;
			st->st_count = 1;
			return(st);
		}
	return(NULL);
}

struct so_table *
_find_socket(port)
u_short port;
{
	register struct so_table *st;

	for (st = _so_table; st < &_so_table[NSO_TABLE]; st++) {
		if (st->st_count > 0 && st->st_udpport == port) {
			st->st_count++;
			return(st);
		}
	}
	return(NULL);
}

_free_socket(sx)
unsigned sx;
{
	register struct so_table *st;
	register struct mbuf *m, *n;

	if (sx >= NSO_TABLE)
		goto bad;
	st = &_so_table[sx];
	if (--st->st_count < 0) {
		st->st_count = 0;
bad:
		printf("_free_socket screw-up\n");
	}
	if (m = st->st_mbuf) {
		st->st_mbuf = 0;
		while (m) {
			n = m->m_act;
			_m_freem(m);
			m = n;
		}
	}
	st->st_count = 0; /* allow bootp (multiple sockets) to work */
}

_so_append(st, src_addr, m0)
struct so_table *st;
struct sockaddr *src_addr;
struct mbuf *m0;
{
	register struct mbuf *m;
	register nmbufs;

	m0->m_srcaddr = *src_addr;
	m0->m_act = NULL;
	if ((m = st->st_mbuf) == NULL)
		st->st_mbuf = m0;
	else {
		while (m->m_act) {
			m = m->m_act;
		}
		m->m_act = m0;
	}
	return(1);
}

struct mbuf *
_so_remove(st)
struct so_table *st;
{
	struct mbuf *m;

	if (m = st->st_mbuf)
		st->st_mbuf = m->m_act;
	m->m_act = 0;
	return (m);
}
