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
#ident	"$Header: xdr_mbuf.c,v 1.2.1.2 90/05/07 21:03:53 wje Exp $"
/*
 * @(#)xdr_mbuf.c 1.3  88/07/27 4.0NFSSRC Copyright Sun Microsystems, Inc.
 * @(#)xdr_mbuf.c 1.22 88/02/08           Copyright Sun Microsystems, Inc.
 *
 * xdr_mbuf.c, XDR implementation on kernel mbufs.
 */

#ifdef KERNEL
#include "sys/param.h"
#include "sys/mbuf.h"
#include "../rpc/types.h"
#include "../rpc/xdr.h"
#include "bsd/netinet/in.h"
#include "sys/uio.h"
#else
#include <rpc/types.h>
#include <rpc/xdr.h>
#ifdef SYSTYPE_BSD43
#include <sys/param.h>
#include <sys/mbuf.h>
#include <netinet/in.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/sys/param.h>
#include <bsd/sys/mbuf.h>
#include <bsd/netinet/in.h>
#endif
#include <sys/uio.h>
#endif

bool_t	xdrmbuf_getlong(), xdrmbuf_putlong();
bool_t	xdrmbuf_getbytes(), xdrmbuf_putbytes();
u_int	xdrmbuf_getpos();
bool_t	xdrmbuf_setpos();
long *	xdrmbuf_inline();
void	xdrmbuf_destroy();

/*
 * Xdr on mbufs operations vector.
 */
struct	xdr_ops xdrmbuf_ops = {
	xdrmbuf_getlong,
	xdrmbuf_putlong,
	xdrmbuf_getbytes,
	xdrmbuf_putbytes,
	xdrmbuf_getpos,
	xdrmbuf_setpos,
	xdrmbuf_inline,
	xdrmbuf_destroy
};

/*
 * Initailize xdr stream.
 */
void
xdrmbuf_init(xdrs, m, op)
	register XDR		*xdrs;
	register struct mbuf	*m;
	enum xdr_op		 op;
{

	xdrs->x_op = op;
	xdrs->x_ops = &xdrmbuf_ops;
	xdrs->x_base = (caddr_t)m;
	xdrs->x_private = mtod(m, caddr_t);
	xdrs->x_public = (caddr_t)0;
	xdrs->x_handy = m->m_len;
}

void
/* ARGSUSED */
xdrmbuf_destroy(xdrs)
	XDR	*xdrs;
{
	/* do nothing */
}

bool_t
xdrmbuf_getlong(xdrs, lp)
	register XDR	*xdrs;
	long		*lp;
{

	if ((xdrs->x_handy -= sizeof(long)) < 0) {
		if (xdrs->x_handy != -sizeof(long))
			printf("xdr_mbuf: long crosses mbufs!\n");
		if (xdrs->x_base) {
			register struct mbuf *m =
			    ((struct mbuf *)xdrs->x_base)->m_next;

			xdrs->x_base = (caddr_t)m;
			if (m == NULL)
				return (FALSE);
			xdrs->x_private = mtod(m, caddr_t);
			xdrs->x_handy = m->m_len - sizeof(long);
		} else {
			return (FALSE);
		}
	}
	*lp = ntohl(*((long *)(xdrs->x_private)));
	xdrs->x_private += sizeof(long);
	return (TRUE);
}

bool_t
xdrmbuf_putlong(xdrs, lp)
	register XDR	*xdrs;
	long		*lp;
{

	if ((xdrs->x_handy -= sizeof(long)) < 0) {
		if (xdrs->x_handy != -sizeof(long))
			printf("xdr_mbuf: putlong, long crosses mbufs!\n");
		if (xdrs->x_base) {
			register struct mbuf *m =
			    ((struct mbuf *)xdrs->x_base)->m_next;

			xdrs->x_base = (caddr_t)m;
			if (m == NULL)
				return (FALSE);
			xdrs->x_private = mtod(m, caddr_t);
			xdrs->x_handy = m->m_len - sizeof(long);
		} else {
			return (FALSE);
		}
	}
	*(long *)xdrs->x_private = htonl(*lp);
	xdrs->x_private += sizeof(long);
	return (TRUE);
}

bool_t
xdrmbuf_getbytes(xdrs, addr, len)
	register XDR	*xdrs;
	caddr_t		 addr;
	register u_int	 len;
{

	while ((xdrs->x_handy -= len) < 0) {
		if ((xdrs->x_handy += len) > 0) {
			bcopy(xdrs->x_private, addr, (u_int)xdrs->x_handy);
			addr += xdrs->x_handy;
			len -= xdrs->x_handy;
		}
		if (xdrs->x_base) {
			register struct mbuf *m =
			    ((struct mbuf *)xdrs->x_base)->m_next;

			xdrs->x_base = (caddr_t)m;
			if (m == NULL)
				return (FALSE);
			xdrs->x_private = mtod(m, caddr_t);
			xdrs->x_handy = m->m_len;
		} else {
			return (FALSE);
		}
	}
	bcopy(xdrs->x_private, addr, (u_int)len);
	xdrs->x_private += len;
	return (TRUE);
}

/*
 * Sort of like getbytes except that instead of getting
 * bytes we return the mbuf that contains all the rest
 * of the bytes.
 */
bool_t
xdrmbuf_getmbuf(xdrs, mm, lenp)
	register XDR	*xdrs;
	register struct mbuf **mm;
	register u_int *lenp;
{
	register struct mbuf *m;
	register int len, used;

	if (! xdr_u_int(xdrs, lenp)) {
		return (FALSE);
	}
	m = (struct mbuf *)xdrs->x_base;
	used = m->m_len - xdrs->x_handy;
	m->m_off += used;
	m->m_len -= used;
	*mm = m;
	/*
	 * Consistency check.
	 */
	len = 0;
	while (m) {
		len += m->m_len;
		m = m->m_next;
	}
	if (len < *lenp) {
#ifdef KERNEL
		printf("xdrmbuf_getmbuf failed\n");
#endif
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdrmbuf_putbytes(xdrs, addr, len)
	register XDR	*xdrs;
	caddr_t		 addr;
	register u_int		 len;
{

	while ((xdrs->x_handy -= len) < 0) {
		if ((xdrs->x_handy += len) > 0) {
			bcopy(addr, xdrs->x_private, (u_int)xdrs->x_handy);
			addr += xdrs->x_handy;
			len -= xdrs->x_handy;
		}
		if (xdrs->x_base) {
			register struct mbuf *m =
			    ((struct mbuf *)xdrs->x_base)->m_next;

			xdrs->x_base = (caddr_t)m;
			if (m == NULL)
				return (FALSE);
			xdrs->x_private = mtod(m, caddr_t);
			xdrs->x_handy = m->m_len;
		} else {
			return (FALSE);
		}
	}
	bcopy(addr, xdrs->x_private, len);
	xdrs->x_private += len;
	return (TRUE);
}

/*
 * Like putbytes, only we avoid the copy by pointing a type 2
 * mbuf at the buffer.  Not safe if the buffer goes away before
 * the mbuf chain is deallocated.
 */
#ifdef RISCOS
bool_t
xdrmbuf_putbuf(xdrs, addr, len, func, arg, dfunc, darg)
#else
bool_t
xdrmbuf_putbuf(xdrs, addr, len, func, arg)
#endif
	register XDR	*xdrs;
	caddr_t		 addr;
	u_int		 len;
	int		 (*func)();
	int		 arg;
#ifdef RISCOS
	int		 (*dfunc)();
	int		 darg;
#endif
{
	register struct mbuf *m;
	struct mbuf *mclgetx();
	long llen = len;

	if (len & 3) {  /* can't handle roundup problems */
		return (FALSE);
	}
	if (! xdrmbuf_putlong(xdrs, &llen)) {
		return(FALSE);
	}
	((struct mbuf *)xdrs->x_base)->m_len -= xdrs->x_handy;
#ifdef RISCOS
	m = mclgetx(func, arg, dfunc, darg, addr, (int)len, M_WAIT);
#else
	m = mclgetx(func, arg, addr, (int)len, M_WAIT);
#endif
	if (m == NULL) {
		printf("xdrmbuf_putbuf: mclgetx failed\n");
		return (FALSE);
	}
	((struct mbuf *)xdrs->x_base)->m_next = m;
	xdrs->x_handy = 0;
	return (TRUE);
}

u_int
xdrmbuf_getpos(xdrs)
	register XDR	*xdrs;
{

	return (
	    (u_int)xdrs->x_private - mtod(((struct mbuf *)xdrs->x_base), u_int));
}

bool_t
xdrmbuf_setpos(xdrs, pos)
	register XDR	*xdrs;
	u_int		 pos;
{
	register caddr_t	newaddr =
	    mtod(((struct mbuf *)xdrs->x_base), caddr_t) + pos;
	register caddr_t	lastaddr =
	    xdrs->x_private + xdrs->x_handy;

	if ((int)newaddr > (int)lastaddr)
		return (FALSE);
	xdrs->x_private = newaddr;
	xdrs->x_handy = (int)lastaddr - (int)newaddr;
	return (TRUE);
}

long *
xdrmbuf_inline(xdrs, len)
	register XDR	*xdrs;
	int		 len;
{
	long *buf = 0;

	if (xdrs->x_handy >= len) {
		xdrs->x_handy -= len;
		buf = (long *) xdrs->x_private;
		xdrs->x_private += len;
	}
	return (buf);
}
