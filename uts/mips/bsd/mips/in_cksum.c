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
#ident	"$Header: in_cksum.c,v 1.2.5.2 90/05/10 04:23:19 wje Exp $"
#include "sys/param.h"
#include "sys/mbuf.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"

/*
 * Checksum routine for Internet Protocol family headers (mips version).
 *
 * This routine is very heavily used in the network
 * code and should be modified for each CPU to be as fast as possible.
 */

int nocksum;

in_cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register int ck;
	unsigned short in_checksum();
	register int mlen;
	register char *addr;
	register rlen;

	if (nocksum)
		return(0);

	ck = rlen = 0;
	while(m) {
		mlen = (m->m_len > len) ? len : m->m_len;
		addr = mtod(m, char *);

		if ((rlen^(int)addr)&1)
			ck = nuxi_s(in_checksum(addr, mlen, nuxi_s(ck)));
		else
			ck = in_checksum(addr, mlen, ck);

		rlen += mlen;
		len -= mlen;
		if (len == 0)
			break;
		while (m = m->m_next)
			if (m->m_len)
				break;
	}
	if (len)
		printf("in_cksum, ran out of data, %d bytes left\n", len);

	return(~ck & 0xFFFF);
}
