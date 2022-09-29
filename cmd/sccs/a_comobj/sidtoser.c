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
#ident	"$Header: sidtoser.c,v 1.6.2.2 90/05/09 18:42:51 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"defines.h"


sidtoser(sp,pkt)
register struct sid *sp;
struct packet *pkt;
{
	register int n;
	register struct idel *rdp;

	for (n = maxser(pkt); n; n--) {
		rdp = &pkt->p_idel[n];
		if (rdp->i_sid.s_rel == sp->s_rel &&
			rdp->i_sid.s_lev == sp->s_lev &&
			rdp->i_sid.s_br == sp->s_br &&
			rdp->i_sid.s_seq == sp->s_seq)
				break;
	}
	return(n);
}
