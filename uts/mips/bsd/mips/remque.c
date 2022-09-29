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
#ident	"$Header: remque.c,v 1.2.4.2 90/05/10 04:23:38 wje Exp $"

/* VAX insque/remque emulator
 *
 * $Header: remque.c,v 1.2.4.2 90/05/10 04:23:38 wje Exp $
 */


/*
 * generic list struct for use with _insque/_remque
 * any structure that is linked via _insque/_remque should have the
 * list pointers as the first two elements
 */
struct generic_list {
	struct generic_list *link;	/* forward link */
	struct generic_list *rlink;	/* backward link */
};

_insque(ep, pp)
register struct generic_list *ep, *pp;
{
	ep->link = pp->link;
	ep->rlink = pp;
	pp->link->rlink = ep;
	pp->link = ep;
}

_remque(ep)
register struct generic_list *ep;
{
	ep->rlink->link = ep->link;
	ep->link->rlink = ep->rlink;
}
