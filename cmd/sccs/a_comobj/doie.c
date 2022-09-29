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
#ident	"$Header: doie.c,v 1.6.2.2 90/05/09 18:41:04 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"defines.h"


doie(pkt,ilist,elist,glist)
struct packet *pkt;
char *ilist, *elist, *glist;
{
	if (ilist) {
		if (pkt->p_verbose & DOLIST) {
			fprintf(pkt->p_stdout,"================\n");
			fprintf(pkt->p_stdout,"Included:\n");
			dolist(pkt,ilist,INCLUDE);
			fprintf(pkt->p_stdout,"================\n");
		}
		else dolist(pkt,ilist,INCLUDE);
	}
	if (elist) {
		if (pkt->p_verbose & DOLIST) {
			fprintf(pkt->p_stdout,"================\n");
			fprintf(pkt->p_stdout,"Excluded:\n");
			dolist(pkt,elist,EXCLUDE);
			fprintf(pkt->p_stdout,"================\n");
		}
		else dolist(pkt,elist,EXCLUDE);
	}
	if (glist)
		dolist(pkt,glist,IGNORE);
}
