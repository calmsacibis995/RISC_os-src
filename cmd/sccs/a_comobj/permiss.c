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
#ident	"$Header: permiss.c,v 1.7.2.2 90/05/09 18:42:14 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"defines.h"


finduser(pkt)
register struct packet *pkt;
{
	register char *p;
	char *user, *logname();
	char *strend(), *getline();
	char groupid[6];
	int none;
	int ok_user;

	none = 1;
	user = logname();
	sprintf(groupid,"%d",getgid());
	while ((p = getline(pkt)) != NULL && *p != CTLCHAR) {
		none = 0;
		ok_user = 1;
		repl(p,'\n','\0');	/* this is done for equal test below */
		if(*p == '!') {
			++p;
			ok_user = 0;
			}
		if (!pkt->p_user)
			if (equal(user,p) || equal(groupid,p))
				pkt->p_user = ok_user;
		*(strend(p)) = '\n';	/* repl \0 end of line w/ \n again */
	}
	if (none)
		pkt->p_user = 1;
	if (p == NULL || p[1] != EUSERNAM)
		fmterr(pkt);
}


char	*Sflags[NFLAGS] = { 0 } ;	/* 0 required to force loading */

doflags(pkt)
struct packet *pkt;
{
	register char *p;
	register int k;
	char *getline(), *fmalloc();

	for (k = 0; k < NFLAGS; k++)
		Sflags[k] = 0;
	while ((p = getline(pkt)) != NULL && *p++ == CTLCHAR && *p++ == FLAG) {
		NONBLANK(p);
		k = *p++ - 'a';
		NONBLANK(p);
		Sflags[k] = fmalloc(size(p));
		copy(p,Sflags[k]);
		for (p = Sflags[k]; *p++ != '\n'; )
			;
		*--p = 0;
	}
}


permiss(pkt)
register struct packet *pkt;
{
	extern char *Sflags[];
	register char *p;
	register int n;

	if (!pkt->p_user)
		fatal("not authorized to make deltas (co14)");
	if (p = Sflags[FLORFLAG - 'a']) {
		if (((unsigned)pkt->p_reqsid.s_rel) < (n = patoi(p))) {
			sprintf(Error,"release %u < %u (floor) (co15)",
				pkt->p_reqsid.s_rel,n);
			fatal(Error);
		}
	}
	if (p = Sflags[CEILFLAG - 'a']) {
		if (((unsigned)pkt->p_reqsid.s_rel) > (n = patoi(p))) {
			sprintf(Error,"release %u > %u (ceiling) (co16)",
				pkt->p_reqsid.s_rel,n);
			fatal(Error);
		}
	}
	/*
	check to see if the file or any particular release is
	locked against editing. (Only if the `l' flag is set)
	*/
	if ((p = Sflags[LOCKFLAG - 'a']))
		ck_lock(p,pkt);
}



char	l_str[]    =    "SCCS file locked against editing (co23)";
ck_lock(p,pkt)
register char *p;
register struct packet *pkt;
{
	int l_rel;
	int locked;

	locked = 0;
	if (*p == 'a')
		locked++;
	else while(*p) {
		p = satoi(p,&l_rel);
		++p;
		if (l_rel == pkt->p_gotsid.s_rel || l_rel == pkt->p_reqsid.s_rel) {
			locked++;
			sprintf(l_str,"release `%d' locked against editing (co23)",
				l_rel);
			break;
		}
	}
	if (locked)
		fatal(l_str);
}
