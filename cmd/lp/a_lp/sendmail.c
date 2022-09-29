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
#ident	"$Header: sendmail.c,v 1.4.2.2 90/05/09 16:28:21 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* sendmail(user, msg) -- send msg to user's mailbox */

#include	"lp.h"


sendmail(user, msg)
char *user;
char *msg;
{
	FILE *pfile, *popen();
	char mailcmd[LOGMAX + 6];

	if(isnumber(user))
		return;

	sprintf(mailcmd, "mail %s", user);
	if((pfile = popen(mailcmd, "w")) != NULL) {
		fprintf(pfile, "%s\n", msg);
		pclose(pfile);
	}
}

isnumber(s)
char *s;
{
	register c;

	while((c = *(s++)) != '\0')
		if(c < '0' || c > '9')
			return(FALSE);
	return(TRUE);
}
