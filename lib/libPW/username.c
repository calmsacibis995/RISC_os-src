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
#ident	"$Header: username.c,v 1.5.2.2 90/05/10 01:11:31 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	Gets user's login name.
	Note that the argument must be an integer.
	Returns pointer to login name on success,
	pointer to string representation of used ID on failure.

	Remembers user ID and login name for subsequent calls.
*/

char *
username(uid)
register int uid;
{
	char pw[200];
	static int ouid;
	static char lnam[9], *lptr;
	register int i;

	if (ouid!=uid || ouid==0) {
		if (getpw(uid,pw))
			sprintf(lnam,"%d",uid);
		else {
			for (i=0; i<8; i++)
				if ((lnam[i] = pw[i])==':') break;
			lnam[i] = '\0';
		}
		lptr = lnam;
		ouid = uid;
	}
	return(lptr);
}
