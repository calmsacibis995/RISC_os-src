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
#ident	"$Header: auxf.c,v 1.6.2.2 90/05/09 18:39:44 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"defines.h"


/*
	Figures out names for g-file, l-file, x-file, etc.

	File	Module	g-file	l-file	x-file & rest

	a/s.m	m	m	l.m	a/x.m

	Second argument is letter; 0 means module name is wanted.
*/

char *
auxf(sfile,ch)
register char *sfile;
register char ch;
{
	static char auxfile[FILESIZE];
	register char *snp;

	snp = sname(sfile);

	switch(ch) {

	case 0:
	case 'g':	copy(&snp[2],auxfile);
			break;

	case 'l':	copy(snp,auxfile);
			auxfile[0] = 'l';
			break;

	default:
			copy(sfile,auxfile);
			auxfile[snp-sfile] = ch;
	}
	return(auxfile);
}
