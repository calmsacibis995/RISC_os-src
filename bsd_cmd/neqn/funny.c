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
#ident	"$Header: funny.c,v 1.2.1.2 90/05/07 18:54:45 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)funny.c	4.3 8/11/83";
#endif

# include "e.h"
# include "e.def"

funny(n) int n; {
	char *f;

	yyval = oalloc();
	switch(n) {
	case SUM:
		f = "\\(*S"; break;
	case UNION:
		f = "\\(cu"; break;
	case INTER:	/* intersection */
		f = "\\(ca"; break;
	case PROD:
		f = "\\(*P"; break;
	default:
		error(FATAL, "funny type %d in funny", n);
	}
#ifndef NEQN
	printf(".ds %d \\s%d\\v'.3m'\\s+5%s\\s-5\\v'-.3m'\\s%d\n", yyval, ps, f, ps);
	eht[yyval] = VERT( (ps+5)*6 -(ps*6*2)/10 );
	ebase[yyval] = VERT( (ps*6*3)/10 );
#else NEQN
	printf(".ds %d %s\n", yyval, f);
	eht[yyval] = VERT(2);
	ebase[yyval] = 0;
#endif NEQN
	if(dbg)printf(".\tfunny: S%d <- %s; h=%d b=%d\n", 
		yyval, f, eht[yyval], ebase[yyval]);
	lfont[yyval] = rfont[yyval] = ROM;
}
