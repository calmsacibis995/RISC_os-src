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
#ident	"$Header: abbrev.c,v 1.1.1.2 90/05/07 19:27:44 wje Exp $"

#ifndef lint
static char abbrev_sccsid[] = "@(#)abbrev.c	4.2	(Berkeley)	82/11/06";
#endif not lint

struct dict abbrev_d[] = {
"St",'N',
"Dr",'N',
"Drs",'N',
"Mr",'N',
"Mrs",'N',
"Ms",'N',
"Rev",'N',
"No",'Y',
"Nos",'Y',
"NO",'Y',
"NOs",'Y',
"no",'Y',
"Fig",'Y',
"Figs",'Y',
"Dept",'Y',
"Depts",'Y',
"dept",'Y',
"depts",'Y',
"Eq",'Y',
"Eqs",'Y',
"eq",'Y',
"eqs",'Y',
"dB",'Y',
"vs",'P',
"in",'Y',
"ft",'Y',
"yr",'Y',
"ckts",'Y',
"mi",'Y',
"Jr",'J',
"jr",'J',
"Ch",'Y',
"ch",'Y',
"Ref",'Y',
"Refs",'Y',
"ref",'Y',
"refs",'Y',
"Inc",'J',
"Co",'N',
"Corp",'N',
"Jan",'N',
"Feb",'N',
"Mar",'N',
"Apr",'N',
"Jun",'N',
"Aug",'N',
"Sept",'N',
"Oct",'N',
"Nov",'N',
"Dec",'N',
"Sen",'Y',
"Sens",'Y',
"Rep",'Y',
"Hon",'Y',
"Gov",'Y',
"Lt",'Y',
"Col",'Y',
"Comdr",'Y',
"Cmdr",'Y',
"Capt",'Y',
"Calif",'N',
"Ky",'N',
"Va",'N',
0,0
};
