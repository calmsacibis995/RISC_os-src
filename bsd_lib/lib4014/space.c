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
#ident	"$Header: space.c,v 1.1.2.2 90/05/07 20:19:39 wje Exp $"

extern float botx;
extern float boty;
extern float obotx;
extern float oboty;
extern float scalex;
extern float scaley;
extern int scaleflag;
space(x0,y0,x1,y1){
	botx = 0.;
	boty = 0.;
	obotx = x0;
	oboty = y0;
	if(scaleflag)
		return;
	scalex = 3120./(x1-x0);
	scaley = 3120./(y1-y0);
}
