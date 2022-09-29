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
#ident	"$Header: subr.c,v 1.1.3.2 90/05/09 14:56:03 wje Exp $"

extern float obotx;
extern float oboty;
extern float boty;
extern float botx;
extern float scalex;
extern float scaley;
xsc(xi){
	int xa;
	xa = (xi-obotx)*scalex+botx;
	return(xa);
}
ysc(yi){
	int ya;
	ya = (yi-oboty)*scaley+boty;
	return(ya);
}
