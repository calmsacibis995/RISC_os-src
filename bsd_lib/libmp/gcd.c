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
#ident	"$Header: gcd.c,v 1.1.2.2 90/05/07 21:42:21 wje Exp $"

/*	@(#)gcd.c	4.1	12/25/82	*/

#include <mp.h>
gcd(a,b,c) MINT *a,*b,*c;
{	MINT x,y,z,w;
	x.len=y.len=z.len=w.len=0;
	move(a,&x);
	move(b,&y);
	while(y.len!=0)
	{	mdiv(&x,&y,&w,&z);
		move(&y,&x);
		move(&z,&y);
	}
	move(&x,c);
	xfree(&x);
	xfree(&y);
	xfree(&z);
	xfree(&w);
	return;
}
invert(a, b, c) MINT *a, *b, *c;
{	MINT x, y, z, w, Anew, Aold;
	int i = 0;
	x.len = y.len = z.len = w.len = Aold.len = 0;
	Anew.len = 1;
	Anew.val = xalloc(1, "invert");
	*Anew.val = 1;
	move(b, &x);
	move(a, &y);
	while(y.len != 0)
	{	mdiv(&x, &y, &w, &z);
		move(&Anew, &x);
		mult(&w, &Anew, &Anew);
		madd(&Anew, &Aold, &Anew);
		move(&x, &Aold);
		move(&y, &x);
		move(&z, &y);
		i++;
	}
	move(&Aold, c);
	if( (i&01) == 0) msub(b, c, c);
	xfree(&x);
	xfree(&y);
	xfree(&z);
	xfree(&w);
	xfree(&Aold);
	xfree(&Anew);
}
