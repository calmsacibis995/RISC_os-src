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
#ident	"$Header: msqrt.c,v 1.1.2.2 90/05/07 21:42:43 wje Exp $"

/*	@(#)msqrt.c	4.1	12/25/82	*/

#include <mp.h>
msqrt(a,b,r) MINT *a,*b,*r;
{	MINT x,junk,y;
	int j;
	x.len=junk.len=y.len=0;
	if(a->len<0) fatal("msqrt: neg arg");
	if(a->len==0)
	{	b->len=0;
		r->len=0;
		return(0);
	}
	if(a->len%2==1) x.len=(1+a->len)/2;
	else x.len=1+a->len/2;
	x.val=xalloc(x.len,"msqrt");
	for(j=0;j<x.len;x.val[j++]=0);
	if(a->len%2==1) x.val[x.len-1]=0400;
	else x.val[x.len-1]=1;
	xfree(b);
	xfree(r);
loop:
	mdiv(a,&x,&y,&junk);
	xfree(&junk);
	madd(&x,&y,&y);
	sdiv(&y,2,&y,(short *)&j);
	if(mcmp(&x,&y)>0)
	{	xfree(&x);
		move(&y,&x);
		xfree(&y);
		goto loop;
	}
	xfree(&y);
	move(&x,b);
	mult(&x,&x,&x);
	msub(a,&x,r);
	xfree(&x);
	return(r->len);
}
