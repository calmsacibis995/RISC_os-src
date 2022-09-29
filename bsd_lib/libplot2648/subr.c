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
#ident	"$Header: subr.c,v 1.1.2.2 90/05/07 21:47:55 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	5.1 (Berkeley) 5/7/85";
#endif not lint

#include <sgtty.h>
#include "hp2648.h"

handshake()
{
	int i;
	char ch;

	if( shakehands != TRUE )
		return;
	ch = ' ';
	putchar(ENQ);
	fflush(stdout);
	while(1){
		i = read(fildes, &ch, 1);
		if(i < 0)
			continue;
		if(ch == ACK)
			break;
		putchar('Z');
		fflush(stdout);
		stty(fildes, &sarg);
		exit(0);
	}
}

buffready(n)
int n;
{
	buffcount = buffcount + n;
	if(buffcount >= 80){
		handshake();
		putchar(ESC); 
		putchar(GRAPHIC);
		putchar(PLOT);
		putchar(BINARY);
		buffcount = n+4;
	}
}

itoa(num,byte1,byte2)
int num;
char *byte1,*byte2;
{
	*byte1 = (num & 037) | 040;
	*byte2 = ((num>>5) & 037) | 040;
}
