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
#ident	"$Header: refer3.c,v 1.1.1.2 90/05/07 19:12:43 wje Exp $"
#ifndef lint
static char *sccsid = "@(#)refer3.c	4.2 (Berkeley) 8/26/85";
#endif

#include "refer..c"
#define move(x, y) close(y); dup(x); close(x);

corout(in, out, rprog, arg, outlen)
char *in, *out, *rprog;
{
	int pipev[2], fr1, fr2, fw1, fw2, n;
	int status;

	pipe(pipev); 
	fr1 = pipev[0]; 
	fw1 = pipev[1];
	pipe(pipev); 
	fr2 = pipev[0]; 
	fw2 = pipev[1];
	if (fork() == 0)
	{
		close(fw1); 
		close(fr2);
		move(fr1, 0);
		move(fw2, 1);
		execl(rprog, "deliv", arg, 0);
		err("Can't run %s", rprog);
	}
	close(fw2); 
	close(fr1);
	write(fw1, in , strlen(in));
	close(fw1);
	wait(&status);
	n = read(fr2, out, outlen);
	out[n] = 0;
	close(fr2);
	return(n);
}
