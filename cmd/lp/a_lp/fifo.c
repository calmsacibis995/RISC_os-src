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
#ident	"$Header: fifo.c,v 1.4.2.2 90/05/09 16:26:03 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* enqueue(cmd, args) -- writes command and args for scheduler on FIFO */

#include	"lp.h"


static FILE *fifo = NULL;

static int
sig14()
{
	signal(SIGALRM, sig14);
}

enqueue(cmd, args)
char cmd;
char *args;
{
	int i = 0;
	unsigned alarm();

	if(eaccess(FIFO, ACC_W) == 0) {
		if(fifo == NULL) {
			sig14();
			alarm(ALTIME);
			fifo = fopen(FIFO, "w");
			alarm(0);
		}
		if(fifo != NULL) {
			alarm(ALTIME);
			i = fprintf(fifo, "%c %s\n", cmd, args);
			alarm(0);
			if(i != 0)
				fflush(fifo);
			else
				fifo = NULL;
		}
	}
	else
		fifo = NULL;

	return(fifo != NULL);
}
