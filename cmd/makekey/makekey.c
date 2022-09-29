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
#ident	"$Header: makekey.c,v 1.6.2.2 90/05/09 16:45:49 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <stdio.h>

/*
 * You send it 10 bytes.
 * It sends you 13 bytes.
 * The transformation is expensive to perform
 * (a significant part of a second).
 */

char	*crypt();

main(argc)
	int argc;
{
	char key[8];
	char salt[2];

	if (argc != 1) {
		fprintf(stderr, "usage: makekey\n");
		exit(2);
	}
	
	read(0, key, 8);
	read(0, salt, 2);
	write(1, crypt(key, salt), 13);
	return(0);
}
