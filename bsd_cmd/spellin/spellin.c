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
#ident	"$Header: spellin.c,v 1.1.2.2 90/05/07 19:18:34 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)spellin.c	4.1 12/18/82";
#endif

#include "spell.h"
/* add entries to hash table for use by spell
   preexisting hash table is first argument
   words to be added are standard input
   if no hash table is given, create one from scratch
*/

main(argc,argv)
char **argv;
{
	register i, j;
	long h;
	register long *lp;
	char word[NW];
	register char *wp;

	if(!prime(argc,argv)) {
		fprintf(stderr,
		    "spellin: cannot initialize hash table\n");
		exit(1);
	}
	while (fgets(word, sizeof(word), stdin)) {
		for (i=0; i<NP; i++) {
			for (wp = word, h = 0, lp = pow2[i];
				 (j = *wp) != '\0'; ++wp, ++lp)
				h += j * *lp;
			h %= p[i];
			set(h);
		}
	}
#ifdef gcos
	freopen((char *)NULL, "wi", stdout);
#endif
	if (fwrite((char *)tab, sizeof(*tab), TABSIZE, stdout) != TABSIZE) {
		fprintf(stderr,
		    "spellin: trouble writing hash table\n");
		exit(1);
	}
	return(0);
}
