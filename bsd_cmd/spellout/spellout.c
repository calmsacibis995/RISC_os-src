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
#ident	"$Header: spellout.c,v 1.1.2.2 90/05/07 19:18:55 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)spellout.c	4.1 12/18/82";
#endif

#include "spell.h"

main(argc, argv)
char **argv;
{
	register i, j;
	long h;
	register long *lp;
	char word[NW];
	int dflag = 0;
	int indict;
	register char *wp;

	if (argc>1 && argv[1][0]=='-' && argv[1][1]=='d') {
		dflag = 1;
		argc--;
		argv++;
	}
	if(argc<=1) {
		fprintf(stderr,"spellout: arg count\n");
		exit(1);
	}
	if(!prime(argc,argv)) {
		fprintf(stderr,
		    "spellout: cannot initialize hash table\n");
		exit(1);
	}
	while (fgets(word, sizeof(word), stdin)) {
		indict = 1;
		for (i=0; i<NP; i++) {
			for (wp = word, h = 0, lp = pow2[i];
				(j = *wp) != '\0'; ++wp, ++lp)
				h += j * *lp;
			h %= p[i];
			if (get(h)==0) {
				indict = 0;
				break;
			}
		}
		if (dflag == indict) {
			fputs(word, stdout);
			if (word[strlen(word) - 1] != '\n') {
				putc('\n', stdout);
			}
		}
	}
	exit(0);
}
