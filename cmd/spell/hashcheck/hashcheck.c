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
#ident	"$Header: hashcheck.c,v 1.6.2.2 90/05/09 19:03:49 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <stdio.h>
#include "hash.h"
long fetch();
int index[NI];
unsigned *table;
unsigned wp;
int bp;
#define U (BYTE*sizeof(unsigned))
#define L (BYTE*sizeof(long))

main()
{
	int i;
	long v;
	long a;
	extern char *malloc();
	rhuff(stdin);
	fread((char*)index, sizeof(*index), NI, stdin);
	table = (unsigned*)malloc(index[NI-1]*sizeof(*table));
	fread((char*)table, sizeof(*table), index[NI-1], stdin);
	for(i=0;i<NI-1;i++) {
		bp = U;
		v = (long)i<<(HASHWIDTH-INDEXWIDTH);
		if (index[i] > NI) {
			fprintf(stderr, "hashcheck: Malformed input\n");
			exit(0);
		}
		for(wp=index[i];wp<index[i+1]; ) {
			if(wp==index[i]&&bp==U)
				a = fetch();
			else {
				a = fetch();
				if(a==0)
					break;
			}
			if(wp>index[i+1]||
				wp==index[i+1]&&bp<U)
				break;
			v += a;
			printf("%.9lo\n",v);
		}
	}
}

long fetch()
{
	long w;
	long y = 0;
	int empty = L;
	int i = bp;
	int tp = wp;
	while(empty>=i) {
		empty -= i;
		i = U;
		y |= (long)table[tp++] << empty;
	}
	if(empty>0)
		y |= table[tp]>>i-empty;
	i = decode((y>>1)&((1L<<(BYTE*sizeof(y)-1))-1), &w);
	bp -= i;
	while(bp<=0) {
		bp += U;
		wp++;
	}
	return(w);
}
