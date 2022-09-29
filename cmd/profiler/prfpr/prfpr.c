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
#ident	"$Header: prfpr.c,v 1.6.2.2 90/05/09 18:18:47 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	prfpr - print profiler log files
 */

#include <stdio.h>
#include <time.h>
#include <a.out.h>
typedef SYMR SYMENT;
#include <ldfcn.h>

#define PRFMAX  2048		/* max number of text symbols */
int N_TEXT;

struct	profile	{
	long	p_date;		/* time stamp of record */
	long	p_ctr[PRFMAX];	/* profiler counter values */
} p[2];


SYMENT *stbl;		/* start of symbol table */
char *strtab;			/* start of string table */
int symcnt;			/* number of symbols */
int prfmax;			/* actual number of text symbols */
int cutoff = 1;
int pc;
int t[PRFMAX];

char *namelist = "/unix";
char *logfile;

long sum, osum;

LDFILE	*ldptr;

main(argc, argv)
char **argv;
{
	register int ff, log, i;
	SYMENT *sp, *search();

	char *sptr[1];
	sptr[0] = (char *) NULL;

	switch(argc) {
		case 4:
			namelist = argv[3];
		case 3:
			cutoff = strtol(argv[2], sptr, 10);
			if (argv[2] == *sptr) {
				printf("prfpr: %s illegal value for cutoff\n",
					argv[2]);
				error("usage: prfpr file [ cutoff [ namelist ] ]");
			}
		case 2:
			logfile = argv[1];
			break;
		default:
			error("usage: prfpr file [ cutoff [ namelist ] ]");
	}
	if((log = open(logfile, 0)) < 0)
		error("cannot open data file");
	if(cutoff > 100 || cutoff < 0)
		error("invalid cutoff percentage");
	if(read(log, &prfmax, sizeof prfmax) != sizeof prfmax || prfmax == 0)
		error("bad data file");
	if(read(log, t, prfmax * sizeof (int)) != prfmax * sizeof (int))
		error("cannot read profile addresses");
	osum = sum = ff = 0;

	read(log, &p[!ff], (prfmax + 2) * sizeof (int));
	for(i = 0; i <= prfmax; i++)
		osum += p[!ff].p_ctr[i];

	rdsymtab();
	rdstrtab();
	for(;;) {
		sum = 0;
		if(read(log, &p[ff], (prfmax + 2) * sizeof (int)) !=
		    (prfmax + 2) * sizeof (int))
			exit(0);
		shtime(&p[!ff].p_date);
		shtime(&p[ff].p_date);
		printf("\n");
		for(i = 0; i <= prfmax; i++)
			sum += p[ff].p_ctr[i];
		if(sum == osum)
			printf("no samples\n\n");
		else for(i = 0; i <= prfmax; i++) {
			pc = 1000 * (p[ff].p_ctr[i] - p[!ff].p_ctr[i]) /
				(sum - osum);
			if(pc > 10 * cutoff)
				if(i == prfmax)
					printf("user     %d.%d\n",
					 pc/10, pc%10);
				else {
					sp = search(t[i], N_TEXT, N_TEXT);
					if(sp == 0)
						printf("unknown  %d.%d\n",
						 pc/10, pc%10);
					else {
						printname(sp);
						printf(" %d.%d\n", pc/10, pc%10);
					}
				}
		}
		ff = !ff;
		osum = sum;
		printf("\n");
	}
}

error(s)
char *s;
{
	printf("error: %s\n", s);
	exit(1);
}

shtime(l)
register long *l;
{
	register  struct  tm  *t;
	struct  tm  *localtime();

	if(*l == (long) 0) {
		printf("initialization\n");
		return;
	}
	t = localtime(l);
	printf("%02.2d/%02.2d/%02.2d %02.2d:%02.2d\n", t->tm_mon + 1,
		t->tm_mday, t->tm_year, t->tm_hour, t->tm_min);
}

rdsymtab()
{
	int	i;

	if((ldptr = ldopen(namelist, ldptr)) == NULL)
		error("cannot open namelist file");
	/* external symbols start at index isymMax and are iextMax long */
	symcnt = SYMHEADER(ldptr).iextMax; 
	if((stbl = (SYMR *) malloc (symcnt * sizeof(SYMR)))
		== NULL)
		error("cannot allocate space for namelist");
	for(i = 0; i < symcnt; i++) {
		if(ldtbread(ldptr, i + SYMHEADER(ldptr).isymMax, &stbl[i]) 
			!= SUCCESS) 
			error("cannot read symbol from namelist");
	}
}

SYMENT *
search(addr, sect1, sect2)
{
	register SYMENT *sp;
	register SYMENT *save;
	unsigned value;

	value = 0;
	save = 0;
	for(sp = stbl; sp < &stbl[symcnt]; sp++) {
		if(sp->st == stProc && sp->sc == scText &&
		  sp->value <= addr && sp->value > value) {
			value = sp->value;
			save = sp;
		}
	}
	return(save);
}

rdstrtab()
{
	/*
	 * String table is not read in, 
	 * but file is left open for use by printname.
	 */
}

printname(ent)
SYMENT *ent;
{
	printf("%-8.8s", ldgetname(ldptr, ent));
}
