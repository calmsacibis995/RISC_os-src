/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: prfld.c,v 1.5.2.2.1.1.1.2 90/10/05 10:06:08 beacker Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	prfld - load profiler with sorted kernel text addresses
 */

#include <stdio.h>
#include <sys/errno.h>
#include <a.out.h>
typedef SYMR SYMENT;
#include <ldfcn.h>

#define PRFMAX	2048		/* maximum number of symbols */
int N_TEXT;
int symcnt;			/* number of symbols */
SYMENT *stbl;			/* start of symbol table */
char *namelist = "/unix";	/* namelist file */
extern int errno;

main(argc, argv)
char **argv;
{
	register int *ip, prf;
	register SYMENT *sp;
	int taddr[PRFMAX], ntaddr;
	int compar();
	int	ai;
	char	**ap;
	int	unique_addr = 0;
	int	truncate_list = 0;

	ai = argc - 1;
	ap = argv + 1;
	while (ai > 0) {
		if (! strcmp(*ap,"-U")) {
			ai--;
			ap++;
			unique_addr = 1;
		} else if (! strcmp(*ap,"-T")) {
			ai--;
			ap++;
			truncate_list = 1;
		} else
			break;
	};
	if (ai > 0) {
		namelist = *ap;
		ai--;
		ap++;
	};
	if (ai != 0)
		error("usage: prfld [-U] [-T] [/unix]");
	if((prf = open("/dev/prf", 1)) < 0)
		error("cannot open /dev/prf");
	rdsymtab();
	ip = taddr;
	*ip++ = 0;
	for(sp = stbl; --symcnt; sp++) {
		if(ip >= &taddr[PRFMAX]) {
			if (truncate_list)
				break;
			error("too many text symbols");
		};
		if(sp->sc == scText && sp->st== stProc) {
			if (unique_addr) {
				for (ai = 0; (taddr + ai) < ip; ai++)
					if (taddr[ai] == sp->value)
						break;
				if ((taddr + ai) == ip)
					*ip++ = sp->value;
			} else				
				*ip++ = sp->value;
		};
	}
	ntaddr = ip - taddr;
	qsort(taddr, ntaddr, sizeof (int), compar);
	if(write(prf, taddr, ntaddr*sizeof(int)) != ntaddr*sizeof(int))
		switch(errno) {
		case ENOSPC:
			error("insufficient space in system for addresses");
		case E2BIG:
			error("unaligned data or insufficient addresses");
		case EBUSY:
			error("profiler is enabled");
		case EINVAL:
			error("text addresses not sorted properly");
		default:
			error("cannot load profiler addresses");
		}
	exit(0);
}

compar(x, y)
	register  unsigned  *x, *y;
{
	if(*x > *y)
		return(1);
	else if(*x == *y)
		return(0);
	return(-1);
}

error(s)
char *s;
{
	printf("error: %s\n", s);
	exit(1);
}

rdsymtab()
{
	LDFILE	*ldptr = NULL;
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
	if(ldclose(ldptr) != SUCCESS)
		error("cannot ldclose namelist file");
}
