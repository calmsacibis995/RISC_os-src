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
#ident	"$Header: symtab.c,v 1.4.1.4 90/06/05 16:49:38 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions: nm, ds, and ts, as well
 * as the initialization routine rdsymtab.
 */

#define SYMTAB_C 1
#include "crash.h"
#undef SUCCESS
#undef FAILURE
#undef FREAD
#undef FWRITE
#include "ldfcn.h"

extern char *namelist;
struct syment *stbl;			/* symbol table */
int symcnt, symmax;                     /* symbol count */
extern char *malloc(), *realloc();
extern char *ldgetname();
unsigned long startoftext, endoftext;
LDFILE *np;

/* symbol table initialization function */

int symcomp(a,b)
struct syment *a,*b;
{
	return(a->n_value - b->n_value);
}

int
rdsymtab()
{
	FILHDR filehdr;
	SCNHDR scnhdr;
	register struct syment   *sp, *nsp;
	EXTR ep;
	SYMR sy;
	HDRR shdr;
	PDR  apd;
	struct procent *ptbl, *pp;
	int i;

	/*
	 * Open the namelist and associate a stream with it. Read the file into a buffer.
	 * Determine if the file is in the correct format via a magic number check.
	 * An invalid format results in a return to main(). Otherwise, dynamically 
	 * allocate enough space for the namelist. 
	 */

	if(!(np = ldopen(namelist, NULL)))
		fatal("cannot open namelist file\n");
	if(ldfhread(np,&filehdr) != SUCCESS)
		fatal("read error in namelist file\n");
	if(!( ISCOFF(filehdr.f_magic) ||
	      filehdr.f_magic == MIPSEBMAGIC_2 ||
	      filehdr.f_magic == MIPSELMAGIC_2 ))
		fatal("namelist not in a.out format\n");
	ldreadst(np,
/*              ST_PEXTS        |       /* externals */
/*              ST_PSYMS        |       /* symbols */
		ST_PLINES       |       /* lines */
/*              ST_PHEADER      |       /* headers */
/*              ST_PDNS         |       /* dense numbers */
/*              ST_POPTS        |       /* optimization entries */
/*              ST_PRFDS        |       /* file indirect entries */
/*              ST_PSSS         |       /* string space */
		ST_PPDS         |       /* proc descriptors */
/*              ST_PFDS         |       /* file descriptors */
/*              ST_PAUXS        |       /* auxiliaries */
/*              ST_PSSEXTS      |       /* external string space */
		0);
	for (i=1; i<=filehdr.f_nscns; i++) {
		ldshread(np,i,&scnhdr);
		if (scnhdr.s_flags & STYP_TEXT) {
			startoftext = scnhdr.s_vaddr;
			endoftext = startoftext + scnhdr.s_size;
			break;
		}
	}

	fprintf(fp,"reading symboltable"); fflush(fp);
	shdr = SYMHEADER(np);
	symmax = shdr.isymMax+shdr.iextMax;

	if(!(stbl=(struct syment *)malloc((unsigned)(symmax*SYMESZ))))
		fatal("cannot allocate space for namelist\n");
	/*
	 * Find the beginning of the namelist and read in the contents of the list.
	 */

	symcnt = 0;
	/* read the interesting local symbols */
	for(i=0, sp=stbl; i < symmax; i++) {
		if ( i%500 == 0) {
			fputc('.',fp); fflush(fp);
		}
		if(ldtbread(np, i, &sy) == FAILURE)
			fatal("read error in namelist file\n");
		if (sy.st != stGlobal && sy.st != stStatic &&
		   sy.st != stProc && sy.st != stStaticProc)
			continue;
		sp->n_value = sy.value;
		sp->n_scnum = sy.sc;
		sp->n_name = ldgetname(np,&sy);
		sp->n_proc = 0;
		symcnt++;
		sp++;
	}

	qsort(stbl,symcnt,sizeof(*sp),symcomp);
	/* remove duplicates */
	for(sp = nsp = stbl; sp < &stbl[symcnt]; sp++) {
		*nsp++ = *sp;
		while (sp[1].n_value == sp[0].n_value)
			sp++;
	}
	symcnt = nsp-stbl;
	stbl=(struct syment *)realloc((char *)stbl, (unsigned)(symcnt*SYMESZ));

	if(!(ptbl=(struct procent *)malloc((unsigned)(shdr.ipdMax*sizeof(*ptbl)))))
		fatal("cannot allocate space for procent list\n");

	for (i=0,pp=ptbl,sp=stbl; i<shdr.ipdMax; i++,pp++) {
		ldgetpd(np,i,&apd);
		pp->isym = apd.isym;
		pp->iline = apd.iline;
		pp->regmask = apd.regmask;
		pp->regoffset = apd.regoffset;
		pp->frameoffset = apd.frameoffset;
		if (apd.framereg != 29)
			continue;
		if (sp->n_value > apd.adr)
			continue;
		while (sp->n_value < apd.adr)
			sp++;
		sp->n_proc = pp;
#ifdef DEBUG
		printf("adr %8x regmask %8x regoff %5d frameeoff %5d name %s\n",
		apd.adr,pp->regmask,pp->regoffset,pp->frameoffset, sp->n_name);
#endif
	}
	fputc('\n',fp); fflush(fp);
}

/* find symbol */
struct syment *
findsym(addr)
register unsigned long addr;
{
	register struct syment *sp;
	register l,h,i;
	static unsigned long last_addr;
	static struct syment *last_sym;

	if (addr == last_addr)
		return(last_sym);
	l = 0;
	h = symcnt;
	while ((h-l)>1) {
		i = (l+h)/2;
#ifdef DEBUG
printf("l=%5d h=%5d i=%5d nval=%8x addr=%8x\n",l,h,i,stbl[i].n_value,addr);
#endif
		if (stbl[i].n_value <= addr)
			l = i;
		else    h = i;
	}
#ifdef DEBUG
printf("END l=%5d h=%5d i=%5d nval=%8x addr=%8x\n",l,h,i,stbl[l].n_value,addr);
#endif
	sp = &stbl[l];
	last_addr = addr;
	last_sym = sp;
	return(sp);
}

prlineno(pc,sp)
long pc;
register struct  syment  *sp;
{
	int ifd;
	char *fname;
	int lnno;
	extern char *st_str_ifd_iss();

	if (sp->n_proc == 0) {
		fprintf(fp,"%s+%x [0x%x]", sp->n_name, pc-sp->n_value, pc);
		return;
	}
	ifd = ld_ifd_symnum(np, sp->n_proc->isym);
	fname = st_str_ifd_iss(ifd, 1);
	lnno =  np->pchdr->pline[sp->n_proc->iline +
		 ((pc - sp->n_value) / sizeof(long))];
/*      lnno =  SYMTAB(np)->pline[sp->n_proc->iline +
		 ((pc - sp->n_value) / sizeof(long))];
*/
	fprintf(fp,"%s+%x [%s: %d, 0x%x]", sp->n_name, pc-sp->n_value,
		fname, lnno, pc);
}

/* get arguments for ds and ts functions */
int
getsymbol()
{
	int c;

	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		do {prsymbol(args[optind++]);
		}while(args[optind]);
	}
	else longjmp(syn,0);
}

/* print result of ds and ts functions */
int
prsymbol(string)
char *string;
{
	struct syment *spd,*spt,*sp;
	long addr;

	if((addr = strcon(string,'h')) == -1)
		error("\n");
	if(!(sp = findsym((unsigned long)addr))) {
		prerrmes("%s does not match\n",string);
		return;
	}
	if (addr >= startoftext && addr < endoftext && (addr&3) == 0) {
		prlineno(addr,sp);
		fputc('\n', fp);
	} else
		fprintf(fp,"%s + %x\n",sp->n_name, addr - (long)sp->n_value);
}


/* search symbol table */
struct syment *
symsrch(s)
char *s;
{
	register struct syment *sp;
	struct syment *found;
	char *name;

	found = 0;


	for(sp = stbl; sp < &stbl[symcnt]; sp++) {
		if(!strcmp(sp->n_name,s)) {
			found = sp;
			break;
		}
	}
	return(found);
}

/* get arguments for nm function */
int
getnm()
{
	int c;

	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) 
		do { prnm(args[optind++]);
		}while(args[optind]);
	else longjmp(syn,0);
}


/* print result of nm function */
int
prnm(string)
char *string;
{
	char *cp;
	struct syment *sp;

	if(!(sp = symsrch(string))) {
		prerrmes("%s does not match in symbol table\n",string);
		return;
	}
	fprintf(fp,"%s   %08.8lx  ",string,sp->n_value);


	if      (sp -> n_scnum == scText)
		cp = " text";
	else if (sp -> n_scnum == scRData)
		cp = " rdata";
	else if (sp -> n_scnum == scData)
		cp = " data";
	else if (sp -> n_scnum == scSData)
		cp = " sdata";
	else if (sp -> n_scnum == scBss)
		cp = " bss";
	else if (sp -> n_scnum == scSBss)
		cp = " sbss";
	else if (sp -> n_scnum == scUndefined)
		cp = " undefined";
	else if (sp -> n_scnum == scAbs)
		cp = " absolute";
	else
		cp = " type unknown";

	fprintf(fp,"%s\n", cp);
}

#ifdef DEBUG
FILE *fp;
jmp_buf syn;
char *args[1];
int argcnt;
char *namelist;
fatal(){}
redirect(){}
error(){}
prerrmes(){}
strcon(){}
main()
{
	int addr;
	fp = stdout;
	namelist = "a.out";
	rdsymtab();
	for(;;) {
		scanf("%x",&addr);
		findsym(addr);
	}
}
#endif
