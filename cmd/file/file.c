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
#ident	"$Header: file.c,v 1.8.2.2 90/05/09 15:50:08 wje Exp $"

/*	@(#)file.c	1.9	*/	/* DAG -- removed embedded ESC */

#include	<stdio.h>
#include	<ctype.h>
/* #include	<signal.h>	/* DAG -- unused */
#include	<sys/param.h>
#ifndef makedev			/* defined in sys/types.h	*/
#include	<sys/types.h>
#endif
#include	<sys/sysmacros.h>
#include	<sys/stat.h>

/*
**	Types
*/

#define	BYTE	0
#define	SHORT	2
#define	LONG	4
#define	STR	8

/*
**	Opcodes
*/

#define	EQ	0
#define	GT	1
#define	LT	2
#define	STRC	3	/* string compare */
#define	ANY	4
#define NEQ	5	/* not equal (!) */
#define	SUB	64	/* or'ed in */

/*
**	Misc
*/

#define	NENT	500
#define	BSZ	128
#define	FBSZ	512
#define	reg	register

/* Ass lang comment char */
#ifdef pdp11
#define ASCOMCHAR '/'
#else
#define ASCOMCHAR '#'
#endif
/*
**	Structure of magic file entry
*/

struct	entry	{
	char	e_level;	/* 0 or 1 */
	long	e_off;		/* in bytes */
	char	e_type;
	char	e_opcode;
	union	{
		long	num;
		char	*str;
	}	e_value;
	char	*e_str;
};

typedef	struct entry	Entry;

Entry	*mtab;
char	fbuf[FBSZ];
#if BRL
#if pdp11
char	*mfile = "/usr/lib/magic";
#else
char	*mfile = "/usr/5lib/magic";
#endif
#else	/* !BRL */
char	*mfile = "/etc/magic";
#endif
char	*troff[] = {	/* DAG -- added for ditroff intermediate language */
	"x","T","res","init","font","V0","p1",0};
char	*fort[] = {
	"function","subroutine","common","dimension","block","integer",
	"real","data","double",0};
char	*asc[] = {
#if vax
	"chmk",
#else
#if gould
	"svc",
#else
	"sys",
#endif
#endif
	"mov","tst","clr","jmp",0};
char	*c[] = {
	"int","char","float","double","struct","extern","static",0};
char	*as[] = {
	"globl","byte","even","text","data","bss","comm",0};
char	*strchr();
char	*malloc();
long	atolo();
int	i = 0;
int	fbsz;
int	ifd = -1;	/* DAG -- initialize < 0 */

#define	prf(x)	printf("%s:%s", x, strlen(x)>6 ? "\t" : "\t\t");

main(argc, argv)
char **argv;
{
	reg	char	*p;
	reg	int	ch;
	reg	FILE	*fl;
	reg	int	cflg = 0, eflg = 0, fflg = 0;
	auto	char	ap[128];
	extern	int	optind;
	extern	char	*optarg;

	while((ch = getopt(argc, argv, "cf:m:")) != EOF)
	switch(ch) {
	case 'c':
		cflg++;
		break;

	case 'f':
		fflg++;
		if ((fl = fopen(optarg, "r")) == NULL) {
			fprintf(stderr, "cannot open %s\n", optarg);
			goto use;
		}
		break;

	case 'm':
		mfile = optarg;
		break;

	case '?':
		eflg++;
		break;
	}
	if(!cflg && !fflg && (eflg || optind == argc)) {
use:
		fprintf(stderr,
			"usage: file [-c] [-f ffile] [-m mfile] file...\n");
		exit(2);
	}
	if(cflg) {
		reg	Entry	*ep;

		mkmtab(1);
		printf("level	off	type	opcode	value	string\n");
		for(ep = mtab; ep->e_off != -1L; ep++) {
			printf("%d\t%d\t%d\t%d\t", ep->e_level, ep->e_off,
				ep->e_type, ep->e_opcode);
			if(ep->e_type == STR)
				printf("%s\t", ep->e_value.str);
			else
				printf("%lo\t", ep->e_value.num);
			printf("%s", ep->e_str);
			if(ep->e_opcode & SUB)
				printf("\tsubst");
			printf("\n");
		}
		exit(0);
	}
	for(; fflg || optind < argc; optind += !fflg) {
		reg	int	l;

		if(fflg) {
			if((p = fgets(ap, 128, fl)) == NULL) {
				fflg = 0;
				optind--;
				continue;
			}
			l = strlen(p);
			if(l > 0)
				p[l - 1] = '\0';
		} else
			p = argv[optind];
		prf(p);
		type(p);
		if(ifd >= 0)	/* DAG -- bug fix, was != 0 */
			close(ifd);
	}
	exit(0);
}

type(file)
char	*file;
{
	int	j,nl;
	char	ch;
	struct	stat	mbuf;

	ifd = -1;
	/*
	 * Check for symbolic link.
	 */
	if(lstat(file, &mbuf) < 0) {
		printf("cannot open\n");
		return;
	}

	if ((mbuf.st_mode & S_IFMT) == S_IFLNK) {
		printf("symbolic link to ");
		if(stat(file, &mbuf) < 0) {
			printf("nonexistent filename\n");
			return;
		}
	}
	switch (mbuf.st_mode & S_IFMT) {
	case S_IFCHR:
		printf("character");
		goto spcl;

	case S_IFDIR:
		printf("directory\n");
		return;

#ifdef S_IFIFO
	case S_IFIFO:
		printf("fifo\n");
		return;
#endif

#ifdef	S_IFSOCK
	case S_IFSOCK:	/* DAG -- added */
		printf("socket\n");
		return;
#endif

	case S_IFBLK:
		printf("block");

spcl:
		printf(" special (%d/%d)\n", major(mbuf.st_rdev),
							minor(mbuf.st_rdev));
		return;
	}
	ifd = open(file, 0);
	if(ifd < 0) {
		printf("cannot open for reading\n");
		return;
	}
	fbsz = read(ifd, fbuf, FBSZ);
	if(fbsz == 0) {
		printf("empty\n");
		goto out;
	}
	if(script()) {
		goto out;
	}
	if(sccs()) {
		printf("sccs\n");	/* DAG -- removed space before newline */
		goto out;
	}
	if(rcs()) {
		printf("RCS file\n");
		goto out;
	}
	if(ckmtab())
		goto out;
	/* DAG -- PRESS file test borrowed from 4.2BSD and cleaned up: */
	if(mbuf.st_size % 512 == 0) {	/* it may be a PRESS file */
		short	buf;
		(void)lseek(ifd, -512L, 2);	/* last block */
		if(read(ifd, (char *)&buf, sizeof buf) == (int)sizeof buf
		&& buf == 12138) {
			(void)printf("PRESS file\n");
			goto out;
		}
	}
	i = 0;
	if(ccom() == 0)
		goto notc;
	while(fbuf[i] == '#') {
		j = i;
		while(fbuf[i++] != '\n') {
			if(i - j > 255) {
				printf("data\n"); 
				goto out;
			}
			if(i >= fbsz)
				goto notc;
		}
		if(ccom() == 0)
			goto notc;
	}
check:
	if(lookup(c) == 1) {
		while((ch = fbuf[i++]) != ';' && ch != '{')
			if(i >= fbsz)
				goto notc;
		printf("c program text");
		goto outa;
	}
	nl = 0;
	while(fbuf[i] != '(') {
		if(fbuf[i] <= 0)
			goto notas;
		if(fbuf[i] == ';'){
			i++; 
			goto check; 
		}
		if(fbuf[i++] == '\n')
			if(nl++ > 6)goto notc;
		if(i >= fbsz)goto notc;
	}
	while(fbuf[i] != ')') {
		if(fbuf[i++] == '\n')
			if(nl++ > 6)
				goto notc;
		if(i >= fbsz)
			goto notc;
	}
	while(fbuf[i] != '{') {
		if(fbuf[i++] == '\n')
			if(nl++ > 6)
				goto notc;
		if(i >= fbsz)
			goto notc;
	}
	printf("c program text");
	goto outa;
notc:
	i = 0;
	while(fbuf[i] == 'c' || fbuf[i] == '#') {
		while(fbuf[i++] != '\n')
			if(i >= fbsz)
				goto notfort;
	}
	if(lookup(fort) == 1){
		printf("fortran program text");
		goto outa;
	}
notfort:
	i = 0;
	if(ascom() == 0)
		goto notas;
	j = i-1;
	if(fbuf[i] == '.') {
		i++;
		if(lookup(as) == 1){
			printf("assembler program text"); 
			goto outa;
		}
		else if(j != -1 && fbuf[j] == '\n' && isalpha(fbuf[j+2])){
			printf("[nt]roff, tbl, or eqn input text");
			goto outa;
		}
	}
	while(lookup(asc) == 0) {
		if(ascom() == 0)
			goto notas;
		while(fbuf[i] != '\n' && fbuf[i++] != ':')
			if(i >= fbsz)
				goto notas;
		while(fbuf[i] == '\n' || fbuf[i] == ' ' || fbuf[i] == '\t')
			if(i++ >= fbsz)
				goto notas;
		j = i - 1;
		if(fbuf[i] == '.'){
			i++;
			if(lookup(as) == 1) {
				printf("assembler program text"); 
				goto outa; 
			}
			else if(fbuf[j] == '\n' && isalpha(fbuf[j+2])) {
				printf("[nt]roff, tbl, or eqn input text");
				goto outa;
			}
		}
	}
	printf("assembler program text");
	goto outa;
notas:
	for(i=0; i < fbsz; i++)
		if(fbuf[i]&0200) {
			if (fbuf[0]=='\100' && fbuf[1]=='\357') {
				printf("otroff output\n");	/* DAG -- bug fix (was troff) */
				goto out;
			}
			printf("data\n"); 
			goto out; 
		}
	if (mbuf.st_mode&((S_IEXEC)|(S_IEXEC>>3)|(S_IEXEC>>6)))
		printf("commands text");
	else if (troffint(fbuf, fbsz))	/* DAG -- added */
		printf("troff intermediate output text");
	else if(english(fbuf, fbsz))
		printf("English text");
	else
		printf("ascii text");
outa:
	while(i < fbsz)
		if((fbuf[i++]&0377) > 127) {
			printf(" with garbage\n");
			goto out;
		}
	printf("\n");
out:
	/* DAG -- bug fix.  Lazy programmer took a shortcut! */
	{
	struct	{
		time_t	actime;
		time_t	modtime;
		}	utb;

	utb.actime = mbuf.st_atime;
	utb.modtime = mbuf.st_mtime;

	(void)utime(file, &utb);	/* DAG -- was &mbuf.st_atime */
	}
}

mkmtab(cflg)
reg	int	cflg;
{
	reg	Entry	*ep;
	reg	FILE	*fp;
	reg	int	lcnt = 0;
	auto	char	buf[BSZ];
	auto	Entry	*mend;

	ep = (Entry *) calloc(sizeof(Entry), NENT);
	if(ep == NULL) {
		fprintf(stderr, "no memory for magic table\n");
		exit(2);
	}
	mtab = ep;
	mend = &mtab[NENT];
	fp = fopen(mfile, "r");
	if(fp == NULL) {
		fprintf(stderr, "warning : magic file <%s> can not be read.\n", mfile);
		exit(2);
	}
	while(fgets(buf, BSZ, fp) != NULL) {
		reg	char	*p = buf;
		reg	char	*p2;
		reg	char	opc;

		if(*p == '\n' || *p == '#')
			continue;
		lcnt++;
			

			/* LEVEL */
		if(*p == '>') {
			ep->e_level = 1;
			p++;
		}
			/* OFFSET */
		p2 = strchr(p, '\t');
		if(p2 == NULL) {
			if(cflg)
				fprintf(stderr, "fmt error, no tab after %son line %d\n", p, lcnt);
			continue;
		}
		*p2++ = NULL;
		ep->e_off = atolo(p);
		while(*p2 == '\t')
			p2++;
			/* TYPE */
		p = p2;
		p2 = strchr(p, '\t');
		if(p2 == NULL) {
			if(cflg)
				fprintf(stderr, "fmt error, no tab after %son line %d\n", p, lcnt);
			continue;
		}
		*p2++ = NULL;
		if(*p == 's') {
			if(*(p+1) == 'h')
				ep->e_type = SHORT;
			else
				ep->e_type = STR;
		} else if (*p == 'l')
			ep->e_type = LONG;
		/* DAG -- else calloc has filled in 0 (BYTE) */
		while(*p2 == '\t')
			p2++;	/* DAG -- removed * */
			/* OP-VALUE */
		p = p2;
		p2 = strchr(p, '\t');
		if(p2 == NULL) {
			if(cflg)
				fprintf(stderr, "fmt error, no tab after %son line %d\n", p, lcnt);
			continue;
		}
		*p2++ = NULL;
		if(ep->e_type != STR) {
			opc = *p++;
			switch(opc) {
			case '!':
			case '^':
				ep->e_opcode = NEQ;
				break;

			case '=':
				ep->e_opcode = EQ;
				break;

			case '>':
				ep->e_opcode = GT;
				break;

			case '<':
				ep->e_opcode = LT;
				break;

			case 'x':
				ep->e_opcode = ANY;
				break;

			default:
				/* DAG -- calloc has filled in 0 (EQ) */
				p--;
			}
		}
		if(ep->e_opcode != ANY) {
			if(ep->e_type != STR)
				ep->e_value.num = atolo(p);
			else {
				ep->e_value.str = malloc(strlen(p) + 1);
				convcopy(ep->e_value.str, p);
			}
		}
		while(*p2 == '\t')
			p2++;	/* DAG -- removed * */
			/* STRING */
		ep->e_str = malloc(strlen(p2) + 1);
		p = ep->e_str;
		while(*p2 != '\n') {
			if(*p2 == '%')
				ep->e_opcode |= SUB;
			*p++ = *p2++;
		}
		*p = NULL;
		ep++;
		if(ep >= mend) {
			fprintf(stderr, "file: magic tab overflow - increase NENT in file.c.\n");
			exit(2);
		}
	}
	ep->e_off = -1L;
}

long
atolo(s)
reg	char	*s;
{
	reg	char	c;
	reg	char	*fmt = "%ld";
	auto	long	j = 0L;

	if(*s == '0') {
		s++;
		if(*s == 'x') {
			s++;
			fmt = "%lx";
		} else
			fmt = "%lo";
	}
	sscanf(s, fmt, &j);
	return(j);
}

ckmtab()
{

	reg	Entry	*ep;
	reg	char	*p;
	reg	int	lev1 = 0;
	auto	union	{
		long	l;
		char	ch[4];
		}	val, revval;
	static	char	init = 0, tmpbyte;

	if(!init) {
		mkmtab(0);
		init = 1;
	}
	for(ep = mtab; ep->e_off != -1L; ep++) {
		if(lev1) {
			if(ep->e_level != 1)
				break;
		} else if(ep->e_level == 1)
			continue;
		p = &fbuf[ep->e_off];
		switch(ep->e_type) {
		case STR:
		{
			if(strncmp(p,ep->e_value.str,strlen(ep->e_value.str)))
				continue;
			if(ep->e_opcode & SUB) {
				printfield(lev1, ep->e_str, ep->e_value.str);
			} else {
				printfield(lev1, ep->e_str, "");
			}
			lev1 = 1;
		}

		case BYTE:
			val.l = (long)(*(unsigned char *) p);
			break;

		case SHORT:
			val.l = (long)(*(unsigned short *) p);
			break;

		case LONG:
			val.l = (*(long *) p);
			break;
		}
		switch(ep->e_opcode & ~SUB) {
		case EQ:
#ifdef u3b
			if(val.l != ep->e_value.num)
				if(ep->e_type == SHORT) {
					/* reverse bytes */
					revval.l = 0L;
					tmpbyte = val.ch[3];
					revval.ch[3] = val.ch[2];
					revval.ch[2] = tmpbyte;
					if(revval.l != ep->e_value.num)
						continue;
					else
						break;
				}
				else	continue;
			else
				break;
#else
			if(val.l != ep->e_value.num)
				continue;
			break;
#endif

		case NEQ:
#ifdef u3b
			if(val.l != ep->e_value.num)
				if(ep->e_type == SHORT) {
					/* reverse bytes */
					revval.l = 0L;
					tmpbyte = val.ch[3];
					revval.ch[3] = val.ch[2];
					revval.ch[2] = tmpbyte;
					if(revval.l == ep->e_value.num)
						continue;
					else
						break;
				}
				else	continue;
			else
				break;
#else
			if(val.l == ep->e_value.num)
				continue;
			break;
#endif
		case GT:
			if(val.l <= ep->e_value.num)
				continue;
			break;

		case LT:
			if(val.l >= ep->e_value.num)
				continue;
			break;
		}
		if(ep->e_opcode & SUB) {
			printfield(lev1, ep->e_str, val.l);
		} else {
			printfield(lev1, ep->e_str, "");
		}
		lev1 = 1;
	}
	if(lev1) {
		putchar('\n');
		return(1);
	}
	return(0);
}

/*
 * The subroutine printfield(pspace, fmt, text) prints the given field from the
 * magic table. If pspace is 1, a leading space may be printed. If fmt begins
 * with a backspace or a backlash and a b, no leading space will be printed.
 */

printfield(pspace, fmt, text)
	int pspace;
	char *fmt;
	char *text;
{

	if (fmt[0] == '\b') {
		pspace = 0;
		fmt++;
	} else if (fmt[0] == '\\' && fmt[1] == 'b') {
		pspace = 0;
		fmt += 2;
	}

	if (pspace) {
		putchar(' ');
	}
	printf(fmt, text);
}

lookup(tab)
reg	char **tab;
{
	reg	char	r;
	reg	int	k,j,l;

	while(fbuf[i] == ' ' || fbuf[i] == '\t' || fbuf[i] == '\n')
		i++;
	for(j=0; tab[j] != 0; j++) {
		l = 0;
		for(k=i; ((r=tab[j][l++]) == fbuf[k] && r != '\0');k++);
		if(r == '\0')
			if(fbuf[k] == ' ' || fbuf[k] == '\n' || fbuf[k] == '\t'
			    || fbuf[k] == '{' || fbuf[k] == '/') {
				i=k;
				return(1);
			}
	}
	return(0);
}

ccom()
{
	reg	char	cc;

	while((cc = fbuf[i]) == ' ' || cc == '\t' || cc == '\n')
		if(i++ >= fbsz)
			return(0);
	if(fbuf[i] == '/' && fbuf[i+1] == '*') {
		i += 2;
		while(fbuf[i] != '*' || fbuf[i+1] != '/') {
			if(fbuf[i] == '\\')
				i += 2;
			else
				i++;
			if(i >= fbsz)
				return(0);
		}
		if((i += 2) >= fbsz)
			return(0);
	}
	if(fbuf[i] == '\n')
		if(ccom() == 0)
			return(0);
	return(1);
}

ascom()
{
	while(fbuf[i] == ASCOMCHAR) {
		i++;
		while(fbuf[i++] != '\n')
			if(i >= fbsz)
				return(0);
		while(fbuf[i] == '\n')
			if(i++ >= fbsz)
				return(0);
	}
	return(1);
}

script() 	/* Also prints script type			*/
{
	reg int i;

	if (fbuf[0] == '#' && fbuf[1] == '!') {
		i = 2;
		while (fbuf[i] != '\n' && fbuf[i] != '\0') {
			i++;
		}
		printf("executable script for %.*s\n", i - 2, &fbuf[2]);
		return 1;
	}
	return 0;
}

sccs() {
	reg int i;

	if(fbuf[0] == 1 && fbuf[1] == 'h')
		for(i=2; i<=6; i++)
			if(isdigit(fbuf[i])) continue;
			else return(0);
	else
		return(0);
	return(1);
}

rcs()		/* head{spaces}{num}.{num}	*/
{
	reg char *place;

	if (fbuf[0] != 'h' || fbuf[1] != 'e' || fbuf[2] != 'a' || fbuf[3] != 'd') {
		return 0;
	}

	place = &fbuf[4];
	if (*place != ' ') {
		return 0;
	}
	place++;
	while (*place == ' ') {
		place++;
	}
	if (!isdigit(*place)) {
		return 0;
	}
	while (isdigit(*place)) {
		place++;
	}
	if (*place != '.') {
		return 0;
	}
	place++;
	if (!isdigit(*place)) {
		return 0;
	}
	return 1;
}

english (bp, n)
char *bp;
{
#	define NASC 128
	reg	int	j, vow, freq, rare;
	reg	int	badpun = 0, punct = 0;
	auto	int	ct[NASC];

	if (n<50)
		return(0); /* no point in statistics on squibs */
	for(j=0; j<NASC; j++)
		ct[j]=0;
	for(j=0; j<n; j++)
	{
		if (bp[j]<NASC)
			ct[bp[j]|040]++;
		switch (bp[j])
		{
		case '.': 
		case ',': 
		case ')': 
		case '%':
		case ';': 
		case ':': 
		case '?':
			punct++;
			if(j < n-1 && bp[j+1] != ' ' && bp[j+1] != '\n')
				badpun++;
		}
	}
	if (badpun*5 > punct)
		return(0);
	vow = ct['a'] + ct['e'] + ct['i'] + ct['o'] + ct['u'];
	freq = ct['e'] + ct['t'] + ct['a'] + ct['i'] + ct['o'] + ct['n'];
	rare = ct['v'] + ct['j'] + ct['k'] + ct['q'] + ct['x'] + ct['z'];
	if(2*ct[';'] > ct['e'])
		return(0);
	if((ct['>']+ct['<']+ct['/'])>ct['e'])
		return(0);	/* shell file test */
	return (vow*5 >= n-ct[' '] && freq >= 10*rare);
}

/* DAG -- following borrowed from 4.2BSD and slightly revised: */

troffint (bp, n)
char *bp;
int n;
{
	reg	int	k;

	i = -1;
	for(k=0; k<6; k++)
	{
		if(i++ >= n)
			return(0);
		if(lookup(troff) == 0)
			return(0);
		if(lookup(troff) == 0)
			return(0);
		while(i < n && fbuf[i] != '\n')
			i++;
	}
	return(1);
}

/*
 * The subroutine convcopy() is just like strcpy() except that it converts
 * the following \ sequences:
 *
 *	\t	tab
 *	\n	newline
 */

convcopy(dest, src)
reg char *dest;
reg char *src;
{
	
	while (*src) {
		if (*src == '\\') {
			switch (*(src + 1)) {
				case 'n':
					*dest = '\n';
					src++;
					break;

				case 't':
					*dest = '\t';
					src++;
					break;

				default:
					*dest = '\\';
					break;
			}
		} else {
			*dest = *src;
		}
		src++;
		dest++;
	}
}
