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
#ident	"$Header: fgrep.c,v 1.5.2.2 90/05/09 15:49:56 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * fgrep -- print all lines containing any of a set of keywords
 *
 *	status returns:
 *		0 - ok, and some matches
 *		1 - ok, but no matches
 *		2 - some error
 */

#include <stdio.h>

/* The same() macro and letter() function were inserted to allow for the -i option */

#define	same(a,b) (a == b || iflag && (a ^ b) == ' ' && letter(a) == letter(b))

#define	MAXSIZ 6000
#define QSIZE 400
struct words {
	char 	inp;
	char	out;
	struct	words *nst;
	struct	words *link;
	struct	words *fail;
} w[MAXSIZ], *smax, *q;

FILE *fptr;
long	lnum;
int	bflag, cflag, lflag, fflag, nflag, vflag, xflag, eflag;
int	iflag;
int	retcode = 0;
int	nfile;
long	blkno;
int	nsucc;
long	tln;
FILE	*wordf;
char	*argptr;
extern	char *optarg;
extern	int optind;
void exit();

main(argc, argv)
char **argv;
{
	register c;
	char *usage;
	int errflg = 0;
	usage = "[ -bcilnvx ] [ -e exp ] [ -f file ] [ strings ] [ file ] ...";

	while(( c = getopt(argc, argv, "bcie:f:lnvx")) != EOF)
		switch(c) {

		case 'b':
			bflag++;
			continue;

		case 'i':
			iflag++;
			continue;

		case 'c':
			cflag++;
			continue;

		case 'e':
			eflag++;
			argptr = optarg;
			continue;

		case 'f':
			fflag++;
			wordf = fopen(optarg, "r");
			if (wordf==NULL) {
				fprintf(stderr, "fgrep: can't open %s\n", optarg);
				exit(2);
			}
			continue;

		case 'l':
			lflag++;
			continue;

		case 'n':
			nflag++;
			continue;

		case 'v':
			vflag++;
			continue;

		case 'x':
			xflag++;
			continue;

		case '?':
			errflg++;
	}

	argc -= optind;
	if (errflg || ((argc <= 0) && !fflag && !eflag)) {
		printf("usage: fgrep %s\n",usage);
		exit(2);
	}
	if ( !eflag  && !fflag ) {
		argptr = argv[optind];
		optind++;
		argc--;
	}
	cgotofn();
	cfail();
	nfile = argc;
	argv = &argv[optind];
	if (argc<=0) {
		if (lflag) exit(1);
		execute((char *)NULL);
	}
	else
		while ( --argc >= 0 ) {
			execute(*argv);
			argv++;
		}
	exit(retcode != 0 ? retcode : nsucc == 0);
}

execute(file)
char *file;
{
	register char *p;
	register struct words *c;
	register ccount;
	char buf[2*BUFSIZ];
	int failed;
	char *nlp;
	if (file) {
		if ((fptr = fopen(file, "r")) == NULL) {
			fprintf(stderr, "fgrep: can't open %s\n", file);
			retcode = 2;
			return;
		}
	}
	else
		fptr = stdin;
	ccount = 0;
	failed = 0;
	lnum = 1;
	tln = 0;
	blkno = 0;
	p = buf;
	nlp = p;
	c = w;
	for (;;) {
		if (--ccount <= 0) {
			if (p == &buf[2*BUFSIZ]) p = buf;
			if (p > &buf[BUFSIZ]) {
				if ((ccount = fread(p, sizeof(char), &buf[2*BUFSIZ] - p, fptr)) <= 0) break;
			}
			else if ((ccount = fread(p, sizeof(char), BUFSIZ, fptr)) <= 0) break;
			blkno += (long)ccount;
		}
		nstate:
			if (same(c->inp,*p)) {
				c = c->nst;
			}
			else if (c->link != 0) {
				c = c->link;
				goto nstate;
			}
			else {
				c = c->fail;
				failed = 1;
				if (c==0) {
					c = w;
					istate:
					if (same(c->inp,*p)) {
						c = c->nst;
					}
					else if (c->link != 0) {
						c = c->link;
						goto istate;
					}
				}
				else goto nstate;
			}
		if (c->out) {
			while (*p++ != '\n') {
				if (--ccount <= 0) {
					if (p == &buf[2*BUFSIZ]) p = buf;
					if (p > &buf[BUFSIZ]) {
						if ((ccount = fread(p, sizeof(char), &buf[2*BUFSIZ] - p, fptr)) <= 0) break;
					}
					else if ((ccount = fread(p, sizeof(char), BUFSIZ, fptr)) <= 0) break;
					blkno += (long)ccount;
				   }
			}
			if ( (vflag && (failed == 0 || xflag == 0)) || (vflag == 0 && xflag && failed) )
				goto nomatch;
	succeed:	nsucc = 1;
			if (cflag) tln++;
			else if (lflag) {
				printf("%s\n", file);
				fclose(fptr);
				return;
			}
			else {
				if (nfile > 1) printf("%s:", file);
				if (bflag) printf("%d:", (blkno-(long)(ccount-1))/BUFSIZ);
				if (nflag) printf("%ld:", lnum);
				if (p <= nlp) {
					while (nlp < &buf[2*BUFSIZ]) putchar(*nlp++);
					nlp = buf;
				}
				while (nlp < p) putchar(*nlp++);
			}
	nomatch:	lnum++;
			nlp = p;
			c = w;
			failed = 0;
			continue;
		}
		if (*p++ == '\n')
			if (vflag) goto succeed;
			else {
				lnum++;
				nlp = p;
				c = w;
				failed = 0;
			}
	}
	fclose(fptr);
	if (cflag) {
		if (nfile > 1)
			printf("%s:", file);
		printf("%ld\n", tln);
	}
}

getargc()
{
			/* appends a newline to shell quoted argument list so */
			/* the list looks like it came from an ed style file  */
	register c;
	static int endflg;
	if (wordf)
		return(getc(wordf));
	if (endflg) 
		return(EOF);
	if ((c = *argptr++) == '\0') {
		endflg++;
		return('\n');
	}
	return(c);
}

cgotofn() {
	register c;
	register struct words *s;

	s = smax = w;
nword:	for(;;) {
		c = getargc();
		if (c==EOF)
			return;
		if (c == '\n') {
			if (xflag) {
				for(;;) {
					if (s->inp == c) {
						s = s->nst;
						break;
					}
					if (s->inp == 0) goto nenter;
					if (s->link == 0) {
						if (smax >= &w[MAXSIZ -1]) overflo();
						s->link = ++smax;
						s = smax;
						goto nenter;
					}
					s = s->link;
				}
			}
			s->out = 1;
			s = w;
		} else {
		loop:	if (s->inp == c) {
				s = s->nst;
				continue;
			}
			if (s->inp == 0) goto enter;
			if (s->link == 0) {
				if (smax >= &w[MAXSIZ - 1]) overflo();
				s->link = ++smax;
				s = smax;
				goto enter;
			}
			s = s->link;
			goto loop;
		}
	}

	enter:
	do {
		s->inp = c;
		if (smax >= &w[MAXSIZ - 1]) overflo();
		s->nst = ++smax;
		s = smax;
	} while ((c = getargc()) != '\n' && c!=EOF);
	if (xflag) {
	nenter:	s->inp = '\n';
		if (smax >= &w[MAXSIZ -1]) overflo();
		s->nst = ++smax;
	}
	smax->out = 1;
	s = w;
	if (c != EOF)
		goto nword;
}

overflo() {
	fprintf(stderr, "wordlist too large\n");
	exit(2);
}
cfail() {
	struct words *queue[QSIZE];
	struct words **front, **rear;
	struct words *state;
	register char c;
	register struct words *s;
	s = w;
	front = rear = queue;
init:	if ((s->inp) != 0) {
		*rear++ = s->nst;
		if (rear >= &queue[QSIZE - 1]) overflo();
	}
	if ((s = s->link) != 0) {
		goto init;
	}

	while (rear!=front) {
		s = *front;
		if (front == &queue[QSIZE-1])
			front = queue;
		else front++;
	cloop:	if ((c = s->inp) != 0) {
			*rear = (q = s->nst);
			if (front < rear)
				if (rear >= &queue[QSIZE-1])
					if (front == queue) overflo();
					else rear = queue;
				else rear++;
			else
				if (++rear == front) overflo();
			state = s->fail;
		floop:	if (state == 0) state = w;
			if (state->inp == c) {
			qloop:	q->fail = state->nst;
				if ((state->nst)->out == 1) q->out = 1;
				if ((q = q->link) != 0) goto qloop;
			}
			else if ((state = state->link) != 0)
				goto floop;
		}
		if ((s = s->link) != 0)
			goto cloop;
	}
}

letter(c)
register int c;
{
	if (c >= 'a' && c <= 'z')
		return (c);
	if (c >= 'A' && c <= 'z')
		return (c + 'a' - 'A');
	return(0);
}
