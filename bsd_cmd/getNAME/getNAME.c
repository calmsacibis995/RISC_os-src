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
#ident	"$Header: getNAME.c,v 1.2.2.3 90/05/07 18:36:15 wje Exp $"

#ifndef lint
static char *sccsid = "@(#)getNAME.c	4.3 (Berkeley) 7/10/83";
#endif

/*
 * Get name sections from manual pages.
 *	-t	for building toc
 *	-i	for building intro entries
 *	other	apropos database
 */
#include <strings.h>
#include <stdio.h>
#include <ctype.h>

int tocrc;
int intro;

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--, argv++;
	if (!strcmp(argv[0], "-t"))
		argc--, argv++, tocrc++;
	if (!strcmp(argv[0], "-i"))
		argc--, argv++, intro++;
	while (argc > 0)
		getfrom(*argv++), argc--;
	exit(0);
}

char *skip_blanks();
extern char *index();

getfrom(name)
	char *name;
{
	char headbuf[BUFSIZ];
	char linbuf[BUFSIZ];
	register char *cp;
	int i = 0;
	int	saw_dot = 0;
	int	cat_format = 0;
	int	not_blank;
	char	*sp;
	char	*next_space;
	char	*lparen;
	char	*rparen;
	char	*nps;
	char	*npe;

	if (freopen(name, "r", stdin) == 0) {
		perror(name);
		return;
	}
	for (;;) {
		if (fgets(headbuf, sizeof headbuf, stdin) == NULL)
			return;
		not_blank = do_col_b(headbuf);
		if (headbuf[0] != '.') {
			if (not_blank && 
			    ! saw_dot ) {
				sp = skip_blanks(headbuf);
				lparen = index(sp,'(');
				rparen = index(sp,')');
				if (*sp &&
				    lparen &&
				    rparen &&
				    lparen > sp &&
				    rparen > lparen) {
					next_space = sp;
					while (next_space && *next_space) {
						if (isspace(*next_space))
							break;
						next_space++;
					}
					if (next_space > rparen) {
						cat_format = 1;
						break;
					}
				};
			};			    	
			continue;
		};
		saw_dot = 1;
		if (headbuf[1] == 'T' && headbuf[2] == 'H')
			break;
		if (headbuf[1] == 't' && headbuf[2] == 'h')
			break;
	}
	for (;;) {
		if (fgets(linbuf, sizeof linbuf, stdin) == NULL)
			return;
		if (cat_format) {
			do_col_b(linbuf);
			nps = skip_blanks(linbuf);
			if (*nps == 0)
				continue;
			for (npe = nps; *npe && ! isspace(*npe);
			     npe++) ;
			if ((npe - nps) == 4 &&
			    ! strncmp(nps,"NAME",npe - nps))
				break;
			continue;
		}
		if (linbuf[0] != '.')
			continue;
		if (linbuf[1] == 'S' && linbuf[2] == 'H')
			break;
		if (linbuf[1] == 's' && linbuf[2] == 'h')
			break;
	}
	trimln(headbuf);
	if (tocrc)
		doname(name);
	if (!intro) 
	  	if (cat_format) {
			*lparen = 0;
			*rparen = 0;
			for (cp = sp; *cp != 0 ; cp++)
				if (islower(*cp))
					*cp = toupper(*cp);
			printf(".TH %s %s \" \" \" \" \" \"\t",
				sp, skip_blanks(lparen + 1));
		} else
			printf("%s\t", headbuf);
	for (;;) {
		if (fgets(linbuf, sizeof linbuf, stdin) == NULL)
			break;
		if (! cat_format &&
		    linbuf[0] == '.') {
			if (linbuf[1] == 'S' && linbuf[2] == 'H')
				break;
			if (linbuf[1] == 's' && linbuf[2] == 'h')
				break;
		}
		trimln(linbuf);
		cp = linbuf;
		if (cat_format) {
			do_col_b(cp);
			cp = skip_blanks(cp);
			if (*cp == 0) 
				break;
		};
		if (intro) {
			split(cp, name);
			continue;
		}
		if (i != 0)
			printf(" ");
		i++;
		printf("%s", cp);
	}
	printf("\n");
}

char *
skip_blanks(sp)
	char	*sp;
{
	while (isspace(*sp))
		sp++;
	return(sp);
}


do_col_b(buf)
	char	*buf;
{
	char	*rp;
	char	*wp;
	int	not_blank;
	char	last = 'x';

	not_blank = 0;
	for (wp = buf, rp = buf; *rp != 0; rp++) {
		if (*rp == '\b') {
			if (wp > buf)
				wp--;
			continue;
		} else {
			if (! isspace(*rp))
				not_blank = 1;
			else if (isspace(last))
				continue;
			else 
				*rp = ' ';
			last = *rp;
			*wp++ = last;
		};
	};
	*wp = 0;
	return(not_blank);	
}


trimln(cp)
	register char *cp;
{

	while (*cp)
		cp++;
	if (*--cp == '\n')
		*cp = 0;
}

doname(name)
	char *name;
{
	register char *dp = name, *ep;

again:
	while (*dp && *dp != '.')
		putchar(*dp++);
	if (*dp)
		for (ep = dp+1; *ep; ep++)
			if (*ep == '.') {
				putchar(*dp++);
				goto again;
			}
	putchar('(');
	if (*dp)
		dp++;
	while (*dp)
		putchar (*dp++);
	putchar(')');
	putchar(' ');
}

split(line, name)
	char *line, *name;
{
	register char *cp, *dp;
	char *sp, *sep;

	cp = index(line, '-');
	if (cp == 0)
		return;
	sp = cp + 1;
	for (--cp; *cp == ' ' || *cp == '\t' || *cp == '\\'; cp--)
		;
	*++cp = '\0';
	while (*sp && (*sp == ' ' || *sp == '\t'))
		sp++;
	for (sep = "", dp = line; dp && *dp; dp = cp, sep = "\n") {
		cp = index(dp, ',');
		if (cp) {
			register char *tp;

			for (tp = cp - 1; *tp == ' ' || *tp == '\t'; tp--)
				;
			*++tp = '\0';
			for (++cp; *cp == ' ' || *cp == '\t'; cp++)
				;
		}
		printf("%s%s\t", sep, dp);
		dorefname(name);
		printf("\t%s", sp);
	}
}

dorefname(name)
	char *name;
{
	register char *dp = name, *ep;

again:
	while (*dp && *dp != '.')
		putchar(*dp++);
	if (*dp)
		for (ep = dp+1; *ep; ep++)
			if (*ep == '.') {
				putchar(*dp++);
				goto again;
			}
	putchar('.');
	if (*dp)
		dp++;
	while (*dp)
		putchar (*dp++);
}
