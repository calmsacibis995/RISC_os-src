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
#ident	"$Header: test.c,v 1.7.2.2 90/05/09 19:00:44 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *      test expression
 *      [ expression ]
 */

#include	"defs.h"
#include <sys/types.h>
#include <sys/stat.h>

int	ap, ac;
char	**av;

test(argn, com)
char	*com[];
int	argn;
{
	ac = argn;
	av = com;
	ap = 1;
	if (eq(com[0],"["))
	{
		if (!eq(com[--ac], "]"))
			failed("test", "] missing");
	}
	com[ac] = 0;
	if (ac <= 1)
		return(1);
	return(exp() ? 0 : 1);
}

char *
nxtarg(mt)
{
	if (ap >= ac)
	{
		if (mt)
		{
			ap++;
			return(0);
		}
		failed("test", "argument expected");
	}
	return(av[ap++]);
}

exp()
{
	int	p1;
	char	*p2;

	p1 = e1();
	p2 = nxtarg(1);
	if (p2 != 0)
	{
		if (eq(p2, "-o"))
			return(p1 | exp());

#ifdef notdef
		if (eq(p2, "]") && !eq(p2, ")"))	/* DAG */
			failed("test", synmsg);		/* DAG */
#endif notdef
	}
	ap--;
	return(p1);
}

e1()
{
	int	p1;
	char	*p2;

	p1 = e2();
	p2 = nxtarg(1);

	if ((p2 != 0) && eq(p2, "-a"))
		return(p1 & e1());
	ap--;
	return(p1);
}

e2()
{
	if (eq(nxtarg(0), "!"))
		return(!e3());
	ap--;
	return(e3());
}

e3()
{
	int	p1;
	register char	*a;
	char	*p2;
	long	atol();
	long	int1, int2;

	a = nxtarg(0);
	if (eq(a, "("))
	{
		p1 = exp();
		if (!eq(nxtarg(0), ")"))
			failed("test",") expected");
		return(p1);
	}
	p2 = nxtarg(1);
	ap--;
	if ((p2 == 0) || (!eq(p2, "=") && !eq(p2, "!=")))
	{
		if (eq(a, "-r"))
			return(chk_access(nxtarg(0), S_IREAD, 0) == 0);
		if (eq(a, "-w"))
			return(chk_access(nxtarg(0), S_IWRITE, 0) == 0);
		if (eq(a, "-x"))
			return(chk_access(nxtarg(0), S_IEXEC, 0) == 0);
		if (eq(a, "-d"))
			return(filtyp(nxtarg(0), S_IFDIR));
		if (eq(a, "-c"))
			return(filtyp(nxtarg(0), S_IFCHR));
		if (eq(a, "-b"))
			return(filtyp(nxtarg(0), S_IFBLK));
		if (eq(a, "-f"))
			return(filtyp(nxtarg(0), S_IFREG));
		if (eq(a, "-u"))
			return(ftype(nxtarg(0), S_ISUID));
		if (eq(a, "-g"))
			return(ftype(nxtarg(0), S_ISGID));
		if (eq(a, "-k"))
			return(ftype(nxtarg(0), S_ISVTX));
		if (eq(a, "-p"))
#ifdef RISCOS
		{
			char   *fp;

			fp = nxtarg(0);
		        return(filtyp(fp, S_IFIFO) ||
			       filtyp(fp, S_IFSOCK));
		}
#else RISCOS
#if defined(BSD_SYS) && !defined(pyr)
#ifndef S_IFIFO
#define S_IFIFO	S_IFSOCK	/* fifo - map to socket on 4.2BSD */
#endif S_IFIFO
#endif
			return(filtyp(nxtarg(0),S_IFIFO));
#endif RISCOS
   		if (eq(a, "-s"))
			return(fsizep(nxtarg(0)));
		if (eq(a, "-t"))
		{
			if (ap >= ac)		/* no args */
				return(isatty(1));
			else if (eq((a = nxtarg(0)), "-a") || eq(a, "-o"))
			{
				ap--;
				return(isatty(1));
			}
			else
				return(isatty(atoi(a)));
		}
		if (eq(a, "-n"))
			return(!eq(nxtarg(0), ""));
		if (eq(a, "-z"))
			return(eq(nxtarg(0), ""));
	}

	p2 = nxtarg(1);
	if (p2 == 0)
		return(!eq(a, ""));
	if (eq(p2, "-a") || eq(p2, "-o"))
	{
		ap--;
		return(!eq(a, ""));
	}
	if (eq(p2, "="))
		return(eq(nxtarg(0), a));
	if (eq(p2, "!="))
		return(!eq(nxtarg(0), a));
	if (eq(p2, "-nt")) {
		return(f_newer(a, nxtarg(0)));
	}
	if (eq(p2, "-ot")) {
		return(f_older(a, nxtarg(0)));
	}
	int1 = atol(a);
	int2 = atol(nxtarg(0));
	if (eq(p2, "-eq"))
		return(int1 == int2);
	if (eq(p2, "-ne"))
		return(int1 != int2);
	if (eq(p2, "-gt"))
		return(int1 > int2);
	if (eq(p2, "-lt"))
		return(int1 < int2);
	if (eq(p2, "-ge"))
		return(int1 >= int2);
	if (eq(p2, "-le"))
		return(int1 <= int2);

	bfailed(btest, badop, p2);
/* NOTREACHED */
#ifdef gould
	return 0;	/* DAG -- added */
#endif
}


ftype(f, field)
char	*f;
int	field;
{
	struct stat statb;

	if (stat(f, &statb) < 0)
		return(0);
	if ((statb.st_mode & field) == field)
		return(1);
	return(0);
}

filtyp(f,field)
char	*f;
int field;
{
	struct stat statb;

	if (stat(f, &statb) < 0)
		return(0);
	if ((statb.st_mode & S_IFMT) == field)
		return(1);
	else
		return(0);
}



fsizep(f)
char	*f;
{
	struct stat statb;

	if (stat(f, &statb) < 0)
		return(0);
	return(statb.st_size > 0);
}

/*
 * fake diagnostics to continue to look like original
 * test(1) diagnostics
 */
bfailed(s1, s2, s3) 
char	*s1;
char	*s2;
char	*s3;
{
	prp();
	prs(s1);
	if (s2)
	{
		prs(colon);
		prs(s2);
		prs(s3);
	}
	newline();
	exitsh(ERROR);
}

/*
 * f_newer() returns a 1 if the first file is newer than the
 * second, and 0 otherwise.
 */

int
f_newer(file1, file2)
	char *file1;
	char *file2;
{

	struct stat stb1;
	struct stat stb2;

	if (stat(file1, &stb1) < 0) {
		stb1.st_mtime = 0;
	}

	if (stat(file2, &stb2) < 0) {
		stb2.st_mtime = 0;
	}

	if (stb1.st_mtime > stb2.st_mtime) {
		return 1;
	}

	return 0;
}

/*
 * f_older() returns a 1 if the first file is older than the
 * second, and 0 otherwise.
 */

int
f_older(file1, file2)
	char *file1;
	char *file2;
{

	struct stat stb1;
	struct stat stb2;

	if (stat(file1, &stb1) < 0) {
		stb1.st_mtime = 0;
	}

	if (stat(file2, &stb2) < 0) {
		stb2.st_mtime = 0;
	}

	if (stb1.st_mtime < stb2.st_mtime) {
		return 1;
	}

	return 0;
}
