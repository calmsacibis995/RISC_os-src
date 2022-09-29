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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: ex_fstk.c,v 1.2.2.2.1.2 90/08/06 14:51:03 hawkes Exp $"

/*
 * Author:	Mark I. Himelstein
 * Date:	Wed Jun 19 14:03:30 PDT 1985
 * Purpose:	file stack for switching in file in vi
 */

#include "ex.h"
#include "ex_temp.h"

static int alttos = -1;
static int altbot;
static struct fstk {
	char	file[FNSIZE];
	int		dot;
} 
altstk [FSTKSZ];

extern int altdot;

fs_push (cp, dot)
char *cp;
int dot;
{
	if ( cp == 0  || *cp == 0)
	    return;	/* silently ignore null file names */
	if (alttos != -1) {
		alttos = (alttos + 1) % FSTKSZ;
		if (alttos == altbot)
			altbot = (altbot + 1) % FSTKSZ;
	} 
	else {
		alttos = 0;
	}

	strcpy (altstk[alttos].file, cp);
	altstk[alttos].dot = dot;

}


fs_pop ()
{
	if (alttos == -1) {
		error ("No stack|File stack is empty.");
	} 
	else {
	    if (altstk[alttos].file != 0 
	      &&altstk[alttos].file[0] != 0 )  /* don't pop empty cell */
	    {
		strcpy (altfile, altstk[alttos].file);
		altdot = altstk[alttos].dot;
	    }
	    else
		error ("No stack|File stack is empty.");
	}

	if (alttos == altbot) {
		alttos = -1;
		altbot = 0;
	} 
	else {
		alttos = ((alttos == 0) ? FSTKSZ : alttos) - 1;
	}
}

char *
fs_top()
{
	if (alttos == -1) {
		return (char *)0;
	}
	return altstk[alttos].file;
}


fs_print ()
{
	int istk;

	if (alttos == -1) {
		return;
	}
	printf (" #  line filename\n");
	printf ("--- ---- -------------\n");
	for (istk = altbot; ; istk = (istk + 1) % FSTKSZ) {
		printf ("%2d.  %3d %s\n",
		((istk >= altbot) ? istk - altbot : (FSTKSZ - altbot) + istk) + 1,
		altstk[istk].dot, altstk[istk].file); 
		if (istk == alttos)
			break;
	}
	flush();
}

fs_pushs (lfile)
char *lfile;
{
	int istk;
	int len = strlen (lfile);
	struct fstk element;
/* caller has to recover from errors, so shouldn't call error() in this */

	if (alttos == -1) {
	    return(1);
	}
	for (istk = alttos - 1; ; istk--) {
		if (istk < 0)	/* fixed a bug when stack empty */
		    istk = 0;
		if (!strncmp (altstk[istk].file, lfile, len)) {
			element = altstk [istk];
			fs_push (element.file, element.dot);
			return(0);
		} 
		if (istk == altbot)
			break;
		if (istk == 0)
		    istk = FSTKSZ;
	}
	return(2);
}

fs_pushn (n)
int n;
{
	struct fstk element;
	int lim;

	n--;
	if (altbot < alttos) {
		lim = alttos - altbot;
	} else {
		lim = alttos + (FSTKSZ - altbot);
	}
	if (n < 0 || n > lim) {
		error ("Out of range|The number argument to pn is out of range.");
	}
	n = (altbot + n) % FSTKSZ;
	element = altstk [n];
	fs_push (element.file, element.dot);
}
