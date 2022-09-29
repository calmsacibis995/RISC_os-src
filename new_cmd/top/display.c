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
#ident	"$Header: display.c,v 1.5.1.2.1.3 90/08/03 11:15:05 hawkes Exp $"

/*
 *  Top - a top users display for Berkeley Unix
 *
 *  This file contains the routines that display information on the screen.
 *  Each section of the screen has two routines:  one for initially writing
 *  all constant and dynamic text, and one for only updating the text that
 *  changes.  The prefix "i_" is used on all the "initial" routines and the
 *  prefix "u_" is used for all the "updating" routines.  NOTE:  it is
 *  assumed that none of the "i_" routines use any of the termcap
 *  capabilities.  In this way, those routines can be safely used on
 *  terminals that have minimal (or nonexistant) terminal capabilities.
 */

#include <stdio.h>
#include <ctype.h>
#ifndef RISCOS
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#if defined(scs)
# define FLOAT		/* for pctcpu in proc.h */
# include <sys/vm.h>	/* for struct spt */
#endif
#include <sys/proc.h>
#include <sys/dk.h>
#else RISCOS
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/signal.h>
#include <sys/sbd.h>
#include <sys/pcb.h>
#include <sys/immu.h>
#include <sys/region.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/termio.h>
#include <sys/fixpoint.h>
#include <sys/var.h>
#include <sys/sysinfo.h>
#endif RISCOS
#include "screen.h"		/* interface to screen package */
#include "layout.h"		/* defines for screen position layout */
#include "top.h"
#include "boolean.h"

static int lmpid = 0;
#ifndef RISCOS
static struct user u;
#else RISCOS
#define UAREA_SIZE 8192
union {
  	struct	user Uval_u;
	char	Uval_c[USIZE * UAREA_SIZE];
	} Uval;
#define u Uval.Uval_u
extern int pfn_to_byte_shift;
#endif RISCOS

#if defined(scs)
static struct spt	pspt;
#endif scs

char *printable();

/* Verbose process state names */

char *state_name[] =
{
#ifndef RISCOS
    "", "sleeping", "ABANDONED", "running", "starting", "zombie", "stopped"
#else RISCOS
    "", "sleeping", "running", "zombie", "stopped", "starting", "switching", 
      "xswapping"
#endif RISCOS
};

/* process state names for the "STATE" column of the display */

char *state_abbrev[] =
{
#ifndef RISCOS
    "", "sleep", "WAIT", "run", "start", "zomb", "stop"
#else RISCOS
    "", "sleep", "run", "zomb", "stop", "start", "onpr", "xbrk"
#endif RISCOS
};

/* cpu state names for percentages */

char *cpu_state[] =
{
#ifndef RISCOS
    "user", "nice", "system", "idle"
#else RISCOS
    "idle", "user", "system", "wait", "sxbrk"
#endif RISCOS
};

/* screen positions for cpustate figures */
#ifndef RISCOS
char x_cpustates[] = { 12, 24, 36, 50 };
#else RISCOS
char x_cpustates[] = { 12, 24, 36, 50, 62 };
#endif RISCOS

i_loadave(mpid, avenrun)

int mpid;
#ifdef RISCOS
fix *avenrun;
#else RISCOS
#if defined(sun)
long *avenrun;
#else
double *avenrun;
#endif sun
#endif RISCOS

{
    register int i;

    printf("last pid: %5d;  load averages", mpid);

    for (i = 0; i < 3; i++)
    {
	printf("%c %4.2f",
	    i == 0 ? ':' : ',',
#ifdef RISCOS
	    FIX_TO_DBL(avenrun[i])
#else RISCOS
#if defined(sun)
	    (double)avenrun[i] / FSCALE
#else
	    avenrun[i]
#endif
#endif RISCOS
		);
    }
    lmpid = mpid;
}

u_loadave(mpid, avenrun)

int mpid;
#ifdef RISCOS
fix *avenrun;
#else RISCOS
#if defined(sun)
long *avenrun;
#else
double *avenrun;
#endif sun
#endif RISCOS

{
    register int i;

    if (mpid != lmpid);
    {
	Move_to(x_lastpid, y_lastpid);
	printf("%5d", mpid);
	lmpid = mpid;
    }

    Move_to(x_loadave, y_loadave);
    for (i = 0; i < 3; i++)
    {
	printf("%s%4.2f",
	    i == 0 ? "" : ", ",
#ifdef RISCOS
	    FIX_TO_DBL(avenrun[i])
#else RISCOS
#if defined(sun)
	    (double)avenrun[i] / FSCALE
#else
	    avenrun[i]
#endif
#endif RISCOS
		);
    }
}

static int ltotal = 0;

static int lbrkdn[NUM_PROCSTATES];

i_procstates(total, brkdn)

int total;
int *brkdn;

{
    register int i;

    printf("%4d processes", total);	/* ??? */
    ltotal = total;
    for (i = 1; i < NUM_PROCSTATES; i++)
    {
	if (brkdn[i] != 0)
	{
	    printf("%c %d %s%s",
		    i == 1 ? ':' : ',',
		    brkdn[i],
		    state_name[i],
		    (i == SZOMB) && (brkdn[i] > 1) ? "s" : "");
	}
    }
    bcopy(brkdn, lbrkdn, sizeof(lbrkdn));
}

u_procstates(total, brkdn)

int total;
int *brkdn;

{
    register int i;

    if (ltotal != total)
    {
	Move_to(x_procstate, y_procstate);
	printf("%d ", total);
	ltotal = total;
    }
    else if (bcmp(brkdn, lbrkdn, sizeof(lbrkdn)) == 0)
    {
	return;
    }

    Move_to(x_brkdn, y_brkdn);
    for (i = 1; i < NUM_PROCSTATES; i++)
    {
	if (brkdn[i] != 0)
	{
	    printf("%s%d %s%s",
		    i == 1 ? "" : ", ",
		    brkdn[i],
		    state_name[i],
		    (i == SZOMB) && (brkdn[i] > 1) ? "s" : "");
	}
    }
    putcap(clear_line);
    bcopy(brkdn, lbrkdn, sizeof(lbrkdn));
}

i_cpustates(changes, total)

int *changes;
int total;

{
    register int i;

    printf("\nCpu states: ");
    for (i = 0; i < CPUSTATES; i++)
    {
	printf("%s%4.1f%% %s",
		i == 0 ? "" : ", ",
		((float)changes[i] / (float)total) * 100.0,
		cpu_state[i]);
    }
    printf("\n");
}

u_cpustates(changes, total)

int *changes;
int total;

{
    register int i;

    for (i = 0; i < CPUSTATES; i++)
    {
	Move_to(x_cpustates[i], y_cpustates);
	printf("%4.1f",
		((float)changes[i] / (float)total) * 100.0);
    }
}

z_cpustates()

{
    register int i;

    printf("\nCpu states: ");
    for (i = 0; i < CPUSTATES; i++)
    {
	printf("%s    %% %s", i == 0 ? "" : ", ", cpu_state[i]);
    }
    printf("\n");
}

i_memory(i1, i2, i3, i4, i5)

int i1, i2, i3, i4, i5;

{
    printf("Memory: %6dK (%6dK) real, %6dK (%6dK) virtual, %6dK free",
	i1, i2, i3, i4, i5);
}

u_memory(i1, i2, i3, i4, i5)

int i1, i2, i3, i4, i5;

{
    Move_to(x_realmem, y_mem);
    printf("%6dK (%6d", i1, i2);
    Move_to(x_virtmem, y_mem);
    printf("%6dK (%6d", i3, i4);
    Move_to(x_free, y_mem);
    printf("%6d", i5);
}

i_header(f2)

char *f2;

{
    printf(
#ifdef RISCOS
      "\n\n  PID %s PRI NICE   SIZE      STATE   TIME   WCPU    CPU COMMAND", 
#else RISCOS
      "\n\n  PID %s PRI NICE   SIZE   RES STATE   TIME   WCPU    CPU COMMAND", 
#endif RISCOS
      f2);
}

u_header()

{
    Move_to(0, y_header);
}

#ifdef RISCOS
#define percent_cpu(pp) (FIX_TO_DBL((pp)->p_pctcpu))
#else RISCOS
#if defined(sun)
#define percent_cpu(pp) ((double)(pp)->p_pctcpu / FSCALE)
#else
#define percent_cpu(pp) ((pp)->p_pctcpu)
#endif sun
#endif RISCOS

#define weighted_cpu(pct, pp) ((pp)->p_time == 0 ? 0.0 : \
			 ((pct) / (1.0 - exp((pp)->p_time * logcpu))))

#ifdef DEBUG
FILE *debug;
#endif

static void
fmt_proc(thisline, pp, get_userid)
	char		*thisline;
	struct proc	*pp;
	char		*(*get_userid)();
{
	register long cputime;
	register double pctcpu;

	/* get the cpu usage and calculate the cpu percentages */
	cputime = get_ucpu(pp);
	pctcpu = percent_cpu(pp);

	/* format the line */

#ifdef RISCOS
#define Proc_format \
	"%5d %-8.8s %3d %4d%6dK      %-5s%4d:%02d %5.2f%% %5.2f%% %.14s"
#else RISCOS
#define Proc_format \
	"%5d %-8.8s %3d %4d%6dK %4dK %-5s%4d:%02d %5.2f%% %5.2f%% %.14s"
#endif RISCOS

#if !defined(pyr) && !defined(scs)
	/* vax or sun, both are the same here */
	sprintf(thisline, Proc_format,
		pp->p_pid,
		(*get_userid)(pp->p_uid),
		pp->p_pri - PZERO,
		pp->p_nice - NZERO,
#ifdef RISCOS
		pagetok(pp->p_size),
#else RISCOS
		pagetok(pp->p_tsize + pp->p_dsize + pp->p_ssize),
		pagetok(pp->p_rssize),
#endif RISCOS
		state_abbrev[pp->p_stat],
		cputime / 60l,
		cputime % 60l,
		100.0 * weighted_cpu(pctcpu, pp),
		100.0 * pctcpu,
#ifdef RISCOS
		printable(u.u_psargs)
#else RISCOS
		printable(u.u_comm)
#endif RISCOS
		);
#else
	/* pyr xor scs */
#if defined(pyr)
	sprintf(thisline, Proc_format,
		pp->p_pid,
		(*get_userid)(pp->p_uid),
		pp->p_pri - PZERO,
		pp->p_nice - NZERO,
		pagetok(pp->p_tsize+pp->p_dsize+pp->p_cssize+pp->p_ussize),
		pagetok(pp->p_rssize),
		state_abbrev[pp->p_stat],
		cputime / 60l,
		cputime % 60l,
		100.0 * weighted_cpu(pctcpu, pp),
		100.0 * pctcpu,
		printable(u.u_comm));
#endif pyr
#if defined(scs)
	get_spt(pp->p_spti, &pspt);
	sprintf(thisline, Proc_format,
		pp->p_pid,
		(*get_userid)(pp->p_uid),
		pp->p_pri - PZERO,
		pp->p_nice - NZERO,
		pagetok(pspt.spt_usedpages),
		pagetok(pspt.spt_mempages),
		state_abbrev[pp->p_stat],
		cputime / 60l,
		cputime % 60l,
		100.0 * weighted_cpu(pctcpu, pp),
		100.0 * pctcpu,
		printable(u.u_comm));
#endif scs
#endif /* for vax/sun / pyr/scs */
}

i_process(line, pp, get_userid)
	int line;
	struct proc *pp;
	char *(*get_userid)();
{
	register char *thisline;
	int len;

#ifdef DEBUG
	debug = fopen("debug", "w");
#endif

	/* calculate a pointer to the buffer for this line */
	thisline = screenbuf[line];

	/* format the line into our buffer */
	fmt_proc(thisline, pp, get_userid);

	/* write the line out */
	putchar('\n');
	fputs(thisline, stdout);

	/* zero fill the rest of it */
	len = strlen(thisline);
	bzero(thisline + len, Display_width - len);
}

static int lastline = 0;

u_process(line, pp, get_userid)
	int line;
	struct proc *pp;
	char *(*get_userid)();
{
    register char *optr;
    register char *nptr;
    register int ch;
    register int diff;
    register int newcol = 1;
    register int lastcol = 0;
    char cursor_on_line = No;
    char *thisline;
    int screen_line = line + Header_lines;
    static char newline[Display_width];

    /* get a pointer to the old text for this line */
    optr = thisline = screenbuf[line];

	/* format the line into our temporary buffer */
	fmt_proc(newline, pp, get_userid);

    /* compare the two strings and only rewrite what has changed */
    nptr = newline;
#ifdef DEBUG
    fputs(optr, debug);
    fputc('\n', debug);
    fputs(nptr, debug);
    fputs("\n-\n", debug);
#endif

    /* start things off on the right foot		    */
    /* this is to make sure the invariants get set up right */
    if ((ch = *nptr++) != *optr)
    {
	if (screen_line - lastline == 1)
	{
	    putchar('\n');
	}
	else
	{
	    Move_to(0, screen_line);
	}
	cursor_on_line = Yes;
	putchar(ch);
	*optr = ch;
	lastcol = 1;
    }
    optr++;

    /*
     *  main loop -- check each character.  If the old and new aren't the
     *	same, then update the display.  When the distance from the current
     *	cursor position to the new change is small enough, the characters
     *	that belong there are written to move the cursor over.
     *
     *	Invariants:
     *	    lastcol is the column where the cursor currently is sitting
     *		(always one beyond the end of the last mismatch).
     */
    do		/* yes, a do...while */
    {
	if ((ch = *nptr++) != *optr)
	{
	    /* new character is different from old	  */
	    /* put the new character in the screen buffer */
	    *optr = ch;

	    /* make sure the cursor is on top of this character */
	    diff = newcol - lastcol;
	    if (diff > 0)
	    {
		/* some motion is required--figure out which is shorter */
		if (diff < 6 && cursor_on_line)
		{
		    /* overwrite old stuff--get it out of the screen buffer */
		    printf("%.*s", diff, &thisline[lastcol]);
		}
		else
		{
		    /* use cursor addressing */
		    Move_to(newcol, screen_line);
		    cursor_on_line = Yes;
		}
		/* remember where the cursor is */
		lastcol = newcol + 1;
	    }
	    else
	    {
		/* already there, update position */
		lastcol++;
	    }

	    /* write what we need to */
	    if (ch == '\0')
	    {
		/* at the end--terminate with a clear-to-end-of-line */
		putcap(clear_line);
	    }
	    else
	    {
		/* write the new character */
		putchar(ch);
	    }
	}

	/* update working column and screen buffer pointer */
	newcol++;
	optr++;

    } while (ch != '\0');

    /* zero out the rest of the line buffer -- MUST BE DONE! */
    bzero(optr, Display_width - newcol);

    /* remember where the current line is */
    if (cursor_on_line)
    {
	lastline = screen_line;
    }
}

static int last_hi = 0;

u_endscreen(hi)

register int hi;

{
    register int screen_line = hi + Header_lines;

    if (smart_terminal)
    {
	if (hi < last_hi)
	{
	    if (hi == 0)
	    {
		putchar('\n');
		putchar('\n');
		putcap(clear_line);
		putchar('\n');
	    }
	    else if (screen_line - lastline == 1)
	    {
		putchar('\n');
	    }
	    else
	    {
		Move_to(0, screen_line);
	    }
    
	    while (--last_hi > hi)
	    {
		putcap(clear_line);
		putchar('\n');
	    }
	    putcap(clear_line);
	}
	else
	{
	    last_hi = hi;
	}

	/* move the cursor to a pleasant place */
	Move_to(x_idlecursor, y_idlecursor);
    }
    else
    {
	/* separate this display from the next with some vertical room */
	fputs("\n\n", stdout);
    }
}

/*
 *  get_ucpu(pp) - retrieve the user structure associated with the proc
 *	structure pointed to by pp and return the cpu usage.  The user
 *	structure is stored in the global structure "u" for later use.
 */

#ifdef RISCOS
#define Name_Field u.u_psargs
#else RISCOS
#define Name_Field u.u_comm
#endif RISCOS

get_ucpu(pp)

struct proc *pp;

{
#if defined(scs)

	strcpy(u.u_comm, pp->p_infoname);
	return pp->p_infotime.tv_sec;

#else

    if (getu(pp, &u) == -1)
    {
	strcpy(Name_Field, "<swapped>");
	return(0);
    }
    else
    {
	/* set u_comm for system processes */
	if (Name_Field[0] == '\0')
	{
	    if (pp->p_pid == 0)
	    {
		strcpy(Name_Field, "Swapper");
	    }
	    else if (pp->p_pid == 2)
	    {
		strcpy(Name_Field, "Pager");
	    }
	}

#ifdef FOUR_ONE
	return((int)((float)(u.u_vm.vm_utime + u.u_vm.vm_stime)/hz));
#else
	return(u.u_ru.ru_utime.tv_sec + u.u_ru.ru_stime.tv_sec);
#endif
    }
#endif scs
}

/*
 *  printable(str) - make the string pointed to by "str" into one that is
 *	printable (i.e.: all ascii), by converting all non-printable
 *	characters into '?'.  Replacements are done in place and a pointer
 *	to the original buffer is returned.
 */

char *printable(str)

char *str;

{
    register char *ptr;
    register char ch;

    ptr = str;
    while ((ch = *ptr) != '\0')
    {
	if (!isprint(ch))
	{
	    *ptr = '?';
	}
	ptr++;
    }
    return(str);
}
