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
#ident	"$Header: pigs.c,v 1.3.1.2 90/05/07 19:31:32 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Pigs display from Bill Reeves at Lucasfilm
 */

#include "systat.h"
#ifdef RISCOS
#include "sys/types.h"
#include "sys/buf.h"
#include "sys/stat.h"
#include "sys/dir.h"
#include "sys/pfdat.h"
#include "sys/pcb.h"
#include "sys/sbd.h"
#include "sys/sysmacros.h"
#include "sys/proc.h"
#include "sys/user.h"
#include "sys/var.h"
#define UPAGES USIZE
#define NBPG NBPC
#define CLSIZE 1
#include "sys/elog.h"
#include "bsd43/sys/vmmeter.h"
#define vmmeter bsd43_vmmeter
#define vmtotal bsd43_vmtotal
#define v_syscall bsd43_v_syscall
#define dinfo dinfo_
#else RISCOS
#include <sys/dir.h>
#include <sys/time.h>
#include <sys/proc.h>
#endif RISCOS
#include <pwd.h>

WINDOW *
openpigs()
{

	return (subwin(stdscr, LINES-5-1, 0, 5, 0));
}

closepigs(w)
	WINDOW *w;
{

	if (w == NULL)
		return;
	wclear(w);
	wrefresh(w);
	delwin(w);
}

int	maxind;
int     factor;
float   total;
struct  passwd *getpwuid();
char    pidname[30];
long	stime[CPUSTATES];
double	idle;

showpigs()
{
        register short auid;
        register int i, j, y;
        register float max;
        register struct p_times *ptptr;
        struct p_times temppt;
        register struct users *knptr;
        char *getpname(), *pnamp;

	if (pt == NULL)
		return;
        /* Accumulate the percent of cpu per user. */
        ptptr = pt;
        numprocs = 0;
        total = 0.0;
        for (i = 0; i < nproc; i++) {
                /* discard inactive processes */
                if (ptptr->pt_uid == -1) {
                        ptptr++;
                        continue;
                }
                /* Accumulate the percentage. */
                total += ptptr->pt_pctcpu;
                numprocs++;
                ptptr++;
        }

        pt[numprocs].pt_pctcpu = idle;
	total += idle;
        pt[numprocs].pt_uid = -1;
        pt[numprocs].pt_pid = -1;
        pt[numprocs].pt_pp = NULL;

	if (total < 1.0)
		total = 1.0;
        factor = 50.0/total;

        /* Find the top few by executing a "bubble pass" ten times. */
	y = numprocs + 1;
	if (y > wnd->_maxy-1)
		y = wnd->_maxy-1;
        for (i = 0; i < y; i++) {
                ptptr = &pt[i];
                max = -10000.0;
                maxind = i;
                for (j = i; j < numprocs + 1; j++) {
                        if (ptptr->pt_pctcpu > max) {
                                max = ptptr->pt_pctcpu;
                                maxind = j;
                        }
                        ptptr++;
                }
                if (maxind != i) {
                        temppt = pt[i];
                        pt[i] = pt[maxind];
                        pt[maxind] = temppt;
                }
        }
        y = 1;
        ptptr = pt;
	i = numprocs + 1;
	if (i > wnd->_maxy-1)
		i = wnd->_maxy-1;
        for (; i > 0 && ptptr->pt_pctcpu > 0.01; i--) {
                /* Find the user's name. */
                knptr = known;
                auid = ptptr->pt_uid;
                for (j = numknown - 1; j >= 0; j--) {
                        if (knptr->k_uid == auid) {
                                namp = knptr->k_name;
                                break;
                        }
                        knptr++;
                }
                if (j < 0) {
                        if (numknown < 30) {
                                knptr = &known[numknown];
                                namp = strncpy(knptr->k_name,
                                    getpwuid(auid)->pw_name, 15);
                                knptr->k_name[15] = '\0';
                                knptr->k_uid = auid;
                                numknown++;
                        } else
                                namp = getpwuid(auid)-> pw_name;
                }
                pnamp = getpname(ptptr->pt_pid, ptptr->pt_pp);
                wmove(wnd, y, 0);
                wclrtoeol(wnd);
                mvwaddstr(wnd, y, 0, namp);
                sprintf(pidname, "%10.10s", pnamp);
                mvwaddstr(wnd, y, 9, pidname);
                wmove(wnd, y++, 20);
                for (j = ptptr->pt_pctcpu*factor + 0.5; j > 0; j--)
                        waddch(wnd, 'X');
                ptptr++;
        }
	wmove(wnd, y, 0); wclrtobot(wnd);
}

#ifndef RISCOS
static struct nlist nlst[] = {
#define X_PROC          0
        { "_proc" },
#define X_NPROC         1
        { "_nproc" },
#define X_USRPTMAP      2
        { "_Usrptmap" },
#define X_USRPT         3
        { "_usrpt" },
#define X_CPTIME	4
	{ "_cp_time" },
        { "" }
};
#else RISCOS
static struct nlist nlst[] = {
#define X_PROC          0
        { "proc" },
#define X_SYSINFO          1
        { "sysinfo" },
#define X_CPTIME	2
	{ "cp_time" },
        { "" }
};

extern struct var v;
#endif RISCOS

initpigs()
{

	if (nlst[X_PROC].n_type == 0) {
#ifdef RISCOS
		nlist("/unix", nlst);
		if (nlst[X_PROC].n_type == 0) {
			error("namelist on /unix failed");
			return(0);
		}
		if (! dkinit()) {
			nlst[X_PROC].n_type = 0;
			return(0);
		};
#else RISCOS
		nlist("/vmunix", nlst);
		if (nlst[X_PROC].n_type == 0) {
			error("namelist on /vmunix failed");
			return(0);
		}
#endif RISCOS
	}
        if (procp == NULL) {
#ifdef RISCOS
		procp = (long) nlst[X_PROC].n_value;
		nproc = v.v_proc;
#else RISCOS
                procp = getw(nlst[X_PROC].n_value);
                nproc = getw(nlst[X_NPROC].n_value);
#endif RISCOS
        }
	if (kprocp == NULL)
                kprocp = (struct proc *)calloc(nproc, sizeof (struct proc));
#ifndef RISCOS
        if (usrpt != NULL)
		return(1);
	usrpt = (struct pte *)nlst[X_USRPT].n_value;
	Usrptma = (struct pte *)nlst[X_USRPTMAP].n_value;
#endif RISCOS
	if (pt == NULL)
		pt = (struct p_times *)calloc(nproc, sizeof (struct p_times));
#ifndef RISCOS
	lseek(kmem, (long)nlst[X_CPTIME].n_value, L_SET);
	read(kmem, stime, sizeof stime);
#else RISCOS
	if (nlst[X_CPTIME].n_type != 0) {
		lseek(kmem, (long)nlst[X_CPTIME].n_value, L_SET);
		read(kmem,stime, sizeof stime);
	} else {
		struct sysinfo s;

		lseek(kmem, (long)nlst[X_SYSINFO].n_value,L_SET);
		read(kmem,&s,sizeof s);
		stime[CP_USER] = s.cpu[CPU_USER];
		stime[CP_NICE] = 0;
		stime[CP_SYS] = s.cpu[CPU_KERNEL];
		stime[CP_IDLE] = s.cpu[CPU_IDLE]
				+ s.cpu[CPU_WAIT]
				+ s.cpu[CPU_SXBRK];
	};
#endif RISCOS
	return(1);
}

fetchpigs()
{
        register int i;
        register struct p_times *prt;
        register float time;
        register struct proc *pp;
	long ctime[CPUSTATES];
	double t;

	if (nlst[X_PROC].n_type == 0)
		return;
	if (kprocp == NULL) {
		kprocp = (struct proc *)calloc(nproc, sizeof (struct proc));
		if (kprocp == NULL)
			return;
	}
	if (pt == NULL) {
		pt = (struct p_times *)calloc(nproc, sizeof (struct p_times));
		if (pt == NULL)
			return;
	}
        prt = pt;
        lseek(kmem, procp, L_SET);
        read(kmem, kprocp, sizeof (struct proc) * nproc);
        for (i = 0, pp = kprocp; i < nproc; i++, pp++) {
                time = pp->p_time;
                if (time == 0 || (pp->p_flag & SLOAD) == 0)
                        continue;
                prt->pt_pid = pp->p_pid;
                prt->pt_pp = pp;
#ifdef RISCOS
                prt->pt_pctcpu = FIX_TO_DBL(pp->p_pctcpu) / (1.0 - exp(time * lccpu));
#else RISCOS
                prt->pt_pctcpu = pp->p_pctcpu / (1.0 - exp(time * lccpu));
#endif RISCOS
                prt->pt_uid = pp->p_uid;
                prt++;
        }
        for (; prt < &pt[nproc]; prt++)
                prt->pt_uid = -1;
#ifdef RISCOS
	if (nlst[X_CPTIME].n_type != 0) {
		lseek(kmem, (long)nlst[X_CPTIME].n_value, L_SET);
		read(kmem,ctime, sizeof ctime);
	} else {
		struct sysinfo s;

		lseek(kmem, (long)nlst[X_SYSINFO].n_value,L_SET);
		read(kmem,&s,sizeof s);
		ctime[CP_USER] = s.cpu[CPU_USER];
		ctime[CP_NICE] = 0;
		ctime[CP_SYS] = s.cpu[CPU_KERNEL];
		ctime[CP_IDLE] = s.cpu[CPU_IDLE]
				+ s.cpu[CPU_WAIT]
				+ s.cpu[CPU_SXBRK];
	};
#else RISCOS
	lseek(kmem, (long)nlst[X_CPTIME].n_value, L_SET);
	read(kmem, ctime, sizeof ctime);
#endif RISCOS
	t = 0;
	for (i = 0; i < CPUSTATES; i++)
		t += ctime[i] - stime[i];
	if (t == 0.0)
		t = 1.0;
	idle = (ctime[CP_IDLE] - stime[CP_IDLE]) / t;
	for (i = 0; i < CPUSTATES; i++)
		stime[i] = ctime[i];
}

labelpigs()
{

	wmove(wnd, 0, 0); wclrtoeol(wnd);
        mvwaddstr(wnd, 0, 20,
                "/0   /10  /20  /30  /40  /50  /60  /70  /80  /90  /100");
}
