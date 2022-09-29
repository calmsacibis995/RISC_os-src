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
#ident	"$Header: fetch.c,v 1.2.1.2 90/05/07 19:30:46 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include "systat.h"
#ifdef RISCOS
#include "sys/types.h"
#include "sys/dir.h"
#include "sys/pfdat.h"
#include "sys/pcb.h"
#include "sys/sbd.h"
#include "sys/sysmacros.h"
#include "sys/proc.h"
#include "sys/dir.h"
#include "sys/user.h"
#include "sys/var.h"
#define UPAGES USIZE
#define NBPG NBPC
#define CLSIZE 1
#else RISCOS
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/vmmac.h>
#include <machine/pte.h>
#endif RISCOS
#include <pwd.h>

long
getw(loc)
        int loc;
{
        long word;

        lseek(kmem, loc, L_SET);
        if (read(kmem, &word, sizeof (word)) != sizeof (word))
                printf("Error reading kmem at %x\n", loc);
        return (word);
}

char *
getpname(pid, mproc)
        int pid;
        register struct proc *mproc;
{
        register struct procs *pp;
        register char *namp;
        register int j;
        char *getcmd();

        pp = procs;
        for (j = numprocs - 1; j >= 0; j--) {
                if (pp->pid == pid)
                        return (pp->cmd);
                pp++;
        }
        if (j < 0) {
                if (numprocs < 200) {
                        pp = &procs[numprocs];
                        namp = strncpy(pp->cmd, getcmd(pid, mproc), 15);
                        pp->cmd[15] = 0;
                        pp->pid = pid;
                        numprocs++;
                } else
                        namp = getcmd(pid, mproc);
        }
        return (namp);
}

union {
        struct  user user;
        char    upages[UPAGES][NBPG];
} user;
#define u       user.user

char *
getcmd(pid, mproc)
        int pid;
        register struct proc *mproc;
{
        static char cmd[30];

        if (mproc == NULL || mproc->p_stat == SZOMB)
		return ("");
#ifndef RISCOS
	if (pid == 1)
		return ("swapper");
	if (pid == 2)
		return ("pagedaemon");
#endif RISCOS
        if (mproc->p_flag&(SSYS|SWEXIT))
                return ("");
        if (getu(mproc) == 0)
		return ("???");
        (void) strncpy(cmd, u.u_comm, sizeof (cmd));
        return (cmd);
}

static	int argaddr;
static	int pcbpf;

getu(mproc)
        register struct proc *mproc;
{
#ifndef RISCOS
        struct pte *pteaddr, apte;
        struct pte arguutl[UPAGES+CLSIZE];
#else RISCOS
	pde_t	arguutl[UPAGES];
#endif RISCOS
        register int i;
        int ncl, size;

#ifdef RISCOS
	size = UPAGES * NBPG;
#else RISCOS
        size = sizeof (struct user);
        if ((mproc->p_flag & SLOAD) == 0) {
                if (swap < 0)
                        return (0);
                (void) lseek(swap, (long)dtob(mproc->p_swaddr), L_SET);
                if (read(swap, (char *)&user.user, size) != size) {
			error("cant read u for pid %d", mproc->p_pid);
                        return (0);
                }
                pcbpf = 0;
                argaddr = 0;
                return (1);
        }
        pteaddr = &Usrptma[btokmx(mproc->p_p0br) + mproc->p_szpt - 1];
        klseek(kmem, (long)pteaddr, L_SET);
        if (read(kmem, (char *)&apte, sizeof (apte)) != sizeof (apte)) {
                error("cant read indir pte to get u for pid %d", mproc->p_pid);
                return (0);
        }
        klseek(mem,
            (long)ctob(apte.pg_pfnum+1) - (UPAGES+CLSIZE) * sizeof (struct pte),
                L_SET);
        if (read(mem, (char *)arguutl, sizeof (arguutl)) != sizeof (arguutl)) {
                error("cant read page table for u of pid %d", mproc->p_pid);
                return (0);
        }
        if (arguutl[0].pg_fod == 0 && arguutl[0].pg_pfnum)
                argaddr = ctob(arguutl[0].pg_pfnum);
        else
                argaddr = 0;
        pcbpf = arguutl[CLSIZE].pg_pfnum;
#endif RISCOS
        ncl = (size + NBPG*CLSIZE - 1) / (NBPG*CLSIZE);
        while (--ncl >= 0) {
                i = ncl * CLSIZE;
#ifdef RISCOS
		klseek(mem, (long)ctob(mproc->p_ubptbl[i].pgm.pg_pfn), L_SET);
#else RISCOS
                klseek(mem, (long)ctob(arguutl[CLSIZE+i].pg_pfnum), L_SET);
#endif RISCOS
                if (read(mem, user.upages[i], CLSIZE*NBPG) != CLSIZE*NBPG) {
                        error("cant read page %d of u of pid %d\n",
#ifdef RISCOS
			    mproc->p_ubptbl[i].pgm.pg_pfn, 
#else RISCOS
                            arguutl[CLSIZE+i].pg_pfnum,
#endif RISCOS
			    mproc->p_pid);
                        return (0);
                }
        }
#ifdef U_BSD43_EXTENSION
	if (U_BSD43_EXTENSION(&u)) {
		u.u_bsd43_extension_p = (struct u_bsd43_extension *)
			((((char *) u.u_bsd43_extension_p) - 
				((char *) UADDR)) + ((char *) &u));
	};
#endif U_BSD43_EXTENSION
        return (1);
}

klseek(fd, loc, off)
        int fd;
        long loc;
        int off;
{

        (void) lseek(fd, (long)loc, off);
}
