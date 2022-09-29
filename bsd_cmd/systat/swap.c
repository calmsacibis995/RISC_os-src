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
#ident	"$Header: swap.c,v 1.2.1.2 90/05/07 19:32:10 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
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
#include "sys/swap.h"
#include "sys/sysmips.h"
#include "sys/vnode.h"
#else RISCOS
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/text.h>
#include <sys/conf.h>
#include <sys/vmmac.h>
#include <machine/pte.h>
#endif RISCOS

WINDOW *
openswap()
{

	return (subwin(stdscr, LINES-5-1, 0, 5, 0));
}

closeswap(w)
	WINDOW *w;
{

	if (w == NULL)
		return;
	wclear(w);
	wrefresh(w);
	delwin(w);
}

#ifndef RISCOS
int	dmmin;
int	dmmax;
int	dmtext;
int	nswdev;
#define	MAXSWAPDEV	4
short	buckets[MAXSWAPDEV][NDMAP];
struct	swdevt *swdevt;
int	colwidth;

extern union {
        struct  user user;
        char    upages[UPAGES][NBPG];
} user;
#define u       user.user

showswap()
{
        register int i, j;
	register struct proc *pp;
	register struct text *xp;
	register int row;
	register int ts;
	register swblk_t *dp;

	if (xtext == 0)
		return;
	for (xp = xtext; xp < &xtext[ntext]; xp++) {
		if (xp->x_iptr == NULL)
			continue;
		ts = ctod(xp->x_size);
		dp = xp->x_daddr;
		for (i = 0; i < ts; i += dmtext) {
			j = ts - i;
			if (j > dmtext)
				j = dmtext;
#define	swatodev(addr)	(((addr) / dmmax) % nswdev)
			buckets[swatodev(*dp)][dmtoindex(j)]++;
			dp++;
		}
		if ((xp->x_flag & XPAGI) && xp->x_ptdaddr)
			buckets[swatodev(xp->x_ptdaddr)]
			    [dmtoindex(ctod(ctopt(xp->x_size)))]++;
	}
	row = swapdisplay(2, dmtext, 'X');
	if (kprocp == NULL)
		return;
        for (i = 0, pp = kprocp; i < nproc; i++, pp++) {
		if (pp->p_stat == 0 || pp->p_stat == SZOMB)
			continue;
		if (pp->p_flag & SSYS)
			continue;
		if (getu(pp) == 0)
			continue;
		vsacct(&u.u_dmap);
		vsacct(&u.u_smap);
#ifdef notdef
		if ((pp->p_flag & SLOAD) == 0)
			vusize(pp);
#endif
        }
	(void) swapdisplay(1+row, dmmax, 'X');
}

#define	OFFSET	5			/* left hand column */

swapdisplay(baserow, dmbound, c)
	int baserow, dmbound;
	char c;
{
	register int i, j, k, row;
	register short *pb;
	char buf[10];

	for (row = baserow, i = dmmin; i <= dmbound; i *= 2, row++) {
		for (j = 0; j < nswdev; j++) {
			pb = &buckets[j][row - baserow];
			wmove(wnd, row, OFFSET + j * (1 + colwidth));
			k = MIN(*pb, colwidth);
			if (*pb > colwidth) {
				sprintf(buf, " %d", *pb);
				k -= strlen(buf);
				while (k--)
					waddch(wnd, c);
				waddstr(wnd, buf);
			} else {
				while (k--)
					waddch(wnd, c);
				k = MAX(colwidth - *pb, 0);
				while (k--)
					waddch(wnd, ' ');
			}
			*pb = 0;
		}
	}
	return (row);
}

vsacct(dmp)
	register struct dmap *dmp;
{
	register swblk_t *ip;
	register int blk = dmmin, index = 0;

	for (ip = dmp->dm_map; dmp->dm_alloc > 0; ip++) {
		if (ip - dmp->dm_map >= NDMAP) {
			error("vsacct NDMAP");
			break;
		}
		if (*ip == 0)
			error("vsacct *ip == 0");
		buckets[swatodev(*ip)][index]++;
		dmp->dm_alloc -= blk;
		if (blk < dmmax) {
			blk *= 2;
			index++;
		}
	}
}

dmtoindex(dm)
	int dm;
{
	register int i, j;

	for (j = 0, i = dmmin; i <= dmmax; i *= 2, j++)
		if (dm <= i)
			return (j);
	error("dmtoindex(%d)", dm);
	return (NDMAP - 1);
}

static struct nlist nlst[] = {
#define X_PROC          0
        { "_proc" },
#define X_NPROC         1
        { "_nproc" },
#define X_USRPTMAP      2
        { "_Usrptmap" },
#define X_USRPT         3
        { "_usrpt" },
#define X_NSWAP         4
        { "_nswap" },
#define X_DMMIN         5
        { "_dmmin" },
#define X_DMMAX         6
        { "_dmmax" },
#define	X_DMTEXT	7
	{ "_dmtext" },
#define X_NSWDEV        8
        { "_nswdev" },
#define	X_SWDEVT	9
	{ "_swdevt" },
#define	X_NTEXT		10
	{ "_ntext" },
#define	X_TEXT		11
	{ "_text" },
        { "" }
};

initswap()
{
	if (nlst[X_PROC].n_type == 0) {
		nlist("/vmunix", nlst);
		if (nlst[X_PROC].n_type == 0) {
			error("namelist on /vmunix failed");
			return(0);
		}
	}
        if (nswdev == 0) {
                dmmin = getw(nlst[X_DMMIN].n_value);
                dmmax = getw(nlst[X_DMMAX].n_value);
                dmtext = getw(nlst[X_DMTEXT].n_value);
                nswdev = getw(nlst[X_NSWDEV].n_value);
		if (nswdev > MAXSWAPDEV)
			nswdev = MAXSWAPDEV;
		swdevt = (struct swdevt *)calloc(nswdev, sizeof (*swdevt));
		klseek(kmem, nlst[X_SWDEVT].n_value, L_SET);
		read(kmem, swdevt, nswdev * sizeof (struct swdevt));
		ntext = getw(nlst[X_NTEXT].n_value);
		textp = getw(nlst[X_TEXT].n_value);
        }
        if (procp == NULL) {
		procp = getw(nlst[X_PROC].n_value);
                nproc = getw(nlst[X_NPROC].n_value);
        }
	if (xtext == NULL)
		xtext = (struct text *)calloc(ntext, sizeof (struct text));
	if (kprocp == NULL)
                kprocp = (struct proc *)calloc(nproc, sizeof (struct proc));
        if (usrpt != NULL)
                return(1);
	usrpt = (struct pte *)nlst[X_USRPT].n_value;
	Usrptma = (struct pte *)nlst[X_USRPTMAP].n_value;
	if (pt == NULL)
		pt = (struct p_times *)malloc(nproc * sizeof (struct p_times));
	return(1);
}

fetchswap()
{

	if (nlst[X_PROC].n_type == 0)
		return;
	if (kprocp == NULL) {
                kprocp = (struct proc *)malloc(sizeof (*kprocp) * nproc);
		if (kprocp == NULL)
			return;
	}
        lseek(kmem, procp, L_SET);
        if (read(kmem, kprocp, sizeof (struct proc) * nproc) !=
	    sizeof (struct proc) * nproc) {
		error("couldn't read proc table");
		return;
	}
	if (xtext == NULL) {
		xtext = (struct text *)calloc(ntext, sizeof (struct text));
		if (xtext == NULL)
			return;
	}
	lseek(kmem, textp, L_SET);
	if (read(kmem, xtext, ntext * sizeof (struct text)) !=
	    sizeof (struct text) * ntext)
		error("couldn't read text table");
}

#ifdef vax
char	*devnames[] =
     { "hp", "ht", "up", "rk", "sw", "tm", "ts", "mt", "tu", "ra", "ut",
       "rb", "rx", "rl" };
#endif
#ifdef tahoe
char	*devnames[] = { "ud", "vd", "xp", "cy", "sw" };
#endif

labelswap()
{
	register int row;

	if (nswdev == 0) {
		error("Don't know how many swap devices.\n");
		return;
	}
	colwidth = (COLS - OFFSET - (nswdev - 1)) / nswdev;
	row = swaplabel(0, dmtext, 1);
	(void) swaplabel(row, dmmax, 0);
}

swaplabel(row, dmbound, donames)
	register int row;
	int dmbound, donames;
{
	register int i, j;

	for (i = 0; i < nswdev; i++) {
		if (donames)
			mvwprintw(wnd,
			    row, OFFSET + i*(1 + colwidth) + (colwidth - 3)/2,
			    "%s%d", devnames[major(swdevt[i].sw_dev)],
			        minor(swdevt[i].sw_dev) >> 3);
		for (j = 0; j + 5 < colwidth; j += 5)
			mvwprintw(wnd, row + donames,
			    OFFSET + i*(1 + colwidth) + j, "/%-2d  ", j);
	}
	row += 1 + donames;
	for (j = 0, i = dmmin; i <= dmbound; i *= 2, j++, row++) {
		int k;

		mvwprintw(wnd, row, 0, "%4d|", i);
		for (k = 1; k < nswdev; k++)
			mvwaddch(wnd, row, OFFSET + k*(1 + colwidth) - 1, '|');
	}
	return (row);
}

#else RISCOS

static struct nlist nlst[] = {
#define X_SYSINFO	0
	{"sysinfo"},
#define X_MINFO		1
	{"minfo"},
#define X_AVAILSMEM	2
	{"availsmem"},
#define X_AVAILRMEM	3
	{"availrmem"},
#define X_FREEMEM	4
	{"freemem"},
        { "" },
};
swpt_t  swaptab[MSFILES];
swpt_t	swaptab_base[MSFILES];
struct	vnode swap_vp[MSFILES];
int	swaptab_loaded = 0;

struct sysinfo sysinfo;

struct minfo minfo;

long	availsmem;
long	availrmem;
long	freemem;

extern struct var v;

extern char DevName[];

#define OFFSET 6
#define TABLEROW 2

showswap()
{
	int	row;
	int	i;
	int	j;

	if (! swaptab_loaded) {
		return;
	}
	mvwprintw(wnd,0,0,
		"%6d availsmem, %6d freeswap, %6d availrmem, %6d freemem",
		availsmem, minfo.freeswap, availrmem, freemem);
	for (i = 0, row = 0; row < MSFILES; i++) {
		if (i >= MSFILES) {
			putblanks(row + TABLEROW +1,0,COLS);
		} else if (swaptab[i].st_vp != NULL) {
			char	drive_name[4];

			strncpy(drive_name,DevName + strlen(DevName) - 3,2);
			drive_name[2] = 0;
			mvwprintw(wnd,row + TABLEROW + 1,0,"%s%d", drive_name,
				  rdev_to_drive_number(
				      swap_vp[i].v_rdev));
			mvwprintw(wnd,row + TABLEROW + 1,OFFSET - 1,"|");
			wmove(wnd,row + TABLEROW + 1,OFFSET);
			j = swaphistogram(swaptab[i].st_npgs - swaptab[i].st_nfpgs,
				COLS - OFFSET - 10,
				'+');
			wmove(wnd,row + TABLEROW + 1,OFFSET + j);
			swaphistogram(swaptab[i].st_npgs,COLS - OFFSET - j,'-');
		} else
			continue;
		row++;
	}
}

swaphistogram(v, colwidth, c)
        int	 v;
        int colwidth;
	char	c;
{
        char buf[10];
        register int k;
	int	cols_used;

	v = (v + 500) / 1000;
        k = MIN(v, colwidth);
	cols_used = k;
        if (v > colwidth) {
                sprintf(buf, "%-8d", v);
                k -= strlen(buf);
                while (k--)
                        waddch(wnd, c);
                waddstr(wnd, buf);
                return;
        }
        while (k--)
                waddch(wnd, c);
        wclrtoeol(wnd);
	return(cols_used);
}

initswap()
{
	swpi_t	swaparg;
	int	i;

	if (! swaptab_loaded) {	
	        if (nlst[X_SYSINFO].n_type == 0) {
	                nlist("/unix", nlst);
	                if (nlst[X_SYSINFO].n_type == 0) {
	                        error("System status information isn't in namelist");
	                        return(0);
	                }
	        }
		if (! dkinit())
			return(0);

		bzero(&swaparg,sizeof(swaparg));
		swaparg.si_cmd = SI_LIST;
		swaparg.si_buf = (char *) swaptab;
		if (sysmips(SMIPSSWPI,&swaparg) == -1) {
			error("could not read swaptab");
			return(0);
		};
		bcopy(swaptab,swaptab_base,sizeof(swaptab));
		for (i = 0; i < MSFILES; i++) {
			if (swaptab[i].st_vp == NULL) 
				continue;
			lseek(kmem,(int) swaptab[i].st_vp,L_SET);
			read(kmem,(char *) &swap_vp[i],sizeof(struct vnode));
		};
		swaptab_loaded = 1;
	};
	return(1);
}


fetchswap()
{
	swpi_t	swaparg;
	int	swaptab_changed = 0;
	int	i;

	if (! swaptab_loaded)
		return;
	lseek(kmem,(long)nlst[X_SYSINFO].n_value,L_SET);
	read(kmem,&sysinfo,sizeof(sysinfo));
	lseek(kmem,(long)nlst[X_MINFO].n_value,L_SET);
	read(kmem,&minfo,sizeof(minfo));
	lseek(kmem,(long)nlst[X_AVAILSMEM].n_value,L_SET);
	read(kmem,&availsmem,sizeof(availsmem));
	lseek(kmem,(long)nlst[X_AVAILRMEM].n_value,L_SET);
	read(kmem,&availrmem,sizeof(availrmem));
	lseek(kmem,(long)nlst[X_FREEMEM].n_value,L_SET);
	read(kmem,&freemem,sizeof(freemem));
	swaparg.si_cmd = SI_LIST;
	swaparg.si_buf = (char *) swaptab;
	if (sysmips(SMIPSSWPI,&swaparg) == -1) {
		error("could not read swaptab");
		return(0);
	};
	for (i = 0; i < MSFILES; i++) {
		if (swaptab[i].st_vp != swaptab_base[i].st_vp) {
			if (swaptab[i].st_vp != NULL) {
				lseek(kmem,(int) swaptab[i].st_vp,L_SET);
				read(kmem,(char *) &swap_vp[i],sizeof(struct vnode));
			};
			swaptab_base[i] = swaptab[i];
			swaptab_changed++;
		};
	};
	if (swaptab_changed)
		labelswap();
}

labelswap()
{
	register int row;
	int	i;
	int	j;

	if (! swaptab_loaded) {
		error("Could not read swaptab.\n");
		return;
	}
	for (j = 0; OFFSET + j + 5 < COLS; j+=5)
		mvwprintw(wnd, TABLEROW,
			    OFFSET + j, "/%-3d  ", j);
	for (i = 0, row = 0; row < MSFILES; i++) {
		if (i >= MSFILES) {
			putblanks(row + TABLEROW + 1,0,COLS);
		} else if (swaptab[i].st_vp != NULL) {
			char	drive_name[4];

			strncpy(drive_name,DevName + strlen(DevName) - 3,2);
			drive_name[2] = 0;
			mvwprintw(wnd,row + TABLEROW + 1,0,"%s%d", drive_name,
				  rdev_to_drive_number(
				      swap_vp[i].v_rdev));
			mvwprintw(wnd,row + TABLEROW + 1,OFFSET - 1,"|");
			putblanks(row + TABLEROW + 1,0,OFFSET - 2);
		} else
			continue;
		row++;
	}
}


putblanks(l, c, w)
{
	int	j;

	wmove(wnd,l,c);
	for (j = 0; j < w; j++)
		waddch(wnd,' ');
}

#endif RISCOS
