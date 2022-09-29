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
#ident	"$Header: iostat.c,v 1.3.1.2 90/05/07 19:30:53 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * iostat
 */
#include "systat.h"
#include <sys/buf.h>
#ifdef RISCOS
#include <sys/elog.h>
#include <sys/stat.h>
#include "libutil.h"
#include "machine_setup.h"
#endif RISCOS

WINDOW *
openiostat()
{

	return (subwin(stdscr, LINES-1-5, 0, 5, 0));
}

closeiostat(w)
        WINDOW *w;
{

        if (w == NULL)
                return;
        wclear(w);
        wrefresh(w);
	delwin(w);
}

#ifndef RISCOS
static struct nlist nlst[] = {
#define X_DK_BUSY       0
        { "_dk_busy" },
#define X_DK_TIME       1
        { "_dk_time" },
#define X_DK_XFER       2
        { "_dk_xfer" },
#define X_DK_WDS        3
        { "_dk_wds" },
#define X_DK_SEEK       4
        { "_dk_seek" },
#define X_CP_TIME       5
        { "_cp_time" },
#ifdef vax
#define X_MBDINIT       (X_CP_TIME+1)
        { "_mbdinit" },
#define X_UBDINIT       (X_CP_TIME+2)
        { "_ubdinit" },
#endif
#ifdef tahoe
#define	X_VBDINIT	(X_CP_TIME+1)
	{ "_vbdinit" },
#endif
        { "" },
};
#else RISCOS
static struct nlist nlst[] = {
#define X_ID		0
	{ "XXX" },	/* see machine_setup() */
#define X_IDEQ		1
	{ "XXX" },	/* see machine_setup() */
#define X_SYSINFO	2
	{"sysinfo"},
#define X_CP_TIME       3
        { "cp_time" },
        { "" },
};

extern struct var	v;

struct iotime *idstat = NULL;

#endif RISCOS

static struct {
        int     dk_busy;
        long    cp_time[CPUSTATES];
        long    *dk_time;
        long    *dk_wds;
        long    *dk_seek;
        long    *dk_xfer;
} s, s1;

static  int linesperregion;
static  double etime;
static  int numbers = 0;                /* default display bar graphs */
static  int msps = 0;                   /* default ms/seek shown */

initiostat()
{

#ifdef RISCOS
        if (nlst[X_IDEQ].n_type == 0) {
		machine_setup(nlst,X_ID,X_IDEQ);
                nlist("/unix", nlst);
                if (nlst[X_IDEQ].n_type == 0) {
                        error("Disk init information isn't in namelist");
                        return(0);
                }
        }
#else RISCOS
        if (nlst[X_DK_BUSY].n_type == 0) {
                nlist("/vmunix", nlst);
                if (nlst[X_DK_BUSY].n_type == 0) {
                        error("Disk init information isn't in namelist");
                        return(0);
                }
        }
#endif RISCOS
	if (! dkinit())
		return(0);
	if (dk_ndrive) {
#define	allocate(e, t) \
    s./**/e = (t *)calloc(dk_ndrive, sizeof (t)); \
    s1./**/e = (t *)calloc(dk_ndrive, sizeof (t));
		allocate(dk_time, long);
		allocate(dk_wds, long);
		allocate(dk_seek, long);
		allocate(dk_xfer, long);
#undef allocate
#ifdef RISCOS
		if (idstat == NULL)
	    		idstat = (struct iotime *) calloc(dk_ndrive,
						  sizeof(struct iotime));
#endif RISCOS
	}
	return(1);
}

fetchiostat()
{
#ifdef RISCOS
	int	i;
	struct	sysinfo	sy;

	if (nlst[X_ID].n_type == 0)
		return;
	lseek(kmem, nlst[X_ID].n_value, L_SET);
	read(kmem,idstat,sizeof(struct iotime) * dk_ndrive);
	for (i = 0; i < dk_ndrive; i++) {
		s.dk_time[i] = idstat[i].io_act;
		s.dk_xfer[i] = idstat[i].io_cnt;
		s.dk_wds[i] = idstat[i].io_bcnt * 4;
		s.dk_seek[i] = idstat[i].io_cnt;
	}	
	if (nlst[X_CP_TIME].n_type != 0) {
		lseek(kmem, (long)nlst[X_CP_TIME].n_value, L_SET);
		read(kmem,s.cp_time, sizeof s.cp_time);
	} else {
		lseek(kmem,(long)nlst[X_SYSINFO].n_value,L_SET);
		read(kmem,&sy,sizeof(sy));
		s.cp_time[CP_USER] = sy.cpu[CPU_USER];
		s.cp_time[CP_NICE] = 0;
		s.cp_time[CP_SYS] = sy.cpu[CPU_KERNEL];
		s.cp_time[CP_IDLE] = sy.cpu[CPU_IDLE] + sy.cpu[CPU_WAIT]
				+ sy.cpu[CPU_SXBRK];
	};

#else RISCOS

        if (nlst[X_DK_BUSY].n_type == 0)
                return;
	s.dk_busy = getw(nlst[X_DK_BUSY].n_value);
        lseek(kmem, (long)nlst[X_DK_TIME].n_value, L_SET);
        read(kmem, s.dk_time, dk_ndrive * sizeof (long));
        lseek(kmem, (long)nlst[X_DK_XFER].n_value, L_SET);
        read(kmem, s.dk_xfer, dk_ndrive * sizeof (long));
        lseek(kmem, (long)nlst[X_DK_WDS].n_value, L_SET);
        read(kmem, s.dk_wds, dk_ndrive * sizeof (long));
        lseek(kmem, (long)nlst[X_DK_SEEK].n_value, L_SET);
        read(kmem, s.dk_seek, dk_ndrive * sizeof (long));
        lseek(kmem, (long)nlst[X_CP_TIME].n_value, L_SET);
        read(kmem, s.cp_time, sizeof s.cp_time);
#endif RISCOS
}

#define	INSET	10

labeliostat()
{
        int row;

#ifdef RISCOS
	char	msgbuf[100];

	if (nlst[X_ID].n_type == 0) {
		sprintf(msgbuf,"No %s defined.",nlst[X_ID].n_name);
		error(msgbuf);
		return;
	};
#else RISCOS
        if (nlst[X_DK_BUSY].n_type == 0) {
                error("No dk_busy defined.");
                return;
        }
#endif RISCOS
        row = 0;
        wmove(wnd, row, 0); wclrtobot(wnd);
        mvwaddstr(wnd, row++, INSET, 
            "/0   /10  /20  /30  /40  /50  /60  /70  /80  /90  /100");
        mvwaddstr(wnd, row++, 0, "cpu  user|");
        mvwaddstr(wnd, row++, 0, "     nice|");
        mvwaddstr(wnd, row++, 0, "   system|");
        mvwaddstr(wnd, row++, 0, "     idle|");
        if (numbers)
                row = numlabels(row + 1);
        else
                row = barlabels(row + 1);
}

static
numlabels(row)
{
        int i, col, regions, ndrives;

#define COLWIDTH        14
#define DRIVESPERLINE   ((wnd->_maxx - INSET) / COLWIDTH)
	for (ndrives = 0, i = 0; i < dk_ndrive; i++)
		if (dk_select[i])
			ndrives++;
        regions = howmany(ndrives, DRIVESPERLINE);
        /*
         * Deduct -regions for blank line after each scrolling region.
         */
        linesperregion = (wnd->_maxy - row - regions) / regions;
        /*
         * Minimum region contains space for two
         * label lines and one line of statistics.
         */
        if (linesperregion < 3)
                linesperregion = 3;
        col = 0;
        for (i = 0; i < dk_ndrive; i++)
                if (dk_select[i] && dk_mspw[i] != 0.0) {
                        if (col + COLWIDTH >= wnd->_maxx - INSET) {
                                col = 0, row += linesperregion + 1;
                                if (row > wnd->_maxy - (linesperregion + 1))
                                        break;
                        }
                        mvwaddstr(wnd, row, col + 4, dr_name[i]);
                        mvwaddstr(wnd, row + 1, col, "bps tps msps");
                        col += COLWIDTH;
                }
        if (col)
                row += linesperregion + 1;
        return (row);
}

static
barlabels(row)
        int row;
{
        int i;

        mvwaddstr(wnd, row++, INSET,
            "/0   /5   /10  /15  /20  /25  /30  /35  /40  /45  /50");
        linesperregion = 2 + msps;
        for (i = 0; i < dk_ndrive; i++)
                if (dk_select[i] && dk_mspw[i] != 0.0) {
                        if (row > wnd->_maxy - linesperregion)
                                break;
#ifdef RISCOS
                        mvwprintw(wnd, row++, 0, "%4.4s  bps|", dr_name[i]);
#else RISCOS
                        mvwprintw(wnd, row++, 0, "%3.3s   bps|", dr_name[i]);
#endif RISCOS
                        mvwaddstr(wnd, row++, 0, "      tps|");
                        if (msps)
                                mvwaddstr(wnd, row++, 0, "     msps|");
                }
        return (row);
}

showiostat()
{
        register int i, row, col;
        register long t;

#ifdef RISCOS
	if (nlst[X_ID].n_type == 0)
		return;
#else RISCOS
        if (nlst[X_DK_BUSY].n_type == 0)
                return;
#endif RISCOS
        for (i = 0; i < dk_ndrive; i++) {
#define X(fld)  t = s.fld[i]; s.fld[i] -= s1.fld[i]; s1.fld[i] = t
                X(dk_xfer); X(dk_seek); X(dk_wds); X(dk_time);
        }
        etime = 0;
        for(i = 0; i < CPUSTATES; i++) {
                X(cp_time);
                etime += s.cp_time[i];
        }
        if (etime == 0.0)
                etime = 1.0;
        etime /= (float) hz;
        row = 1;
        for (i = 0; i < CPUSTATES; i++)
                stat1(row++, i);
        if (!numbers) {
                row += 2;
                for (i = 0; i < dk_ndrive; i++)
                        if (dk_select[i] && dk_mspw[i] != 0.0) {
                                if (row > wnd->_maxy - linesperregion)
                                        break;
                                row = stats(row, INSET, i);
                        }
                return;
        }
        col = 0;
        wmove(wnd, row + linesperregion, 0);
        wdeleteln(wnd);
        wmove(wnd, row + 3, 0);
        winsertln(wnd);
        for (i = 0; i < dk_ndrive; i++)
                if (dk_select[i] && dk_mspw[i] != 0.0) {
                        if (col + COLWIDTH >= wnd->_maxx) {
                                col = 0, row += linesperregion + 1;
                                if (row > wnd->_maxy - (linesperregion + 1))
                                        break;
                                wmove(wnd, row + linesperregion, 0);
                                wdeleteln(wnd);
                                wmove(wnd, row + 3, 0);
                                winsertln(wnd);
                        }
                        (void) stats(row + 3, col, i);
                        col += COLWIDTH;
                }
}

static
stats(row, col, dn)
        int row, dn;
{
        double atime, words, xtime, itime;

        atime = s.dk_time[dn];
        atime /= (float) hz;
        words = s.dk_wds[dn]*32.0;      /* number of words transferred */
        xtime = dk_mspw[dn]*words;      /* transfer time */
        itime = atime - xtime;          /* time not transferring */
        if (xtime < 0)
                itime += xtime, xtime = 0;
        if (itime < 0)
                xtime += itime, itime = 0;
        if (numbers) {
                mvwprintw(wnd, row, col, "%3.0f%4.0f%5.1f",
                    words / 512 / etime, s.dk_xfer[dn] / etime,
                    s.dk_seek[dn] ? itime * 1000. / s.dk_seek[dn] : 0.0);
                return (row);
        }
        wmove(wnd, row++, col);
        histogram(words / 512 / etime, 50, 1.0);
        wmove(wnd, row++, col);
        histogram(s.dk_xfer[dn] / etime, 50, 1.0);
        if (msps) {
                wmove(wnd, row++, col);
                histogram(s.dk_seek[dn] ? itime * 1000. / s.dk_seek[dn] : 0,
                   50, 1.0);
        }
        return (row);
}

static
stat1(row, o)
        int row, o;
{
        register i;
        double time;

        time = 0;
        for (i = 0; i < CPUSTATES; i++)
                time += s.cp_time[i];
        if (time == 0.0)
                time = 1.0;
        wmove(wnd, row, INSET);
#define CPUSCALE        0.5
        histogram(100 * s.cp_time[o] / time, 50, CPUSCALE);
}

histogram(val, colwidth, scale)
        double val;
        int colwidth;
        double scale;
{
        char buf[10];
        register int k;
        register int v = (int)(val * scale) + 0.5;

        k = MIN(v, colwidth);
        if (v > colwidth) {
                sprintf(buf, "%4.1f", val);
                k -= strlen(buf);
                while (k--)
                        waddch(wnd, 'X');
                waddstr(wnd, buf);
                return;
        }
        while (k--)
                waddch(wnd, 'X');
        wclrtoeol(wnd);
}

cmdiostat(cmd, args)
        char *cmd, *args;
{

        if (prefix(cmd, "msps"))
                msps = !msps;
	else if (prefix(cmd, "numbers"))
                numbers = 1;
	else if (prefix(cmd, "bars"))
                numbers = 0;
	else if (!dkcmd(cmd, args))
		return (0);
        wclear(wnd);
        labeliostat();
        refresh();
        return (1);
}


