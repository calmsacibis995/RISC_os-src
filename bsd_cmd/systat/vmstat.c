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
#ident	"$Header: vmstat.c,v 1.3.1.4.1.2 90/08/03 11:11:36 hawkes Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Cursed vmstat -- from Robert Elz.
 */

#include "systat.h"

#include <ctype.h>
#include <utmp.h>

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

struct	ncstats {
	int	hits;		/* hits that we can really use */
	int	misses;		/* cache misses */
	int	enters;		/* number of enters done */
	int	dbl_enters;	/* number of enters tried when already cached */
	int	long_enter;	/* long names tried to enter */
	int	long_look;	/* long names tried to look up */
	int	lru_empty;	/* LRU list empty */
	int	purges;		/* number of purges of cache */
};

#else RISCOS
#include <sys/vm.h>
#include <sys/buf.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/namei.h>

#include <machine/pte.h>
#endif RISCOS

static	int ut;

WINDOW *
openkre()
{

	ut = open("/etc/utmp", O_RDONLY);
	if (ut < 0)
		error("No utmp");
        return (stdscr);
}

closekre(w)
        WINDOW *w;
{

	(void) close(ut);
        if (w == NULL)
                return;
        wclear(w);
        wrefresh(w);
}

long	time();
float	cputime();
struct	utmp utmp;

#ifndef RISCOS
static struct nlist name[] = {
	{ "_cp_time" },
#define X_CPTIME	0
	{ "_rate" },
#define X_RATE		1
	{ "_total" },
#define X_TOTAL		2
	{ "_proc" },
#define X_PROC		3
	{ "_nproc" },
#define X_NPROC		4
	{ "_sum" },
#define X_SUM		5
	{ "_dk_busy" },
#define	X_DK_BUSY	6
	{ "_dk_time" },
#define	X_DK_TIME	7
	{ "_dk_xfer" },
#define	X_DK_XFER	8
	{ "_dk_wds" },
#define	X_DK_WDS	9
	{ "_tk_nin" },
#define	X_TK_NIN	10
	{ "_tk_nout" },
#define	X_TK_NOUT	11
	{ "_dk_seek" },
#define	X_DK_SEEK	12
	{ "_nchstats" },
#define	X_NCHSTATS	13
	{ "_intrnames" },
#define	X_INTRNAMES	14
	{ "_eintrnames" },
#define	X_EINTRNAMES	15
	{ "_intrcnt" },
#define	X_INTRCNT	16
	{ "_eintrcnt" },
#define	X_EINTRCNT	17
	{ "" },
};
#else RISCOS
static struct nlist name[] = {
	{ "rate" },
#define X_RATE		0
	{ "total" },
#define X_TOTAL		1
	{ "proc" },
#define X_PROC		2
	{ "sum" },
#define X_SUM		3
	{ "XXX" },	/* see machine_setup() */
#define X_ID		4
	{ "XXX" },	/* see machine_setup() */
#define X_IDEQ		5
	{"sysinfo"},
#define X_SYSINFO	6
	{"ncstats"},
#define X_NCSTATS	7
	{ "_cp_time" },
#define X_CPTIME	8
        { "" },
};

extern char DevName[];	/* name of block devices */

extern struct var	v;

#endif RISCOS

static struct Info {
	long	time[CPUSTATES];
	struct	vmmeter Rate;
	struct	vmtotal Total;
	struct	vmmeter Sum;
#ifdef RISCOS
	struct	sysinfo sysinfo;
	struct	iotime *idstat;
	struct	ncstats ncstats;
	long	nccount;
	long	ncentercount;
	long	nccacheentercount;
#else RISCOS
	struct	forkstat Forkstat;
	long	*dk_time;
	long	*dk_wds;
	long	*dk_seek;
	long	*dk_xfer;
	int	dk_busy;
#endif RISCOS
	long	tk_nin;
	long	tk_nout;
#ifndef RISCOS
	struct	nchstats nchstats;
	long	nchcount;
	long	*intrcnt;
#endif RISCOS
} s, s1, s2, z;

#define total s.Total
#define sum s.Sum
#define sumold s1.Sum
#define rate s.Rate
#ifdef RISCOS
#define nctotal s.ncstats
#define oldnctotal s1.ncstats
#else RISCOS
#define	nchtotal s.nchstats
#define	oldnchtotal s1.nchstats
#endif RISCOS
#define oldrate s1.Rate

static	char buf[26];
static	time_t t;
static	double etime;
static	float hertz;
static	int nintr;
static	long *intrloc;
static	char **intrname;
static	int nextintsrow;

static	enum state { BOOT, TIME, RUN } state = TIME;

/*
 * These constants define where the major pieces are laid out
 */
#define PROCSROW	13	/* uses 2 rows and 20 cols */
#define PROCSCOL	 0
#define NAMEIROW	20	/* uses 3 rows and 38 cols */
#define NAMEICOL	 0
#define GRAPHROW	16	/* uses 3 rows and 51 cols */
#define GRAPHCOL	 0
#define GENSTATROW	14	/* uses 9 rows and 11 cols */
#define GENSTATCOL	51
#define INTSROW		 2	/* uses all rows to bottom and 17 cols */
#define INTSCOL		63
#define STATROW		 0	/* uses 1 row and 68 cols */
#define STATCOL		 2
#define PAGEROW		 2	/* uses 11 rows and 26 cols */
#define PAGECOL		36
#define MEMROW		 2	/* uses 4 rows and 31 cols */
#define MEMCOL		 0
#define DISKROW		 7	/* uses 5 rows and 35 cols */
#define DISKCOL		 0

initkre()
{
	char *intrnamebuf, *cp;
	int i;
	static int once = 0;

	if (name[0].n_type == 0) {
#ifdef RISCOS
		machine_setup(name,X_ID,X_IDEQ);
		nlist("/unix",name);
#else RISCOS
		nlist("/vmunix",name);
#endif RISCOS
		if (name[0].n_type == 0) {
			error("No namelist");
			return(0);
		}
	}
	hertz = phz ? phz : hz;
	if (! dkinit())
		return(0);
	if (dk_ndrive && !once) {
#define	allocate(e, t) \
    s./**/e = (t *)calloc(dk_ndrive, sizeof (t)); \
    s1./**/e = (t *)calloc(dk_ndrive, sizeof (t)); \
    s2./**/e = (t *)calloc(dk_ndrive, sizeof (t)); \
    z./**/e = (t *)calloc(dk_ndrive, sizeof (t));
#ifdef RISCOS
		allocate(idstat, struct iotime);
#else RISCOS
		allocate(dk_time, long);
		allocate(dk_wds, long);
		allocate(dk_seek, long);
		allocate(dk_xfer, long);
#endif RISCOS
		once = 1;
#undef allocate
	}
#ifndef RISCOS
	if (nintr == 0) {
		nintr = (name[X_EINTRCNT].n_value -
			name[X_INTRCNT].n_value) / sizeof (long);
		intrloc = (long *) calloc(nintr, sizeof (long));
		intrname = (char **) calloc(nintr, sizeof (long));
		intrnamebuf = malloc(name[X_EINTRNAMES].n_value -
			name[X_INTRNAMES].n_value);
		if (intrnamebuf == 0 || intrname == 0 || intrloc == 0) {
			error("Out of memory\n");
			if (intrnamebuf)
				free(intrnamebuf);
			if (intrname)
				free(intrname);
			if (intrloc)
				free(intrloc);
			nintr = 0;
			return(0);
		}
		lseek(kmem, (long)name[X_INTRNAMES].n_value, L_SET);
		read(kmem, intrnamebuf, name[X_EINTRNAMES].n_value -
			name[X_INTRNAMES].n_value);
		for (cp = intrnamebuf, i = 0; i < nintr; i++) {
			intrname[i] = cp;
			cp += strlen(cp) + 1;
		}
		nextintsrow = INTSROW + 2;
		allocinfo(&s);
		allocinfo(&s1);
		allocinfo(&s2);
		allocinfo(&z);
	}
#endif RISCOS

	getinfo(&s2, RUN);
	copyinfo(&s2, &s1);
	return(1);
}

fetchkre()
{
	time_t now;

	time(&now);
	strcpy(buf, ctime(&now));
	buf[16] = '\0';
	getinfo(&s, state);
}

#define	MAXDRIVES	6		/* max # to display */

labelkre()
{
	register i, j;

	clear();
	mvprintw(STATROW, STATCOL + 4, "users    Load");
	mvprintw(MEMROW, MEMCOL, "Mem     REAL     VIRTUAL");
	mvprintw(MEMROW + 1, MEMCOL, "      Tot Text   Tot Text");
	mvprintw(MEMROW + 2, MEMCOL, "Act");
	mvprintw(MEMROW + 3, MEMCOL, "All");

	mvprintw(MEMROW + 1, MEMCOL + 28, "Free");

	mvprintw(PAGEROW, PAGECOL, "        PAGING   SWAPPING ");
	mvprintw(PAGEROW + 1, PAGECOL, "        in  out   in  out ");
	mvprintw(PAGEROW + 2, PAGECOL, "count");
	mvprintw(PAGEROW + 3, PAGECOL, "pages");

#ifndef RISCOS
	mvprintw(INTSROW, INTSCOL, " Interrupts");
	mvprintw(INTSROW + 1, INTSCOL + 9, "total");
#endif RISCOS

	mvprintw(GENSTATROW, GENSTATCOL + 8, "Csw");
	mvprintw(GENSTATROW + 1, GENSTATCOL + 8, "Trp");
	mvprintw(GENSTATROW + 2, GENSTATCOL + 8, "Sys");
	mvprintw(GENSTATROW + 3, GENSTATCOL + 8, "Int");
	mvprintw(GENSTATROW + 4, GENSTATCOL + 8, "Pdm");
	mvprintw(GENSTATROW + 5, GENSTATCOL + 8, "Sof");
	mvprintw(GENSTATROW + 6, GENSTATCOL + 8, "Flt");
	mvprintw(GENSTATROW + 7, GENSTATCOL + 8, "Scn");
	mvprintw(GENSTATROW + 8, GENSTATCOL + 8, "Rev");

	mvprintw(PAGEROW + 5, PAGECOL, "Rec It F/S F/F RFL Fre SFr");

	mvprintw(PAGEROW + 8, PAGECOL + 9, " zf");
	mvprintw(PAGEROW + 9, PAGECOL + 9, "nzf");
	mvprintw(PAGEROW + 10, PAGECOL + 9, "%%zf");
	mvprintw(PAGEROW + 8, PAGECOL + 23, " xf");
	mvprintw(PAGEROW + 9, PAGECOL + 23, "nxf");
	mvprintw(PAGEROW + 10, PAGECOL + 23, "%%xf");

	mvprintw(GRAPHROW, GRAPHCOL,
		"    . %% Sys    . %% User    . %% Nice    . %% Idle");
	mvprintw(PROCSROW, PROCSCOL, "Procs  r  p  d  s  w");
	mvprintw(GRAPHROW + 1, GRAPHCOL,
		"|    |    |    |    |    |    |    |    |    |    |");

	mvprintw(NAMEIROW, NAMEICOL, "Namei      Lookups          Enters    ");
	mvprintw(NAMEIROW + 1, NAMEICOL,
		"    Calls     hits    %%     hits     %%");
	mvprintw(DISKROW, DISKCOL, "Discs");
	mvprintw(DISKROW + 1, DISKCOL, "seeks");
	mvprintw(DISKROW + 2, DISKCOL, "xfers");
	mvprintw(DISKROW + 3, DISKCOL, " blks");
	mvprintw(DISKROW + 4, DISKCOL, " msps");
	j = 0;
	for (i = 0; i < dk_ndrive && j < MAXDRIVES; i++)
		if (dk_select[i]) {
#ifdef RISCOS
			mvprintw(DISKROW, DISKCOL + 5 + 5 * j,
				" %4.4s", dr_name[j]);
#else RISCOS
			mvprintw(DISKROW, DISKCOL + 5 + 5 * j,
				"  %3.3s", dr_name[j]);
#endif RISCOS
			j++;
		}
#ifndef RISCOS
	for (i = 0; i < nintr; i++) {
		if (intrloc[i] == 0)
			continue;
		mvprintw(intrloc[i], INTSCOL + 9, "%-8.8s", intrname[i]);
	}
#endif RISCOS
}

#ifdef RISCOS
#define W(fld)	{t=s.idstat[i].fld; s.idstat[i].fld-=s1.idstat[i].fld; if(state==TIME) s1.idstat[i].fld=t;}
#endif RISCOS
#define X(fld)	{t=s.fld[i]; s.fld[i]-=s1.fld[i]; if(state==TIME) s1.fld[i]=t;}
#define Y(fld)	{t = s.fld; s.fld -= s1.fld; if(state == TIME) s1.fld = t;}
#ifdef RISCOS
#define Z(fld)	{t = s.ncstats.fld; s.ncstats.fld -= s1.ncstats.fld; \
	if(state == TIME) s1.ncstats.fld = t;}
#else RISCOS
#define Z(fld)	{t = s.nchstats.fld; s.nchstats.fld -= s1.nchstats.fld; \
	if(state == TIME) s1.nchstats.fld = t;}
#endif RISCOS
#define MAXFAIL 5

static	char cpuchar[CPUSTATES] = { '=' , '>', '-', ' ' };
static	char cpuorder[CPUSTATES] = { CP_SYS, CP_USER, CP_NICE, CP_IDLE };

showkre()
{
	float f1, f2;
	int psiz, inttotal;
	int i, l, c;
	static int failcnt = 0;

	for (i = 0; i < dk_ndrive; i++) {
#ifdef RISCOS
		W(ios.io_ops); W(ios.io_misc); W(ios.io_qcnt);
		W(io_bcnt); W(io_resp); W(io_act);
#else RISCOS
		X(dk_xfer); X(dk_seek); X(dk_wds); X(dk_time);
#endif RISCOS
	}
	Y(tk_nin); Y(tk_nout);
	etime = 0;
	for(i = 0; i < CPUSTATES; i++) {
		X(time);
		etime += s.time[i];
	}
	if (etime < 5.0) {	/* < 5 ticks - ignore this trash */
		if (failcnt++ >= MAXFAIL) {
			clear();
			mvprintw(2, 10, "The alternate system clock has died!");
			mvprintw(3, 10, "Reverting to ``pigs'' display.");
			move(CMDLINE, 0);
			refresh();
			failcnt = 0;
			sleep(5);
			command("pigs");
		}
		return;
	}
	failcnt = 0;
	etime /= hertz;
	inttotal = 0;
#ifdef RISCOS
	Z(hits); Z(misses); Z(enters);
	Z(dbl_enters); Z(long_enter); Z(long_look);
	Z(lru_empty); Z(purges);
	s.nccount = nctotal.hits + nctotal.misses +
			nctotal.long_look;
	s.ncentercount = nctotal.long_enter + nctotal.dbl_enters +
		nctotal.lru_empty + nctotal.enters;
	s.nccacheentercount = nctotal.dbl_enters + nctotal.enters;
	if (state == TIME)
		s1.nccount = s.nccount;
#else RISCOS
	for (i = 0; i < nintr; i++) {
		if (s.intrcnt[i] == 0)
			continue;
		if (intrloc[i] == 0) {
			if (nextintsrow == LINES)
				continue;
			intrloc[i] = nextintsrow++;
			mvprintw(intrloc[i], INTSCOL + 9, "%-8.8s",
				intrname[i]);
		}
		X(intrcnt);
		l = (int)((float)s.intrcnt[i]/etime + 0.5);
		inttotal += l;
		putint(l, intrloc[i], INTSCOL, 8);
	}
	putint(inttotal, INTSROW + 1, INTSCOL, 8);
	Z(ncs_goodhits); Z(ncs_badhits); Z(ncs_miss);
	Z(ncs_long); Z(ncs_pass2); Z(ncs_2passes);
	s.nchcount = nchtotal.ncs_goodhits + nchtotal.ncs_badhits +
	    nchtotal.ncs_miss + nchtotal.ncs_long;
	if (state == TIME)
		s1.nchcount = s.nchcount;
#endif RISCOS

	psiz = 0;
	f2 = 0.0;
	for (c = 0; c < CPUSTATES; c++) {
		i = cpuorder[c];
		f1 = cputime(i);
		f2 += f1;
		l = (int) ((f2 + 1.0) / 2.0) - psiz;
		if (c == 0)
			putfloat(f1, GRAPHROW, GRAPHCOL + 1, 5, 1, 0);
		else
			putfloat(f1, GRAPHROW, GRAPHCOL + 12 * c,
				5, 1, 0);
		move(GRAPHROW + 2, psiz);
		psiz += l;
		while (l-- > 0)
			addch(cpuchar[c]);
	}

	putint(ucount(), STATROW, STATCOL, 3);
	putfloat(avenrun[0], STATROW, STATCOL + 17, 6, 2, 0);
	putfloat(avenrun[1], STATROW, STATCOL + 23, 6, 2, 0);
	putfloat(avenrun[2], STATROW, STATCOL + 29, 6, 2, 0);
	mvaddstr(STATROW, STATCOL + 53, buf);
	putint(total.t_arm/2, MEMROW + 2, MEMCOL + 4, 5);
	putint(total.t_armtxt/2, MEMROW + 2, MEMCOL + 9, 5);
	putint(total.t_avm/2, MEMROW + 2, MEMCOL + 14, 6);
	putint(total.t_avmtxt/2, MEMROW + 2, MEMCOL + 20, 5);
	putint(total.t_rm/2, MEMROW + 3, MEMCOL + 4, 5);
	putint(total.t_rmtxt/2, MEMROW + 3, MEMCOL + 9, 5);
	putint(total.t_vm/2, MEMROW + 3, MEMCOL + 14, 6);
	putint(total.t_vmtxt/2, MEMROW + 3, MEMCOL + 20, 5);
	putint(total.t_free, MEMROW + 2, MEMCOL + 27, 5);
	putint(total.t_rq, PROCSROW + 1, PROCSCOL + 5, 3);
	putint(total.t_pw, PROCSROW + 1, PROCSCOL + 8, 3);
	putint(total.t_dw, PROCSROW + 1, PROCSCOL + 11, 3);
	putint(total.t_sl, PROCSROW + 1, PROCSCOL + 14, 3);
	putint(total.t_sw, PROCSROW + 1, PROCSCOL + 17, 3);
	putrate(rate.v_swtch, oldrate.v_swtch, 
		GENSTATROW, GENSTATCOL, 7);
	putrate(rate.v_trap, oldrate.v_trap, 
		GENSTATROW + 1, GENSTATCOL, 7);
	putrate(rate.v_syscall, oldrate.v_syscall, 
		GENSTATROW + 2, GENSTATCOL, 7);
	putrate(rate.v_intr, oldrate.v_intr, 
		GENSTATROW + 3, GENSTATCOL, 7);
	putrate(rate.v_pdma, oldrate.v_pdma, 
		GENSTATROW + 4, GENSTATCOL, 7);
	putrate(rate.v_soft, oldrate.v_soft, 
		GENSTATROW + 5, GENSTATCOL, 7);
	putrate(rate.v_faults, oldrate.v_faults, 
		GENSTATROW + 6, GENSTATCOL, 7);
	putrate(rate.v_scan, oldrate.v_scan, 
		GENSTATROW + 7, GENSTATCOL, 7);
	putrate(rate.v_rev, oldrate.v_rev, 
		GENSTATROW + 8, GENSTATCOL, 7);
	putrate(rate.v_pgin, oldrate.v_pgin, PAGEROW + 2,
		PAGECOL + 5, 5);
	putrate(rate.v_pgout, oldrate.v_pgout, PAGEROW + 2,
		PAGECOL + 10, 5);
	putrate(rate.v_swpin, oldrate.v_swpin, PAGEROW + 2,
		PAGECOL + 15, 5);
	putrate(rate.v_swpout, oldrate.v_swpout, PAGEROW + 2,
		PAGECOL + 20, 5);
	putrate(rate.v_pgpgin, oldrate.v_pgpgin, PAGEROW + 3,
		PAGECOL + 5, 5);
	putrate(rate.v_pgpgout, oldrate.v_pgpgout, PAGEROW + 3,
		PAGECOL + 10, 5);
	putrate(rate.v_pswpin, oldrate.v_pswpin, PAGEROW + 3,
		PAGECOL + 15, 5);
	putrate(rate.v_pswpout, oldrate.v_pswpout, PAGEROW + 3,
		PAGECOL + 20, 5);
	putrate(rate.v_pgrec, oldrate.v_pgrec, PAGEROW + 6, PAGECOL, 3);
	putrate(rate.v_intrans, oldrate.v_intrans, PAGEROW + 6,
		PAGECOL + 4, 2);
	putrate(rate.v_xsfrec, oldrate.v_xsfrec, PAGEROW + 6,
		PAGECOL + 7, 3);
	putrate(rate.v_xifrec, oldrate.v_xifrec, PAGEROW + 6,
		PAGECOL + 11, 3);
	putrate(rate.v_pgfrec, oldrate.v_pgfrec, PAGEROW + 6,
		PAGECOL + 15, 3);
	putrate(rate.v_dfree, oldrate.v_dfree, PAGEROW + 6,
		PAGECOL + 19, 3);
	putrate(rate.v_seqfree, oldrate.v_seqfree, PAGEROW + 6,
		PAGECOL + 23, 3);
	putrate(rate.v_zfod, oldrate.v_zfod, PAGEROW + 8, PAGECOL, 8);
	putrate(rate.v_nzfod, oldrate.v_nzfod, PAGEROW + 9, PAGECOL, 8);
	putrate(rate.v_exfod, oldrate.v_exfod, PAGEROW + 8,
		PAGECOL + 14, 8);
	putrate(rate.v_nexfod, oldrate.v_nexfod, PAGEROW + 9,
		PAGECOL + 14, 8);
	putfloat (
		rate.v_nzfod == 0 ?
			0.0
		: state != RUN ?
			( 100.0 * rate.v_zfod / rate.v_nzfod )
		: rate.v_nzfod == oldrate.v_nzfod ?
			0.0
		:
			( 100.0 * (rate.v_zfod-oldrate.v_zfod)
			/ (rate.v_nzfod-oldrate.v_nzfod) )
		, PAGEROW + 10
		, PAGECOL
		, 8
		, 2
		, 1
	);
	putfloat (
		rate.v_nexfod == 0 ?
			0.0
		: state != RUN ?
			( 100.0 * rate.v_exfod / rate.v_nexfod )
		: rate.v_nexfod == oldrate.v_nexfod ?
			0.0
		:
			( 100.0 * (rate.v_exfod-oldrate.v_exfod)
			/ (rate.v_nexfod-oldrate.v_nexfod) )
		, PAGEROW + 10
		, PAGECOL + 14
		, 8
		, 2
		, 1
	);
	mvprintw(DISKROW,DISKCOL+5,"                              ");
	for (i = 0, c = 0; i < dk_ndrive && c < MAXDRIVES; i++)
		if (dk_select[i]) {
#ifdef RISCOS
			mvprintw(DISKROW, DISKCOL + 5 + 5 * c,
				" %4.4s", dr_name[i]);
#else RISCOS
			mvprintw(DISKROW, DISKCOL + 5 + 5 * c,
				"  %3.3s", dr_name[i]);
#endif RISCOS
			dinfo(i, ++c);
		}
#ifdef RISCOS
	putint(s.nccount, NAMEIROW + 2, NAMEICOL, 9);
	putint(nctotal.hits, NAMEIROW + 2, NAMEICOL + 9, 9);
#define nz(x)	((x) ? (x) : 1)
	putfloat(nctotal.hits * 100.0 / nz(s.nccount),
	   NAMEIROW + 2, NAMEICOL + 19, 4, 0, 1);
	putint(s.nccacheentercount, NAMEIROW + 2, NAMEICOL + 23, 9);
	putfloat(s.nccacheentercount * 100.0 / nz(s.ncentercount),
	   NAMEIROW + 2, NAMEICOL + 34, 4, 0, 1);
#undef nz
#else RISCOS
	putint(s.nchcount, NAMEIROW + 2, NAMEICOL, 9);
	putint(nchtotal.ncs_goodhits, NAMEIROW + 2, NAMEICOL + 9, 9);
#define nz(x)	((x) ? (x) : 1)
	putfloat(nchtotal.ncs_goodhits * 100.0 / nz(s.nchcount),
	   NAMEIROW + 2, NAMEICOL + 19, 4, 0, 1);
	putint(nchtotal.ncs_pass2, NAMEIROW + 2, NAMEICOL + 23, 9);
	putfloat(nchtotal.ncs_pass2 * 100.0 / nz(s.nchcount),
	   NAMEIROW + 2, NAMEICOL + 34, 4, 0, 1);
#undef nz
#endif RISCOS
}

cmdkre(cmd, args)
	char *cmd, *args;
{

	if (prefix(cmd, "run")) {
		copyinfo(&s2, &s1);
		state = RUN;
		return (1);
	}
	if (prefix(cmd, "boot")) {
		state = BOOT;
		copyinfo(&z, &s1);
		return (1);
	}
	if (prefix(cmd, "time")) {
		state = TIME;
		return (1);
	}
	if (prefix(cmd, "zero")) {
		if (state == RUN)
			getinfo(&s1, RUN);
		return (1);
	}
	return (dkcmd(cmd, args));
}

/* calculate number of users on the system */
static
ucount()
{
	register int nusers = 0;

	if (ut < 0)
		return (0);
	while (read(ut, &utmp, sizeof(utmp)))
		if ((utmp.ut_name[0] != '\0') && (utmp.ut_type == USER_PROCESS))
			nusers++;

	lseek(ut, 0L, L_SET);
	return (nusers);
}

static float
cputime(indx)
	int indx;
{
	double t;
	register i;

	t = 0;
	for (i = 0; i < CPUSTATES; i++)
		t += s.time[i];
	if (t == 0.0)
		t = 1.0;
	return (s.time[indx] * 100.0 / t);
}

static
putrate(r, or, l, c, w)
{

	if (state != TIME) {
		if (state == RUN)
			r -= or;
		putint((int)((float)r/etime + 0.5), l, c, w);
	} else
		putint(r, l, c, w);
}

static
putint(n, l, c, w)
{
	char b[128];

	move(l, c);
	if (n == 0) {
		while (w-- > 0)
			addch(' ');
		return;
	}
	sprintf(b, "%*d", w, n);
	if (strlen(b) > w) {
		while (w-- > 0)
			addch('*');
		return;
	}
	addstr(b);
}

static
putfloat(f, l, c, w, d, nz)
	float f;
{
	char b[128];

	move(l, c);
	if (nz && f == 0.0) {
		while (w-- > 0)
			addch(' ');
		return;
	}
	sprintf(b, "%*.*f", w, d, f);
	if (strlen(b) > w) {
		while (w-- > 0)
			addch('*');
		return;
	}
	addstr(b);
}

static
getinfo(s, st)
	struct Info *s;
	enum state st;
{

#ifndef RISCOS
	lseek(kmem, (long)name[X_CPTIME].n_value,L_SET);
	read(kmem, s->time, sizeof s->time);
#else RISCOS
	lseek(kmem, (long)name[X_SYSINFO].n_value,L_SET);
	read(kmem,&s->sysinfo,sizeof s->sysinfo);
	if (name[X_CPTIME].n_type != 0) {
		lseek(kmem, (long)name[X_CPTIME].n_value, L_SET);
		read(kmem,s->time, sizeof s->time);
	} else {
		s->time[CP_USER] = s->sysinfo.cpu[CPU_USER];
		s->time[CP_NICE] = 0;
		s->time[CP_SYS] = s->sysinfo.cpu[CPU_KERNEL];
		s->time[CP_IDLE] = s->sysinfo.cpu[CPU_IDLE]
				+ s->sysinfo.cpu[CPU_WAIT]
				+ s->sysinfo.cpu[CPU_SXBRK];
	};
#endif RISCOS
	if (st != TIME) {
		lseek(kmem, (long)name[X_SUM].n_value, L_SET);
		read(kmem, &s->Rate, sizeof s->Rate);
	} else {
		lseek(kmem, (long)name[X_RATE].n_value,L_SET);
		read(kmem, &s->Rate, sizeof s->Rate);
	}
	lseek(kmem, (long)name[X_TOTAL].n_value, L_SET);
	read(kmem, &s->Total, sizeof s->Total);
#ifdef RISCOS
	lseek(kmem, (long)name[X_ID].n_value, L_SET);
	read(kmem,s->idstat,sizeof(struct iotime) * dk_ndrive);
	lseek(kmem, (long)name[X_NCSTATS].n_value,  L_SET);
	read(kmem, &s->ncstats, sizeof s->ncstats);
	s->tk_nin = s->sysinfo.rawch;
	s->tk_nout = s->sysinfo.outch;
#else RISCOS
	s->dk_busy = getw(name[X_DK_BUSY].n_value);
 	lseek(kmem, (long)name[X_DK_TIME].n_value,  L_SET);
 	read(kmem, s->dk_time, dk_ndrive * sizeof (long));
 	lseek(kmem, (long)name[X_DK_XFER].n_value,  L_SET);
 	read(kmem, s->dk_xfer, dk_ndrive * sizeof (long));
 	lseek(kmem, (long)name[X_DK_WDS].n_value,  L_SET);
 	read(kmem, s->dk_wds, dk_ndrive * sizeof (long));
	lseek(kmem, (long)name[X_DK_SEEK].n_value,  L_SET);
	read(kmem, s->dk_seek, dk_ndrive * sizeof (long));
	s->tk_nin = getw(name[X_TK_NIN].n_value);
	s->tk_nout = getw(name[X_TK_NOUT].n_value);
	lseek(kmem, (long)name[X_NCHSTATS].n_value,  L_SET);
	read(kmem, &s->nchstats, sizeof s->nchstats);
	lseek(kmem, (long)name[X_INTRCNT].n_value,  L_SET);
	read(kmem, s->intrcnt, nintr * sizeof (long));
#endif RISCOS
}

#ifndef RISCOS
static
allocinfo(s)
	struct Info *s;
{

	s->intrcnt = (long *) malloc(nintr * sizeof(long));
	if (s->intrcnt == NULL) {
		fprintf(stderr, "systat: out of memory\n");
		exit(2);
	}
}
#endif RISCOS

static
copyinfo(from, to)
	register struct Info *from, *to;
{
#ifdef RISCOS
	struct iotime *idstat;

	idstat = to->idstat;
	bcopy(from->idstat, to->idstat = idstat, 
		dk_ndrive * sizeof(struct iotime));
#else RISCOS
	long *time, *wds, *seek, *xfer;
	long *intrcnt;

	time = to->dk_time; wds = to->dk_wds; seek = to->dk_seek;
	xfer = to->dk_xfer; intrcnt = to->intrcnt;
	*to = *from;
	bcopy(from->dk_time, to->dk_time = time, dk_ndrive * sizeof (long));
	bcopy(from->dk_wds, to->dk_wds = wds, dk_ndrive * sizeof (long));
	bcopy(from->dk_seek, to->dk_seek = seek, dk_ndrive * sizeof (long));
	bcopy(from->dk_xfer, to->dk_xfer = xfer, dk_ndrive * sizeof (long));
	bcopy(from->intrcnt, to->intrcnt = intrcnt, nintr * sizeof (int));
#endif RISCOS
}

static
dinfo(dn, c)
{
	double words, atime, itime, xtime;

	c = DISKCOL + c * 5;
#ifdef RISCOS
	atime = s.idstat[dn].io_resp;
#else RISCOS
	atime = s.dk_time[dn];
#endif RISCOS
	atime /= hertz;
#ifdef RISCOS
	words = s.idstat[dn].io_bcnt * 4 * 32.0;
	xtime = s.idstat[dn].io_act;
#else RISCOS
	words = s.dk_wds[dn]*32.0;	/* number of words transferred */
	xtime = dk_mspw[dn]*words;	/* transfer time */
#endif RISCOS
	itime = atime - xtime;		/* time not transferring */
	if (xtime < 0)
		itime += xtime, xtime = 0;
	if (itime < 0)
		xtime += itime, itime = 0;
#ifdef RISCOS
	putint((int)((float)s.idstat[dn].io_cnt/etime+0.5), DISKROW + 1, c, 5);
	putint((int)((float)s.idstat[dn].io_cnt/etime+0.5), DISKROW + 2, c, 5);
#else RISCOS
	putint((int)((float)s.dk_seek[dn]/etime+0.5), DISKROW + 1, c, 5);
	putint((int)((float)s.dk_xfer[dn]/etime+0.5), DISKROW + 2, c, 5);
#endif RISCOS
	putint((int)(words/etime/512.0 + 0.5), DISKROW + 3, c, 5);
#ifdef RISCOS
	if (s.idstat[dn].io_cnt)
		putfloat(itime*1000.0/s.idstat[dn].io_cnt, DISKROW + 4, c, 5, 1, 1);
#else RISCOS
	if (s.dk_seek[dn])
		putfloat(itime*1000.0/s.dk_seek[dn], DISKROW + 4, c, 5, 1, 1);
#endif RISCOS
	else
		putint(0, DISKROW + 4, c, 5);
}
