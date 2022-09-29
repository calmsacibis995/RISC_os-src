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
#ident	"$Header: vmstat.c,v 1.3.1.3 90/05/07 19:49:41 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include <stdio.h>
#include <ctype.h>
#include <nlist.h>

#include <sys/param.h>
#include <sys/file.h>
#ifdef RISCOS
#include <sys/sysinfo.h>
#include <sys/var.h>
#ifndef CPUSTATES
#define	CPUSTATES	4

#define	CP_USER		0
#define	CP_NICE		1
#define	CP_SYS		2
#define	CP_IDLE		3
#endif CPUSTATES
#include <sys/fixpoint.h>
#include <math.h>
#include <sys/types.h>
#include <sys/buf.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/pfdat.h>
#include <sys/pcb.h>
#include <sys/sbd.h>
#include <sys/sysmacros.h>
#include <sys/signal.h>
#include <sys/proc.h>
#include <sys/user.h>
#define UPAGES USIZE
#define NBPG NBPC
#define CLSIZE 1
#include <sys/kmem.h>
#include <sys/elog.h>
#include <bsd43/sys/vmmeter.h>
#define vmmeter bsd43_vmmeter
#define vmtotal bsd43_vmtotal
#define v_syscall bsd43_v_syscall
#define v_vrfod bsd43_v_vrfod
#define dinfo dinfo_
#define signal(x,y) sigset(x,y)
#include "libutil.h"
#include "machine_setup.h"

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
#include <sys/dkstat.h>
#include <sys/buf.h>
#include <sys/dir.h>
#include <sys/inode.h>
#include <sys/namei.h>
#include <sys/text.h>
#include <sys/malloc.h>
#endif RISCOS

#ifdef RISCOS
struct nlist nl[] = {
#define	X_RATE		0
	{ "rate" },
#define X_TOTAL		1
	{ "total" },
#define	X_DEFICIT	2
	{ "deficit" },
#define X_SUM		3
	{ "sum" },
#define X_ID		4
	{ "XXX" },		/* see machine_setup() */
#define X_IDEQ		5
	{ "XXX" },		/* see machine_setup() */
#define X_SYSINFO	6
	{ "sysinfo" },
#define X_V		7
	{ "v" },
#define	X_FIRSTFREE	8
	{ "firstfree" },
#define	X_MAXFREE	9
	{ "maxfree" },
#define	X_BOOTTIME	10
	{ "boottime" },
#define X_NCSTATS	11
	{ "ncstats" },
#define	X_KMEMSTAT	12
	{ "kmemstats" },
#define	X_KMEMBUCKETS	13
	{ "bucket" },
#define X_KMEMMAXBUCKETS 14
	{ "maxbucket" },
#define X_CPTIME	15
	{ "cp_time" },
#define X_ID_STRING	16
	{ "id_string" },
	{ "" },
};
#define X_DK_NDRIVE X_IDEQ

struct var	v;

#else RISCOS
struct nlist nl[] = {
#define	X_CPTIME	0
	{ "_cp_time" },
#define	X_RATE		1
	{ "_rate" },
#define X_TOTAL		2
	{ "_total" },
#define	X_DEFICIT	3
	{ "_deficit" },
#define	X_FORKSTAT	4
	{ "_forkstat" },
#define X_SUM		5
	{ "_sum" },
#define	X_FIRSTFREE	6
	{ "_firstfree" },
#define	X_MAXFREE	7
	{ "_maxfree" },
#define	X_BOOTTIME	8
	{ "_boottime" },
#define	X_DKXFER	9
	{ "_dk_xfer" },
#define X_REC		10
	{ "_rectime" },
#define X_PGIN		11
	{ "_pgintime" },
#define X_HZ		12
	{ "_hz" },
#define X_PHZ		13
	{ "_phz" },
#define X_NCHSTATS	14
	{ "_nchstats" },
#define	X_INTRNAMES	15
	{ "_intrnames" },
#define	X_EINTRNAMES	16
	{ "_eintrnames" },
#define	X_INTRCNT	17
	{ "_intrcnt" },
#define	X_EINTRCNT	18
	{ "_eintrcnt" },
#define	X_DK_NDRIVE	19
	{ "_dk_ndrive" },
#define	X_XSTATS	20
	{ "_xstats" },
#define	X_KMEMSTAT	21
	{ "_kmemstats" },
#define	X_KMEMBUCKETS	22
	{ "_bucket" },
#ifdef vax
#define X_MBDINIT	(X_XSTATS+1)
	{ "_mbdinit" },
#define X_UBDINIT	(X_XSTATS+2)
	{ "_ubdinit" },
#endif
#ifdef tahoe
#define	X_VBDINIT	(X_XSTATS+1)
	{ "_vbdinit" },
#define	X_CKEYSTATS	(X_XSTATS+2)
	{ "_ckeystats" },
#define	X_DKEYSTATS	(X_XSTATS+3)
	{ "_dkeystats" },
#endif
	{ "" },
};
#endif RISCOS

char	**dr_name;
int	*dr_select;
int	dk_ndrive;
int	ndrives = 0;
#ifdef vax
char	*defdrives[] = { "hp0", "hp1", "hp2",  0 };
#else
char	*defdrives[] = { 0 };
#endif
double	stat1();
int	firstfree, maxfree;
#ifdef RISCOS
#ifndef HZ
#define HZ 100
#endif HZ
#define hz HZ
#define phz 0
#else RISCOS
int	hz;
int	phz;
int	HZ;
#endif RISCOS

struct {
	int	busy;
	long	time[CPUSTATES];
#ifndef RISCOS
	long	*xfer;
#endif RISCOS
	struct	vmmeter Rate;
	struct	vmtotal	Total;
	struct	vmmeter Sum;
#ifdef RISCOS
	struct	sysinfo	Sysinfo;
	struct iotime *Iotime;

#else RISCOS
	struct	forkstat Forkstat;
	unsigned rectime;
	unsigned pgintime;
#endif RISCOS
} s, s1, z;
#define	rate		s.Rate
#define	total		s.Total
#define	sum		s.Sum
#ifdef RISCOS
#define sysinfo_ s.Sysinfo
#else RISCOS
#define	forkstat	s.Forkstat
#endif RISCOS


struct	vmmeter osum;
int	deficit;
double	etime;
int 	mf;
time_t	now, boottime;
int	printhdr();
int	lines = 1;

#define	INTS(x)	((x) - (hz + phz))

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *ctime();
	register i;
	int iter, nintv, iflag = 0;
	long t;
	char *arg, **cp, buf[BUFSIZ];

#ifdef RISCOS
	machine_setup(nl,X_ID,X_IDEQ);
	nlist("/unix", nl);
	if(nl[0].n_type == 0) {
		fprintf(stderr, "no /unix namelist\n");
		exit(1);
	}
#else RISCOS
	nlist("/vmunix", nl);
	if(nl[0].n_type == 0) {
		fprintf(stderr, "no /vmunix namelist\n");
		exit(1);
	}
#endif RISCOS
	mf = open("/dev/kmem", 0);
	if(mf < 0) {
		fprintf(stderr, "cannot open /dev/kmem\n");
		exit(1);
	}
#ifdef RISCOS
	/* Check to ensure that the kernel is strings match */
	if (check_kernel_id (mf,
			(long)nl[X_ID_STRING].n_value, "/unix") > 0) {
		fprintf(stderr,"/unix does not match /dev/kmem\n");
		exit(1);
	};
#endif RISCOS
	iter = 0;
	argc--, argv++;
	while (argc>0 && argv[0][0]=='-') {
		char *cp = *argv++;
		argc--;
		while (*++cp) switch (*cp) {

		case 't':
			dotimes();
			exit(0);

		case 'z':
			close(mf);
			mf = open("/dev/kmem", 2);
			lseek(mf, (long)nl[X_SUM].n_value, L_SET);
			write(mf, &z.Sum, sizeof z.Sum);
			exit(0);

		case 'f':
			doforkst();
			exit(0);
		
		case 'm':
			domem();
			exit(0);

		case 's':
			dosum();
			exit(0);

		case 'i':
			iflag++;
			break;

		default:
			fprintf(stderr,
			    "usage: vmstat [ -fsim ] [ interval ] [ count]\n");
			exit(1);
		}
	}
	lseek(mf, (long)nl[X_FIRSTFREE].n_value, L_SET);
	read(mf, &firstfree, sizeof firstfree);
	lseek(mf, (long)nl[X_MAXFREE].n_value, L_SET);
	read(mf, &maxfree, sizeof maxfree);
	lseek(mf, (long)nl[X_BOOTTIME].n_value, L_SET);
	read(mf, &boottime, sizeof boottime);
#ifdef RISCOS
	lseek(mf, (long)nl[X_V].n_value, L_SET);
	read(mf, &v, sizeof(v));
#else RISCOS
	lseek(mf, (long)nl[X_HZ].n_value, L_SET);
	read(mf, &hz, sizeof hz);
	if (nl[X_PHZ].n_value != 0) {
		lseek(mf, (long)nl[X_PHZ].n_value, L_SET);
		read(mf, &phz, sizeof phz);
	}
	HZ = phz ? phz : hz;
#endif RISCOS
	if (nl[X_DK_NDRIVE].n_value == 0) {
		fprintf(stderr, "%s undefined in system\n",
			nl[X_DK_NDRIVE].n_name);
		exit(1);
	}
	lseek(mf, nl[X_DK_NDRIVE].n_value, L_SET);
	read(mf, &dk_ndrive, sizeof (dk_ndrive));
	if (dk_ndrive <= 0) {
		fprintf(stderr, "%s %d\n",nl[X_DK_NDRIVE].n_name, dk_ndrive);
		exit(1);
	}
	dr_select = (int *)calloc(dk_ndrive, sizeof (int));
	dr_name = (char **)calloc(dk_ndrive, sizeof (char *));
#define	allocate(e, t) \
    s./**/e = (t *)calloc(dk_ndrive, sizeof (t)); \
    s1./**/e = (t *)calloc(dk_ndrive, sizeof (t));
#ifdef RISCOS
	allocate(Iotime, struct iotime);
#else RISCOS
	allocate(xfer, long);
#endif RISCOS
	for (arg = buf, i = 0; i < dk_ndrive; i++) {
		dr_name[i] = arg;
#ifdef RISCOS
		strncpy(arg,DevName + strlen(DevName) - 3,2);
		arg[2] = 0;
		sprintf(arg + strlen(arg),"%d",i);
#else RISCOS
		sprintf(dr_name[i], "dk%d", i);
#endif RISCOS
		arg += strlen(dr_name[i]) + 1;
	}
#ifndef RISCOS
	read_names();
#endif RISCOS
	time(&now);
	nintv = now - boottime;
	if (nintv <= 0 || nintv > 60*60*24*365*10) {
		fprintf(stderr,
		    "Time makes no sense... namelist must be wrong.\n");
		exit(1);
	}
	if (iflag) {
		dointr(nintv);
		exit(0);
	}
	/*
	 * Choose drives to be displayed.  Priority
	 * goes to (in order) drives supplied as arguments,
	 * default drives.  If everything isn't filled
	 * in and there are drives not taken care of,
	 * display the first few that fit.
	 */
	ndrives = 0;
	while (argc > 0 && !isdigit(argv[0][0])) {
		for (i = 0; i < dk_ndrive; i++) {
			if (strcmp(dr_name[i], argv[0]))
				continue;
			dr_select[i] = 1;
			ndrives++;
		}
		argc--, argv++;
	}
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i])
			continue;
		for (cp = defdrives; *cp; cp++)
			if (strcmp(dr_name[i], *cp) == 0) {
				dr_select[i] = 1;
				ndrives++;
				break;
			}
	}
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i])
			continue;
		dr_select[i] = 1;
		ndrives++;
	}
	if (argc > 1)
		iter = atoi(argv[1]);
	signal(SIGCONT, printhdr);
loop:
	if (--lines == 0)
		printhdr();
#ifdef RISCOS
	lseek(mf, (long)nl[X_SYSINFO].n_value, L_SET);
	read(mf, &s.Sysinfo, sizeof s.Sysinfo);
	if (nl[X_CPTIME].n_type != 0) {
		lseek(mf, (long)nl[X_CPTIME].n_value, L_SET);
		read(mf,s.time, sizeof s.time);
	} else {
		s.time[CP_USER] = s.Sysinfo.cpu[CPU_USER];
		s.time[CP_NICE] = 0;
		s.time[CP_SYS] = s.Sysinfo.cpu[CPU_KERNEL];
		s.time[CP_IDLE] = s.Sysinfo.cpu[CPU_IDLE]
				+ s.Sysinfo.cpu[CPU_WAIT]
				+ s.Sysinfo.cpu[CPU_SXBRK];
	};
	lseek(mf, (long)nl[X_ID].n_value, L_SET);
	read(mf, s.Iotime, sizeof s.Iotime);
#else RISCOS
	lseek(mf, (long)nl[X_CPTIME].n_value, L_SET);
 	read(mf, s.time, sizeof s.time);
	lseek(mf, (long)nl[X_DKXFER].n_value, L_SET);
	read(mf, s.xfer, dk_ndrive * sizeof (long));
#endif RISCOS
	if (nintv != 1)
		lseek(mf, (long)nl[X_SUM].n_value, L_SET);
	else
		lseek(mf, (long)nl[X_RATE].n_value, L_SET);
	read(mf, &rate, sizeof rate);
	lseek(mf, (long)nl[X_TOTAL].n_value, L_SET);
	read(mf, &total, sizeof total);
	osum = sum;
	lseek(mf, (long)nl[X_SUM].n_value, L_SET);
	read(mf, &sum, sizeof sum);
	lseek(mf, (long)nl[X_DEFICIT].n_value, L_SET);
	read(mf, &deficit, sizeof deficit);
	etime = 0;
	for (i=0; i < dk_ndrive; i++) {
#ifdef RISCOS
#define X(fld) t = s.Iotime[i].fld ; s.Iotime[i].fld -= s1.Iotime[i].fld; \
		s1.Iotime[i].fld = t;
		X(ios.io_ops);
		X(ios.io_misc);
		X(ios.io_qcnt);
		X(ios.io_unlog);
		X(io_bcnt);
		X(io_resp);
		X(io_act);
#undef X
#else RISCOS
		t = s.xfer[i];
		s.xfer[i] -= s1.xfer[i];
		s1.xfer[i] = t;
#endif RISCOS
	}
	for (i=0; i < CPUSTATES; i++) {
		t = s.time[i];
		s.time[i] -= s1.time[i];
		s1.time[i] = t;
		etime += s.time[i];
	}
	if(etime == 0.)
		etime = 1.;
	printf("%2d%2d%2d", total.t_rq, total.t_dw+total.t_pw, total.t_sw);
#if NBPG > 1024
#define pgtok(a) ((a)*(NBPG/1024))
#else
#define pgtok(a) ((a)*NBPG/1024)
#endif
	printf("%6d%6d", pgtok(total.t_avm), pgtok(total.t_free));
	printf("%4d%3d", (rate.v_pgrec - (rate.v_xsfrec+rate.v_xifrec))/nintv,
	    (rate.v_xsfrec+rate.v_xifrec)/nintv);
	printf("%4d", pgtok(rate.v_pgpgin)/nintv);
	printf("%4d%4d%4d%4d", pgtok(rate.v_pgpgout)/nintv,
	    pgtok(rate.v_dfree)/nintv, pgtok(deficit), rate.v_scan/nintv);
	etime /= (float)HZ;
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			stats(i);
	printf("%4d%4d%4d", INTS(rate.v_intr/nintv), rate.v_syscall/nintv,
	    rate.v_swtch/nintv);
	for(i=0; i<CPUSTATES; i++) {
		float f = stat1(i);
		if (i == 0) {		/* US+NI */
			i++;
			f += stat1(i);
		}
		printf("%3.0f", f);
	}
	printf("\n");
	fflush(stdout);
	nintv = 1;
	if (--iter &&argc > 0) {
		sleep(atoi(argv[0]));
		goto loop;
	}
}

printhdr()
{
	register int i, j;

	printf(" procs     memory              page           ");
	i = (ndrives * 3 - 6) / 2;
	if (i < 0)
		i = 0;
	for (j = 0; j < i; j++)
		putchar(' ');
	printf("faults");
	i = ndrives * 3 - 6 - i;
	for (j = 0; j < i; j++)
		putchar(' ');
	printf("               cpu\n");
	printf(" r b w   avm   fre  re at  pi  po  fr  de  sr ");
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			printf("%c%c ", dr_name[i][0], dr_name[i][2]);	
	printf(" in  sy  cs us sy id\n");
	lines = 19;
}

dotimes()
{

#ifndef RISCOS
	lseek(mf, (long)nl[X_REC].n_value, L_SET);
	read(mf, &s.rectime, sizeof s.rectime);
	lseek(mf, (long)nl[X_PGIN].n_value, L_SET);
	read(mf, &s.pgintime, sizeof s.pgintime);
#endif RISCOS
	lseek(mf, (long)nl[X_SUM].n_value, L_SET);
	read(mf, &sum, sizeof sum);
#ifdef RISCOS
	printf("%d reclaims\n", sum.v_pgrec);
#else RISCOS
	printf("%d reclaims, %d total time (usec)\n", sum.v_pgrec, s.rectime);
	printf("average: %d usec / reclaim\n", s.rectime/sum.v_pgrec);
#endif RISCOS
	printf("\n");
#ifdef RISCOS
	printf("%d page ins\n", sum.v_pgin);
#else RISCOS
	printf("%d page ins, %d total time (msec)\n",sum.v_pgin, s.pgintime/10);
	printf("average: %8.1f msec / page in\n", s.pgintime/(sum.v_pgin*10.0));
#endif RISCOS
}

#if defined(tahoe)
#include <tahoe/cpu.h>
#endif

dosum()
{
#ifdef RISCOS
	struct	ncstats	ncstats;
	long nctotal;
#else RISCOS
	struct nchstats nchstats;
	struct xstats xstats;
	long nchtotal;
#endif RISCOS
#if defined(tahoe)
	struct keystats keystats;
#endif

	lseek(mf, (long)nl[X_SUM].n_value, L_SET);
	read(mf, &sum, sizeof sum);
	printf("%9d swap ins\n", sum.v_swpin);
	printf("%9d swap outs\n", sum.v_swpout);
	printf("%9d pages swapped in\n", sum.v_pswpin / CLSIZE);
	printf("%9d pages swapped out\n", sum.v_pswpout / CLSIZE);
	printf("%9d total address trans. faults taken\n", sum.v_faults);
	printf("%9d page ins\n", sum.v_pgin);
	printf("%9d page outs\n", sum.v_pgout);
	printf("%9d pages paged in\n", sum.v_pgpgin);
	printf("%9d pages paged out\n", sum.v_pgpgout);
	printf("%9d sequential process pages freed\n", sum.v_seqfree);
	printf("%9d total reclaims (%d%% fast)\n", sum.v_pgrec,
	    pct(sum.v_fastpgrec, sum.v_pgrec));
	printf("%9d reclaims from free list\n", sum.v_pgfrec);
	printf("%9d intransit blocking page faults\n", sum.v_intrans);
	printf("%9d zero fill pages created\n", sum.v_nzfod / CLSIZE);
	printf("%9d zero fill page faults\n", sum.v_zfod / CLSIZE);
	printf("%9d executable fill pages created\n", sum.v_nexfod / CLSIZE);
	printf("%9d executable fill page faults\n", sum.v_exfod / CLSIZE);
	printf("%9d swap text pages found in free list\n", sum.v_xsfrec);
	printf("%9d inode text pages found in free list\n", sum.v_xifrec);
	printf("%9d file fill pages created\n", sum.v_nvrfod / CLSIZE);
	printf("%9d file fill page faults\n", sum.v_vrfod / CLSIZE);
	printf("%9d pages examined by the clock daemon\n", sum.v_scan);
	printf("%9d revolutions of the clock hand\n", sum.v_rev);
	printf("%9d pages freed by the clock daemon\n", sum.v_dfree / CLSIZE);
	printf("%9d cpu context switches\n", sum.v_swtch);
	printf("%9d device interrupts\n", sum.v_intr);
	printf("%9d software interrupts\n", sum.v_soft);
#ifdef vax
	printf("%9d pseudo-dma dz interrupts\n", sum.v_pdma);
#endif
	printf("%9d traps\n", sum.v_trap);
	printf("%9d system calls\n", sum.v_syscall);
#ifdef RISCOS
	lseek(mf, (long)nl[X_NCSTATS].n_value, 0);
	read(mf, &ncstats, sizeof ncstats);
	nctotal = ncstats.hits + ncstats.misses +
			ncstats.long_look;
	printf("%9d total name lookups", nctotal);
	printf(" (cache hits %d%%)\n",
		pct(ncstats.hits, nctotal));
	printf("%9s toolong %d\n", "",
		ncstats.long_look);
#else RISCOS
	lseek(mf, (long)nl[X_NCHSTATS].n_value, 0);
	read(mf, &nchstats, sizeof nchstats);
	nchtotal = nchstats.ncs_goodhits + nchstats.ncs_badhits +
	    nchstats.ncs_falsehits + nchstats.ncs_miss + nchstats.ncs_long;
	printf("%9d total name lookups", nchtotal);
	printf(" (cache hits %d%% system %d%% per-process)\n",
	    pct(nchstats.ncs_goodhits, nchtotal),
	    pct(nchstats.ncs_pass2, nchtotal));
	printf("%9s badhits %d, falsehits %d, toolong %d\n", "",
	    nchstats.ncs_badhits, nchstats.ncs_falsehits, nchstats.ncs_long);
	lseek(mf, (long)nl[X_XSTATS].n_value, 0);
	read(mf, &xstats, sizeof xstats);
	printf("%9d total calls to xalloc (cache hits %d%%)\n",
	    xstats.alloc, pct(xstats.alloc_cachehit, xstats.alloc));
	printf("%9s sticky %d flushed %d unused %d\n", "",
	    xstats.alloc_inuse, xstats.alloc_cacheflush, xstats.alloc_unused);
	printf("%9d total calls to xfree", xstats.free);
	printf(" (sticky %d cached %d swapped %d)\n",
	    xstats.free_inuse, xstats.free_cache, xstats.free_cacheswap);
#endif RISCOS
#if defined(tahoe)
	lseek(mf, (long)nl[X_CKEYSTATS].n_value, 0);
	read(mf, &keystats, sizeof keystats);
	printf("%9d %s (free %d%% norefs %d%% taken %d%% shared %d%%)\n",
	    keystats.ks_allocs, "code cache keys allocated",
	    pct(keystats.ks_allocfree, keystats.ks_allocs),
	    pct(keystats.ks_norefs, keystats.ks_allocs),
	    pct(keystats.ks_taken, keystats.ks_allocs),
	    pct(keystats.ks_shared, keystats.ks_allocs));
	lseek(mf, (long)nl[X_DKEYSTATS].n_value, 0);
	read(mf, &keystats, sizeof keystats);
	printf("%9d %s (free %d%% norefs %d%% taken %d%% shared %d%%)\n",
	    keystats.ks_allocs, "data cache keys allocated",
	    pct(keystats.ks_allocfree, keystats.ks_allocs),
	    pct(keystats.ks_norefs, keystats.ks_allocs),
	    pct(keystats.ks_taken, keystats.ks_allocs),
	    pct(keystats.ks_shared, keystats.ks_allocs));
#endif
}

doforkst()
{

#ifdef RISCOS
	lseek(mf, (long)nl[X_SYSINFO].n_value, L_SET);
	read(mf, &sysinfo_, sizeof sysinfo_);
	printf("%d forks\n",
		sysinfo_.sysfork);
#else RISCOS
	lseek(mf, (long)nl[X_FORKSTAT].n_value, L_SET);
	read(mf, &forkstat, sizeof forkstat);
	printf("%d forks, %d pages, average=%.2f\n",
		forkstat.cntfork, forkstat.sizfork,
		(float) forkstat.sizfork / forkstat.cntfork);
	printf("%d vforks, %d pages, average=%.2f\n",
		forkstat.cntvfork, forkstat.sizvfork,
		(float)forkstat.sizvfork / forkstat.cntvfork);
#endif RISCOS
}

stats(dn)
{

	if (dn >= dk_ndrive) {
		printf("  0");
		return;
	}
#ifdef RISCOS
	printf("%3.0f", s.Iotime[dn].ios.io_ops/etime);
#else RISCOS
	printf("%3.0f", s.xfer[dn]/etime);
#endif RISCOS
}

double
stat1(row)
{
	double t;
	register i;

	t = 0;
	for(i=0; i<CPUSTATES; i++)
		t += s.time[i];
	if(t == 0.)
		t = 1.;
	return(s.time[row]*100./t);
}

pct(top, bot)
{

	if (bot == 0)
		return (0);
	return ((top * 100) / bot);
}

dointr(nintv)
{
#ifdef RISCOS
	int	inttotal;

	lseek(mf, (long)nl[X_SYSINFO].n_value, L_SET);
	read(mf,&sysinfo_, sizeof(sysinfo_));
	lseek(mf, (long)nl[X_SUM].n_value,L_SET);
	read(mf,&s.Sum,sizeof(s.Sum));

	printf("interrupt      total      rate\n");
	inttotal = 0;
	if (s.Sum.v_intr > 0) {
		printf("%-12s %8ld %8ld\n", "device",
			s.Sum.v_intr, s.Sum.v_intr / nintv);
		inttotal += s.Sum.v_intr;
	};
	if (s.Sum.v_soft > 0) {
		printf("%-12s %8ld %8ld\n", "software",
			s.Sum.v_soft, s.Sum.v_soft / nintv);
		inttotal += s.Sum.v_soft;
	};
#else RISCOS
	int nintr, inttotal;
	long *intrcnt;
	char *intrname, *malloc();

	nintr = (nl[X_EINTRCNT].n_value - nl[X_INTRCNT].n_value) / sizeof(long);
	intrcnt = (long *) malloc(nl[X_EINTRCNT].n_value -
		nl[X_INTRCNT].n_value);
	intrname = malloc(nl[X_EINTRNAMES].n_value - nl[X_INTRNAMES].n_value);
	if (intrcnt == NULL || intrname == NULL) {
		fprintf(stderr, "vmstat: out of memory\n");
		exit(9);
	}
	lseek(mf, (long)nl[X_INTRCNT].n_value, L_SET);
	read(mf, intrcnt, nintr * sizeof (long));
	lseek(mf, (long)nl[X_INTRNAMES].n_value, L_SET);
	read(mf, intrname, nl[X_EINTRNAMES].n_value - nl[X_INTRNAMES].n_value);
	printf("interrupt      total      rate\n");
	inttotal = 0;
	while (nintr--) {
		if (*intrcnt)
			printf("%-12s %8ld %8ld\n", intrname,
			    *intrcnt, *intrcnt / nintv);
		intrname += strlen(intrname) + 1;
		inttotal += *intrcnt++;
	}
#endif RISCOS
	printf("Total        %8ld %8ld\n", inttotal, inttotal / nintv);
}

#ifdef RISCOS
/*
 * These names must be kept in sync with
 * the types defined in <sys/kmem.h>.
 */
char *kmemnames[] = {
	"misc",		/* M_MISC */
	"mbuf",		/* M_MBUF */
	"devbuf",	/* M_DEVBUF */
	"socket",	/* M_SOCKET */
	"pcb",		/* M_PCB */
	"routetbl",	/* M_RTABLE */
	"hosttbl",	/* M_HTABLE */
	"fragtbl",	/* M_FTABLE */
	"zombie",	/* M_ZOMBIE */
	"ifaddr",	/* M_IFADDR */
	"soopts",	/* M_SOOPTS */
	"soname",	/* M_SONAME */
	"namei",	/* M_NAMEI */
	"gprof",	/* M_GPROF */
	"ioctlops",	/* M_IOCTLOPS */
	"superblk",	/* M_SUPERBLK */
	"cred",		/* M_CRED */
	"buf",		/* M_BUF */
	"streams",	/* M_STREAMS */
	"pte",		/* M_PTE */
	"pt_list",	/* M_PT_LIST */
	"quota",	/* M_QUOTA */
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	"temp",		/* M_TEMP */
};
#else RISCOS
/*
 * These names must be kept in sync with
 * the types defined in <sys/malloc.h>.
 */
char *kmemnames[] = {
	"free",		/* M_FREE */
	"mbuf",		/* M_MBUF */
	"devbuf",	/* M_DEVBUF */
	"socket",	/* M_SOCKET */
	"pcb",		/* M_PCB */
	"routetbl",	/* M_RTABLE */
	"hosttbl",	/* M_HTABLE */
	"fragtbl",	/* M_FTABLE */
	"zombie",	/* M_ZOMBIE */
	"ifaddr",	/* M_IFADDR */
	"soopts",	/* M_SOOPTS */
	"soname",	/* M_SONAME */
	"namei",	/* M_NAMEI */
	"gprof",	/* M_GPROF */
	"ioctlops",	/* M_IOCTLOPS */
	"superblk",	/* M_SUPERBLK */
	"cred",		/* M_CRED */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	"temp",		/* M_TEMP */
};
#endif RISCOS

domem()
{
	struct kmemstats kmemstats[M_LAST];
#ifdef  RISCOS
	int	maxbucket;
	struct	kmembuckets *buckets;
#else RISCOS
	struct kmembuckets buckets[MINBUCKET + 16];
#endif RISCOS
	register struct kmembuckets *kp;
	register struct kmemstats *ks;
	int i;

#ifdef RISCOS
	lseek(mf, (long)nl[X_KMEMMAXBUCKETS].n_value, L_SET);
	read(mf, &maxbucket, sizeof(maxbucket));
	buckets = (struct kmembuckets *) malloc(sizeof(struct kmembuckets) * maxbucket);
	if (buckets == NULL) {
		fprintf(stderr,"vmstat: cannot allocate buffer\n");
		return;
	};
	lseek(mf, (long)nl[X_KMEMBUCKETS].n_value, L_SET);
	read(mf, buckets, sizeof(struct kmembuckets) * maxbucket);
#else RISCOS
	lseek(mf, (long)nl[X_KMEMBUCKETS].n_value, L_SET);
	read(mf, buckets, sizeof buckets);
#endif RISCOS
	printf("Memory statistics by bucket size\n");
	printf("    Size   In Use   Free   Requests  HighWater\n");
#ifdef RISCOS
	for (i = 0, kp = &buckets[i]; i < maxbucket; i++, kp++) {
		if (kp->kb_calls == 0)
			continue;
		printf("%8d%9d%7d%11d%8d%11d\n", kp->kb_elmsz,
			kp->kb_total - kp->kb_totalfree,
			kp->kb_totalfree, kp->kb_calls,
			kp->kb_highwat);
		
	}
#else RISCOS
	for (i = MINBUCKET, kp = &buckets[i]; i < MINBUCKET + 16; i++, kp++) {
		if (kp->kb_calls == 0)
			continue;
		printf("%8d%9d%7d%11d%8d%11d\n", 1 << i, 
			kp->kb_total - kp->kb_totalfree,
			kp->kb_totalfree, kp->kb_calls,
			kp->kb_highwat, kp->kb_couldfree);
		
	}
#endif RISCOS
	lseek(mf, (long)nl[X_KMEMSTAT].n_value, L_SET);
	read(mf, kmemstats, sizeof kmemstats);
	printf("Memory statistics by type\n");
	printf("     Type   In Use  MemUse   HighUse  Limit  Requests %s\n",
		"TypeLimit KernLimit");
	for (i = 0, ks = &kmemstats[0]; i < M_LAST; i++, ks++) {
		if (ks->ks_calls == 0)
			continue;
		printf("%10s%7d%8dK%9dK%6dK%9d%7d%10d\n",
			kmemnames[i] ? kmemnames[i] : "undefined",
			ks->ks_inuse, (ks->ks_memuse + 1023) / 1024,
			(ks->ks_maxused + 1023) / 1024,
			(ks->ks_limit + 1023) / 1024, ks->ks_calls,
			ks->ks_limblocks, ks->ks_mapblocks);
	}
}

#define steal(where, var) \
	lseek(mf, where, L_SET); read(mf, &var, sizeof var);
/*
 * Read the drive names out of kmem.
 */
#ifdef vax
#include <vaxuba/ubavar.h>
#include <vaxmba/mbavar.h>

read_names()
{
	struct mba_device mdev;
	register struct mba_device *mp;
	struct mba_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;
	struct uba_device udev, *up;
	struct uba_driver udrv;

	mp = (struct mba_device *) nl[X_MBDINIT].n_value;
	up = (struct uba_device *) nl[X_UBDINIT].n_value;
	if (up == 0) {
		fprintf(stderr, "vmstat: Disk init info not in namelist\n");
		exit(1);
	}
	if (mp) for (;;) {
		steal(mp++, mdev);
		if (mdev.mi_driver == 0)
			break;
		if (mdev.mi_dk < 0 || mdev.mi_alive == 0)
			continue;
		steal(mdev.mi_driver, mdrv);
		steal(mdrv.md_dname, two_char);
		sprintf(dr_name[mdev.mi_dk], "%c%c%d",
		    cp[0], cp[1], mdev.mi_unit);
	}
	for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c%d",
		    cp[0], cp[1], udev.ui_unit);
	}
}
#endif

#ifdef tahoe
#include <tahoevba/vbavar.h>

/*
 * Read the drive names out of kmem.
 */
read_names()
{
	struct vba_device udev, *up;
	struct vba_driver udrv;
	short two_char;
	char *cp = (char *)&two_char;

	up = (struct vba_device *) nl[X_VBDINIT].n_value;
	if (up == 0) {
		fprintf(stderr, "vmstat: Disk init info not in namelist\n");
		exit(1);
	}
	for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c%d",
		     cp[0], cp[1], udev.ui_unit);
	}
}
#endif


