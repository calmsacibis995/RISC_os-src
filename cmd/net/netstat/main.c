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
#ident	"$Header: main.c,v 1.4.1.5.1.2.1.1 90/10/16 11:45:49 beacker Locked $"

#include <sys/param.h>
#include <sys/vmmac.h>
#include <sys/socket.h>
#include <machine/pte.h>
#include <ctype.h>
#include <errno.h>
#include <netdb.h>
#include <nlist.h>
#include <stdio.h>
#ifdef RISCOS
#include <sys/file.h>                /* open file entry structure */
#include "libutil.h"
#endif RISCOS
#include <net/if.h>
#include <sys/ioctl.h>
#include <mipsif/if_enp.h>
typedef unsigned char	UBYTE;
typedef unsigned short	UWORD;
typedef unsigned long	ULONG;
typedef unsigned short Bit;
#include <sysv/sys/if_egl.h>

struct nlist nl[] = {
#define	N_MBSTAT	0
	{ "_mbstat" },
#define	N_IPSTAT	1
	{ "_ipstat" },
#define	N_TCB		2
	{ "_tcb" },
#define	N_TCPSTAT	3
	{ "_tcpstat" },
#define	N_UDB		4
	{ "_udb" },
#define	N_UDPSTAT	5
	{ "_udpstat" },
#define	N_RAWCB		6
	{ "_rawcb" },
#define	N_SYSMAP	7
	{ "kptbl" },
#define	N_SYSSIZE	8
	{ "syssegsz" },
#define	N_IFNET		9
	{ "_ifnet" },
#define	N_HOSTS		10
	{ "_hosts" },
#define	N_RTHOST	11
	{ "_rthost" },
#define	N_RTNET		12
	{ "_rtnet" },
#define	N_ICMPSTAT	13
	{ "_icmpstat" },
#define	N_RTSTAT	14
	{ "_rtstat" },
#ifdef RISCOS
#define	N_V		15
	{ "_v" },   /* System Configuration structure */
#else
#define	N_NFILE		15
	{ "_nfile" },
#endif RISCOS
#define	N_FILE		16
	{ "_file" },
#define	N_UNIXSW	17
	{ "_unixsw" },
#define N_RTHASHSIZE	18
	{ "_rthashsize" },
#define N_IDP		19
	{ "_nspcb"},
#define N_IDPSTAT	20
	{ "_idpstat"},
#define N_SPPSTAT	21
	{ "_spp_istat"},
#define N_NSERR		22
	{ "_ns_errstat"},
#ifdef RISCOS
#define N_ID_STRING	23
	{ "id_string" },
#define N_PAGESIZE	24
	{ "pagesize" },
#endif RISCOS
	0
};

/* internet protocols */
extern	int protopr();
extern	int tcp_stats(), udp_stats(), ip_stats(), icmp_stats();
extern	int nsprotopr();
extern	int spp_stats(), idp_stats(), nserr_stats();
extern	int enp_stats();

struct protox {
	u_char	pr_index;		/* index into nlist of cb head */
	u_char	pr_sindex;		/* index into nlist of stat block */
	u_char	pr_wanted;		/* 1 if wanted, 0 otherwise */
	int	(*pr_cblocks)();	/* control blocks printing routine */
	int	(*pr_stats)();		/* statistics printing routine */
	char	*pr_name;		/* well-known name */
} protox[] = {
	{ N_TCB,	N_TCPSTAT,	1,	protopr,
	  tcp_stats,	"tcp" },
	{ N_UDB,	N_UDPSTAT,	1,	protopr,
	  udp_stats,	"udp" },
	{ -1,		N_IPSTAT,	1,	0,
	  ip_stats,	"ip" },
	{ -1,		N_ICMPSTAT,	1,	0,
	  icmp_stats,	"icmp" },
	{ -1,		-1,		0,	0,
	  0,		0 }
};

struct protox nsprotox[] = {
	{ N_IDP,	N_IDPSTAT,	1,	nsprotopr,
	  idp_stats,	"idp" },
	{ N_IDP,	N_SPPSTAT,	1,	nsprotopr,
	  spp_stats,	"spp" },
	{ -1,		N_NSERR,	1,	0,
	  nserr_stats,	"ns_err" },
	{ -1,		-1,		0,	0,
	  0,		0 }
};

struct	pte *Sysmap;

#ifdef RISCOS
char	*system = "/unix";
#else RISCOS
char	*system = "/vmunix";
#endif RISCOS
char	*kmemf = "/dev/kmem";
int	kmem;
int	kflag;
int	Aflag;
int	aflag;
int	hflag;
int	iflag;
int	mflag;
int	nflag;
int	rflag;
int	sflag;
int	tflag;
int	fflag;
int	eflag;
int	interval;
char	*interface;
int	unit;
char	usage[] = "[ -Aaeihmnrst ] [-f address_family] [-I interface] [ interval ] [ system ] [ core ]";

int	af = AF_UNSPEC;
char	iface[128];

main(argc, argv)
	int argc;
	char *argv[];
{
	int i;
	char *cp, *name;
	register struct protoent *p;
	register struct protox *tp;

	name = argv[0];
	argc--, argv++;
  	while (argc > 0 && **argv == '-') {
		for (cp = &argv[0][1]; *cp; cp++)
		switch(*cp) {

		case 'A':
			Aflag++;
			break;

		case 'a':
			aflag++;
			break;

		case 'h':
			hflag++;
			break;

		case 'i':
			iflag++;
			break;

		case 'm':
			mflag++;
			break;

		case 'n':
			nflag++;
			break;

		case 'r':
			rflag++;
			break;

		case 's':
			sflag++;
			break;

		case 't':
			tflag++;
			break;

		case 'u':
			af = AF_UNIX;
			break;

		case 'e':
			eflag++;
			argv++;
			argc--;
			if (!argc) {
				fprintf(stderr,"usage: %s %s\n", name, usage);
				exit(1);
			}
			strcpy(iface, *argv);
			break;

		case 'f':
			argv++;
			argc--;
#ifdef RISCOS
			if ( argc <= 0)
			{
			/* not enough args, punt */
			    fprintf(stderr,"netstat: missing argument for -f\n");
			    fprintf(stderr,"usage: %s %s\n", name, usage);
			    exit(1);
			}
#endif
			if (strcmp(*argv, "ns") == 0)
				af = AF_NS;
			else if (strcmp(*argv, "inet") == 0)
				af = AF_INET;
			else if (strcmp(*argv, "unix") == 0)
				af = AF_UNIX;
			else {
				fprintf(stderr, "%s: unknown address family\n",
					*argv);
				exit(10);
			}
			break;
			
		case 'I':
#ifdef RISCOS
			if (argc <= 1)
			{
			    fprintf(stderr,"netstat: missing argument for -I\n");
			    fprintf(stderr,"usage: %s %s\n", name, usage);
			    exit(1);
			}
#endif
			iflag++;
			if (*(interface = cp + 1) == 0) {
				if ((interface = argv[1]) == 0)
					break;
				argv++;
				argc--;
			}
			for (cp = interface; isalpha(*cp); cp++)
				;
			unit = atoi(cp);
			*cp-- = 0;
			break;

		default:
use:
			fprintf(stderr,"usage: %s %s\n", name, usage);
			exit(1);
		}
		argv++, argc--;
	}
	if (argc > 0 && isdigit(argv[0][0])) {
		interval = atoi(argv[0]);
		if (interval <= 0)
			goto use;
		argv++, argc--;
		iflag++;
	}
	if (argc > 0) {
		system = *argv;
		argv++, argc--;
	}
	nlist(system, nl);
	if (nl[0].n_type == 0) {
		fprintf(stderr, "%s: no namelist\n", system);
		exit(1);
	}

	if (argc > 0) {
		kmemf = *argv;
		kflag++;
	}
	kmem = open(kmemf, 0);
	if (kmem < 0) {
		fprintf(stderr, "cannot open ");
		perror(kmemf);
		exit(1);
	}
#ifdef RISCOS
	/* Check to ensure that the kernel id strings match */
	if (check_kernel_id (kmem,
			(long)nl[N_ID_STRING].n_value, system) > 0) {
		fprintf(stderr,"%s does not match %s\n",system,kmemf);
		exit(1);
	};
#endif RISCOS
	if (kflag) {

		initkmem(kmem, nl[N_SYSSIZE].n_value, nl[N_SYSMAP].n_value,
			nl[N_PAGESIZE].n_value);
	}
	if (eflag) {
		ifstat_dump();
		exit(0);
	}
	if (mflag) {
		mbpr(nl[N_MBSTAT].n_value);
		exit(0);
	}
	/*
	 * Keep file descriptors open to avoid overhead
	 * of open/close on each call to get* routines.
	 */
	sethostent(1);
	setnetent(1);
	if (iflag) {
		intpr(interval, nl[N_IFNET].n_value);
		exit(0);
	}
	if (hflag) {
		hostpr(nl[N_HOSTS].n_value);
		exit(0);
	}
	if (rflag) {
		if (sflag)
			rt_stats(nl[N_RTSTAT].n_value);
		else
			routepr(nl[N_RTHOST].n_value, nl[N_RTNET].n_value,
				nl[N_RTHASHSIZE].n_value);
		exit(0);
	}
    if (af == AF_INET || af == AF_UNSPEC) {
	setprotoent(1);
	setservent(1);
	while (p = getprotoent()) {

		for (tp = protox; tp->pr_name; tp++)
			if (strcmp(tp->pr_name, p->p_name) == 0)
				break;
		if (tp->pr_name == 0 || tp->pr_wanted == 0)
			continue;
		if (sflag) {
			if (tp->pr_stats)
				(*tp->pr_stats)(nl[tp->pr_sindex].n_value,
					p->p_name);
		} else
			if (tp->pr_cblocks)
				(*tp->pr_cblocks)(nl[tp->pr_index].n_value,
					p->p_name);
	}
	endprotoent();
    }
    if (af == AF_NS || af == AF_UNSPEC) {
	for (tp = nsprotox; tp->pr_name; tp++) {
		if (sflag) {
			if (tp->pr_stats)
				(*tp->pr_stats)(nl[tp->pr_sindex].n_value,
					tp->pr_name);
		} else
			if (tp->pr_cblocks)
				(*tp->pr_cblocks)(nl[tp->pr_index].n_value,
					tp->pr_name);
	}
    }

    if ((af == AF_UNIX || af == AF_UNSPEC) && !sflag)
#ifdef RISCOS
	unixpr (nl[N_V].n_value, nl[N_FILE].n_value,
		nl[N_UNIXSW].n_value);
#else RISCOS
	unixpr(nl[N_NFILE].n_value,nl[N_FILE].n_value,nl[N_UNIXSW].n_value);
#endif RISCOS

    exit(0);
}

char *
plural(n)
	int n;
{

	return (n != 1 ? "s" : "");
}




/*
 * A little tid bit to dump the guts of the ENP-10.  We need this to
 * get a better handle on network performance.
 */

int
ifstat_dump()
{
	int s;
	struct {
		struct enpstat board_stats;
		struct es_stats driver_stats;
		EGL_CSB eglstat;
		} enpstat;
	register struct enpstat *bs = &enpstat.board_stats;
	register struct es_stats *ds = &enpstat.driver_stats;
	extern int errno;
	struct ifreq ifr;

	errno = 0;
	ifr.ifr_data = (caddr_t)&enpstat;
	strcpy(ifr.ifr_name, iface);
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (errno)
		{
		perror("Socket failed on");
		exit(1);
		}
	ioctl(s, SIOCGIFSTATS, &ifr);
	if (errno)
		{
		perror("Ioctl failed on");
		exit(1);
		}
	printf("Interface Statistics Dump For %s\n", iface);
	switch (iface[1]) {
		case 'n':
			/* enp */
		printf("successful transmit\t%d\n", bs->e_xmit_successful);
		printf("multiple retries\t%d\n", bs->e_mult_retry);
		printf("single retries\t\t%d\n", bs->e_one_retry);
		printf("failed retries\t\t%d\n", bs->e_fail_retry);
		printf("deferred transmits\t%d\n", bs->e_deferrals);
		printf("transmit buf error\t%d\n", bs->e_xmit_buff_err);
		printf("silo underrun\t\t%d\t", bs->e_silo_underrun);
		printf("silo overrun\t\t%d\n", bs->e_silo_overrun);
		printf("late collisions\t\t%d\n", bs->e_late_coll);
		printf("lost carrier\t\t%d\n", bs->e_lost_carrier);
		printf("babbling node\t\t%d\n", bs->e_babble);
		printf("no hearbeat\t\t%d\n", bs->e_no_heartbeat);
		printf("transmit mem error\t%d\n", bs->e_xmit_mem_err);
		printf("successful receive\t%d\n", bs->e_rcv_successful);
		printf("dropped packets\t\t%d\n", bs->e_rcv_missed);
		printf("crc error\t\t%d\n", bs->e_crc_err);
		printf("frame error\t\t%d\n", bs->e_frame_err);
		printf("receive buf errror\t%d\n", bs->e_rcv_buff_err);
		printf("receive mem error\t%d\n", bs->e_rcv_mem_err);
		printf("rcv controller error\t%d\n", ds->ictlrerr);
		printf("rcv bad trailer1\t%d\t", ds->ibadtrailer);
		printf("rcv bad trailer2\t%d\n", ds->ibadtrailer2);
		printf("no rcv mbufs\t\t%d\t", ds->inombufs);
		printf("no xmit mbufs\t\t%d\n", ds->onombufs);
		printf("rcv queue full\t\t%d\n", ds->iqfull);
			break;
		case 'g':
			/* egl */
#ifdef notdef
		printf("LANCE Interrupts\t\t%d\n", enpstat.eglstat.csb_NLINT);
		printf("Attempted Transmissions\t\t%d\n", enpstat.eglstat.csb_NATX);
		printf("Transmissions\t\t\t%d\n", enpstat.eglstat.csb_NTX);
		printf("Multiple Retries\t\t%d\n", enpstat.eglstat.csb_NMR);
		printf("Single Retries\t\t\t%d\n", enpstat.eglstat.csb_NSR);
		printf("Retry Failures\t\t\t%d\n", enpstat.eglstat.csb_NRF);
		printf("Deferrals\t\t\t%d\n", enpstat.eglstat.csb_NDEF);
		printf("Transmit Buffer Errors\t\t%d\n", enpstat.eglstat.csb_NTBE);
		printf("SILO Underruns\t\t\t%d\n", enpstat.eglstat.csb_NSLU);
		printf("Late Collisions\t\t\t%d\n", enpstat.eglstat.csb_NLTC);
		printf("Carrier Losses\t\t\t%d\n", enpstat.eglstat.csb_NCLS);
		printf("Babbling Xmitter\t\t%d\n", enpstat.eglstat.csb_NBTX);
		printf("Collisions\t\t\t%d\n", enpstat.eglstat.csb_NCOL);
		printf("Transmit Memory Errors\t\t%d\n", enpstat.eglstat.csb_NTME);
		printf("Transmit Ring Owner Errors\t%d\n", enpstat.eglstat.csb_TROWN);
		printf("Attempted Receptions\t\t%d\n", enpstat.eglstat.csb_NATR);
		printf("Messages Received\t\t%d\n", enpstat.eglstat.csb_NRCV);
		printf("Missed Messages\t\t\t%d\n", enpstat.eglstat.csb_NMIS);
		printf("CRC Errors\t\t\t%d\n", enpstat.eglstat.csb_CRC);
		printf("Framing Errors\t\t\t%d\n", enpstat.eglstat.csb_FRAM);
		printf("Receive Buffer Errors\t\t%d\n", enpstat.eglstat.csb_NRBE);
		printf("Silo Overruns\t\t\t%d\n", enpstat.eglstat.csb_NSLO);
		printf("Receive Memory Errors\t\t%d\n", enpstat.eglstat.csb_NRME);
		printf("Revceive Ring Owner Errors\t%d\n", enpstat.eglstat.csb_RROWN);
#endif notdef
			break;
		}
}


klseek(fd, base, off) 
int	fd, base, off;
{
	kmem_lseek(fd, base, off, kflag);
}

