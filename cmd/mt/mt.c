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
#ident	"$Header: mt.c,v 1.5.2.5.1.1.1.2 90/10/05 10:04:04 beacker Exp $"

static	char *sccsid = "@(#)mt.c	4.8 (Berkeley) 83/05/08";
/*
 * mt --
 *   magnetic tape manipulation program
 */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/mtio.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>
/*
 * The following includes are for buf.h
 */
#include <time.h>
#include <sys/sbd.h>
#include <sys/sema.h>
#include <sys/fstyp.h>
#include <sys/mount.h>
#include <sys/comm.h>
#include <sys/inode.h>
#include <sys/immu.h>
#include <sys/region.h>
#include <sys/proc.h>
/*
 * buf.h is included because tsreg.h uses struct buf
 */
#include <sys/buf.h>

#define	equal(s1,s2)	(strcmp(s1, s2) == 0)

struct commands {
	char *c_name;
	int c_code;
	int c_ronly;
	int c_spl;
} com[] = {
	{ "weof",	MTWEOF,	0 ,0},
	{ "eof",	MTWEOF,	0 ,0},
	{ "fsf",	MTFSF,	1 ,0},
	{ "bsf",	MTBSF,	1 ,0},
	{ "fsr",	MTFSR,	1 ,0},
	{ "bsr",	MTBSR,	1 ,0},
	{ "rewind",	MTREW,	1 ,0},
	{ "offline",	MTOFFL,	1 ,0},
	{ "rewoffl",	MTOFFL,	1 ,0},
	{ "status",	MTNOP,	1 ,1},
	{ "ret",	MTRET,	1 ,0},
	{ "online",	MTONL,	1 ,1},
	{ "append",	MTAPP,	1 ,1},
	{ 0 }
};

int mtfd;
struct mtop mt_com;
struct mtget mt_status;
char *tape;
char	*rmthostname;
char	*rmtprogramname = "mt";
int	nblock = 1;

main(argc, argv)
	char **argv;
{
	char line[80], *getenv();
	register char *cp;
	register struct commands *comp;
	int mode;

	if (argc > 2 && (equal(argv[1], "-N"))) {
		argc -= 2;
		rmthostname = argv[2];
		argv +=2;
	};
	if (argc > 2 && (equal(argv[1], "-t") || equal(argv[1], "-f"))) {
		argc -= 2;
		tape = argv[2];
		argv += 2;
	} else
		if ((tape = getenv("TAPE")) == NULL)
			tape = DEFTAPE;
	if (argc > 2 && (equal(argv[1], "-N"))) {
		argc -= 2;
		rmthostname = argv[2];
		argv +=2;
	};
#ifdef notdef
	if (rmthostname == NULL 
		rmthostname = getenv("RMTTAPEHOST");
#endif notdef
	if (argc < 2) {
		fprintf(stderr, "usage: mt [ -N host ] [ -f device ] command [ count ]\n");
		exit(1);
	}
	cp = argv[1];
	for (comp = com; comp->c_name != NULL; comp++)
		if (strncmp(cp, comp->c_name, strlen(cp)) == 0)
			break;
	if (comp->c_name == NULL) {
		fprintf(stderr, "mt: Unknown command: \"%s\"\n", cp);
		exit(1);
	}
	mode = comp->c_ronly ? O_RDONLY : O_RDWR;
	mode |= comp->c_spl ? O_SYNC : 0;
	if (rmthostname != NULL) {
		if (rmthost(rmthostname) == 0)
		{
			exit(1);
		}
		if ((mtfd = rmtopen(tape, mode)) < 0) {
			exit(1);
		}
		if (comp->c_code != MTNOP) {
			mt_com.mt_op = comp->c_code;
			mt_com.mt_count = (argc > 2 ? atoi(argv[2]) : 1);
			if (mt_com.mt_count < 0) {
				fprintf(stderr, "mt: negative repeat count\n");
				exit(1);
			}
			if (rmtioctl(mtfd, mt_com.mt_op, mt_com.mt_count) < 0) {
				fprintf(stderr, "%s:%s %s %d failed\n", rmthostname, 
					tape, comp->c_name,
					mt_com.mt_count);
				exit(2);
			}
		} else {
			extern struct mtget *rmtstatus();

		  	mt_status = *(rmtstatus());
#ifdef SYSTYPE_SYSV
			mt_status.mt_type ^= 0x8000; 
#endif SYSTYPE_SYSV
			status(&mt_status);
		}
		exit(0);
	};
	if ((mtfd = open(tape, mode)) < 0) {
		perror(tape);
		exit(1);
	}
	if (comp->c_code != MTNOP) {
		mt_com.mt_op = comp->c_code;
		mt_com.mt_count = (argc > 2 ? atoi(argv[2]) : 1);
		if (mt_com.mt_count < 0) {
			fprintf(stderr, "mt: negative repeat count\n");
			exit(1);
		}
		if (ioctl(mtfd, MTIOCTOP, &mt_com) < 0) {
			fprintf(stderr, "%s %s %d ", tape, comp->c_name,
				mt_com.mt_count);
			perror("failed");
			exit(2);
		}
	} else {
		if (ioctl(mtfd, MTIOCGET, (char *)&mt_status) < 0) {
			perror("mt");
			exit(2);
		}
		status(&mt_status);
	}
	exit(0);
}

#ifdef vax
#include <vaxmba/mtreg.h>
#include <vaxmba/htreg.h>

#include <vaxuba/utreg.h>
#include <vaxuba/tmreg.h>
#undef b_repcnt		/* argh */
#include <vaxuba/tsreg.h>
#endif

#ifdef sun
#include <sundev/tmreg.h>
#include <sundev/arreg.h>
#endif

#ifdef mips
#ifdef BSD
#include <mipsvme/tsreg.h>
#else BSD
#include <sys/tsreg.h>
#define JAGS_BITS \
"\20\20FM\17EOM\16RD\15OPN\14WNT\13WAT\12FMT\11SEL\10REW\7WOR\6DSK\5TAP\4WRT\3BSY\2RDY\1ALV"

#define JAGE_BITS \
"\10\10-\7VAR\6NRDY\5ATN\4RO\3NARB\2RMV\1PRT"

#define M120S_BITS \
"\20\20FM\17EOM\16RD\15OPN\14WNT\13WAT\12FMT\11SEL\10REW\7WOR\6DSK\5TAP\4WRT\3BSY\2RDY\1ALV"

#define M120E_BITS \
"\10\10-\7VAR\6NRDY\5ATN\4RO\3NARB\2RMV\1PRT"

#endif BSD
#endif
#ifdef RISCOS
#include <bsd43/sys/mtio.h>
#endif RISCOS

struct tape_desc {
	short	t_type;		/* type of magtape device */
	char	*t_name;	/* printing name */
	char	*t_dsbits;	/* "drive status" register */
	char	*t_erbits;	/* "error" register */
} tapes[] = {
#ifdef vax
	{ MT_ISTS,	"ts11",		0,		TSXS0_BITS },
	{ MT_ISHT,	"tm03",		HTDS_BITS,	HTER_BITS },
	{ MT_ISTM,	"tm11",		0,		TMER_BITS },
	{ MT_ISMT,	"tu78",		MTDS_BITS,	0 },
	{ MT_ISUT,	"tu45",		UTDS_BITS,	UTER_BITS },
#endif
#ifdef sun
	{ MT_ISCPC,	"TapeMaster",	TMS_BITS,	0 },
	{ MT_ISAR,	"Archive",	ARCH_CTRL_BITS,	ARCH_BITS },
#endif
#ifdef mips
	{ MT_ISQIC,	"ISI",		TSXS0_BITS,	TSXS1_BITS },
#ifdef RISCOS
	{ BSD43_MT_ISQIC | 0x8000, "ISI", TSXS0_BITS,	TSXS1_BITS },
	{ MT_M120,	"SCSI",		M120S_BITS,	M120E_BITS },
	{ MT_JAG,	"Interphase",	JAGS_BITS,	JAGE_BITS },
	{ MT_ISXM,	"Xylogics",	0,		0, },
	{ BSD43_MT_ISXY | 0x8000, "Xylogics", 0,	0, },
#endif RISCOS
#endif mips

	{ 0 }
};

/*
 * Interpret the status buffer returned
 */
status(bp)
	register struct mtget *bp;
{
	register struct tape_desc *mt;

	for (mt = tapes; mt->t_type; mt++)
		if (mt->t_type == bp->mt_type)
			break;
	if (mt->t_type == 0) {
		printf("unknown tape drive type (%d)\n", bp->mt_type);
		return;
	}
	printf("%s tape drive, residual=%d\n", mt->t_name, bp->mt_resid);
	printreg("ds", bp->mt_dsreg, mt->t_dsbits);
	printreg("\ner", bp->mt_erreg, mt->t_erbits);
	putchar('\n');
}

/*
 * Print a register a la the %b format of the kernel's printf
 */
printreg(s, v, bits)
	char *s;
	register char *bits;
	register unsigned short v;
{
	register int i, any = 0;
	register char c;

	if (bits && *bits == 8)
		printf("%s=%o", s, v);
	else
		printf("%s=%x", s, v);
	bits++;
	if (v && bits) {
		putchar('<');
		while (i = *bits++) {
			if (v & (1 << (i-1))) {
				if (any)
					putchar(',');
				any = 1;
				for (; (c = *bits) > 32; bits++)
					putchar(c);
			} else
				for (; *bits > 32; bits++)
					;
		}
		putchar('>');
	}
}
