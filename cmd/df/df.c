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
#ident	"$Header: df.c,v 1.14.1.3 90/05/09 15:42:18 wje Exp $"

/*
 * df(1) - report disk free block counts and other statistics
 */
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <sun/mntent.h>
#include <string.h>
#include <sys/types.h>
#include <sys/fsid.h>
#include <sys/nami.h>
#include <sys/stat.h>

#include <sys/buf.h>

#define BBTOK(bb)	((bb) >> 1)

#include <sys/fstyp.h>
#include <sys/statfs.h>

#ifdef DEBUG
char	MTAB[] = "./mtab";
#else
char	MTAB[] = MOUNTED;
#endif

void	put_head(/* void */);
void	put_line(/* mntp */);

char	verify_hostup = 0;		/* verify that NFS host is up */
char	do_free_scan = 0;		/* force scan of freelist */
char	do_inode_stats = 0;		/* print used and free file counts */
char	halfK_units = 0;		/* print in half-K units */

int	types = 0;			/* print info only on specified types */

int
main(argc, argv)
	register int argc;
	register char **argv;
{
	int c;
	extern int optind;
	extern char *optarg;
	register struct mntent *mntp;
	register FILE *mtabp;

	char *basename, *devname, *devnm();
	struct stat stb;
	int errors = 0;
	
	basename = strrchr(argv[0], '/');
	if (basename == NULL)
		basename = argv[0];
	else
		basename++;
	if (!strncmp(basename, "devnm", 5)) {
		while (--argc > 0) {
			if (stat(*++argv, &stb) < 0) {
				fprintf(stderr, "devnm: cannot stat %s\n",
				    *argv);
				errors++;
				continue;
			}
			devname = devnm(*argv, stb.st_dev);
			if (devname != NULL)
				printf("%s %s\n", devname, *argv);
			else {
				printf("devnm: %s not found\n", *argv);
				errors++;
			}
		}
		exit (errors);
	}

	while ((c = getopt(argc, argv, "kbfqiut:")) != EOF) {
		switch (c) {

			case 'k':
				break;

			case 'b':
				halfK_units++;
				break;

			case 'f':
				do_free_scan++;
				break;

			case 'q':
				break;
			
			case 'i':
				do_inode_stats++;
				break;

			case 't':
				types++;
				add_types(optarg);
				break;

			case 'u':
				verify_hostup++;
				break;

			default:
				usage();
				exit(2);
		}
	}
	if (optind == argc) {
		all_entries();

		exit(0);
	}

	if (types) {
		usage();
		exit(2);
	}

	get_mtab();
	put_head();
	while (optind < argc) {
		find_entry(argv[optind]);
		optind++;
	}
	exit (0);
}

int
usage()
{
	fprintf(stderr, "usage: df [-bfikqu] [-t types | filesystem ...]\n");
}

/*
 * The header layout depends on whether do_inode_stats is enabled (the vertical
 * bars indicate where the fields start):

Filesystem       Type  kbytes     use   avail %use   iuse  ifree %use Mounted on
|                 |  |       |       |       |    |      |      |     |
Filesystem                 Type  kbytes    use  avail %use  Mounted on
|                           |  |       |      |      |      |
123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789

 */

void
put_head()
{
	printf("Filesystem       ");
	if (!do_inode_stats)
		printf("          ");
	printf("Type  kbytes     use   avail %%use");
	if (do_inode_stats) {
		printf("   iuse  ifree %%use");
	} else {
		printf(" ");	/* if shorter line, more whitespace */
	}
	printf(" Mounted on\n");
}

#define PERCENT(amount, total) \
	(((amount) * 100 + (total) / 2) / (total))
#define	ZERO_PERCENT(amt, tot) \
	((amt) = 0, (tot) = 1)

void
put_line(mntp)
	register struct mntent *mntp;
{
	struct statfs sfsb;
	register long kbytes;
	register long used;

	/*
	 * Get filesystem statistics.  If do_free_scan, get free counts
	 * the hard way.
	 */
	if (stat_entry(mntp, &sfsb) != 0
	    || do_free_scan && scan_freelist(mntp, &sfsb) != 0) {
		return;
	}

	/*
	 * Compute size in kbytes and scale used and avail counts.
	 * Statfs returns counts in 512-byte units.
	 */
	kbytes = BBTOK(sfsb.f_blocks);
	if (! halfK_units) {
		sfsb.f_bfree = BBTOK(sfsb.f_bfree);
		sfsb.f_blocks = kbytes;
	}

	/*
	 * Print the new line, calculating percentages.
	 */
	if (sfsb.f_blocks > 0) {
		used = sfsb.f_blocks - sfsb.f_bfree;
	} else {
		ZERO_PERCENT(used, sfsb.f_blocks);
	}
	printf(do_inode_stats ? "%-17s" : "%-27.26s", mntp->mnt_fsname);
	if (strlen(mntp->mnt_type) > 3) {
		/* long name */
		printf("%-4s",mntp->mnt_type);
	} else {
		printf(" %-3s", mntp->mnt_type);
	}
	printf("%8ld%8ld%8ld%4ld%%", kbytes, used,
	    sfsb.f_bfree, PERCENT(used, sfsb.f_blocks));
	if (do_inode_stats) {
		register long iused;

		if (sfsb.f_files > 0) {
			iused = sfsb.f_files - sfsb.f_ffree;
		} else {
			ZERO_PERCENT(iused, sfsb.f_files);
		}
		printf("%7ld%7ld%4ld%%", iused, sfsb.f_ffree,
		    PERCENT(iused, sfsb.f_files));
	} else {
		printf(" ");
	}
	if (mntp->mnt_dir != NULL) {
		printf(" %s", mntp->mnt_dir);
	} else {
		printf("  %-.6s", sfsb.f_fname);
	}
	putchar('\n');
}

int
df_perror(s1, s2)
	char *s1, *s2;
{
	if (*s1) {
		fprintf(stderr, "df: %s ", s1);
	} else {
		fprintf(stderr, "df: ");
	}
	perror(s2);
	return errno;
}
