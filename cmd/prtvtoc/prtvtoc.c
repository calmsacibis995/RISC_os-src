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
#ident	"$Header: prtvtoc.c,v 1.12.2.3.1.1.1.5 90/12/05 14:08:29 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * prtvtoc.c
 *
 * Print a disk partition map ("VTOC"). Avoids the standard
 * I/O library to conserve first-floppy space.
 * NOTE:
 *	No longer conserving space.  This program will be run on
 *	a real machine with real disk and no floppies.
 */

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dvh.h>
#include <errno.h>
#include <sys/sysmacros.h>
#include <sys/dkio.h>
#include <sys/param.h>
#include <dirent.h>
#include <sys/map.h>
#include <sys/swap.h>
#include <sys/immu.h>
#include <sys/sysmips.h>
#include <bsd/sys/time.h>
#include <sys/vnode.h>
#include <bsd43/mntent.h>

#ifndef DIOCPARTITION
/* This ifdef should be removed later, defined currently in dkio.h */
#define DIOCPARTITION    _DIOC_(36)      /* get partition status */
#endif

/* the following structure is defined in scsi.h */

union {
        struct {
                u_short layer;  /* for virtual disk, etc.  We must keep a
                                larger counter since # open = # close */
                u_char chr:2;   /* number of open as character device */
                u_char  blk:2;  /* # of blk dev open */
                u_char  swp:2;  /* # of swap dev open */
                u_char mnt:2;   /* # of mnt open, treated as blk dev */
                u_char  rootdev:1; /* only used in ioctl */
                u_char  swapdev:1; /* to flag rootdev/swapdev partition */
        } usage;
        u_int status;
} pu[16];


/*
 * Macros.
 */
#define	strsize(str)	\
		(sizeof(str) - 1)	/* Length of static character array */
#define iddn(x)		\
		(x / NPARTAB)		/* Drive number */
#define idslice(x)	\
		(x & (NPARTAB - 1))	/* partition number */

/*
 * Definitions.
 */
#define	reg	register		/* Convenience */
#define	uint	unsigned int		/* Convenience */
#define	ulong	unsigned long		/* Convenience */
#define	ushort	unsigned short		/* Convenience */
#define	DECIMAL	10			/* Numeric base 10 */
#define	FSTABSZ	1024			/* Fstab size limit (plus one) */
#define	HEX	16			/* Numeric base 16 */

/*
 * Disk freespace structure.
 */
typedef struct {
	ulong	fr_start;		/* Start of free space */
	ulong	fr_size;		/* Length of free space */
} Freemap;

/*
 * External functions.
 */
void	exit();
void	qsort();
char	*strcat();
char	*strchr();
char	*strcpy();
char	*strrchr();
char	*strtok();


/*
 * Internal functions.
 */
void	fatal();
Freemap	*findfree();
char	**getmntpt();
int	getopt();
char	*memstr();
int	partcmp();
int	prtvtoc();
void	putfree();
void	puttable();
int	readpd();
int	readvtoc();
char	*syserr();
void	usage();
int	warn();

/*
 * External variables.
 */
extern int	errno;			/* System error code */
extern char	*sys_errlist[];		/* Error messages */
extern int	sys_nerr;		/* Number of sys_errlist[] entries */

/*
 * Static variables.
 */
static short	fflag;			/* Print freespace shell assignments */
static short	hflag;			/* Omit headers */
static short	sflag;			/* Omit all but the column header */
static short	lflag;			/* Life usage */
static short	oflag;			/* Print overlap info */
static char	*fstab = "/etc/fstab";	/* Fstab pathname */
static char	*myname;		/* Last qualifier of arg0 */
static int	optind = 1;		/* Argument index */
static char	*optarg;		/* Option argument */

unsigned char mounted[16];
char *mount_point[16];
struct stat sb;
dev_t current_target;
#define TARGET(x) ((x) & ~0xf)
#define PART(x) ((x) & 0xf)

#define KMEM "/dev/kmem"
int kmem;

main(ac, av)
int		ac;
reg char	**av;
{
	reg int		idx;

	if (myname = strrchr(av[0], '/'))
		++myname;
	else
		myname = av[0];
	while ((idx = getopt(ac, av, "olfhst:")) != -1)
		switch (idx) {
		case 'o':
			++oflag;
			break;
		case 'f':
			++fflag;
			break;
		case 'h':
			++hflag;
			break;
		case 'l':
			++lflag;
			break;
		case 's':
			++sflag;
			break;
		case 't':
			fstab = optarg;
			break;
		default:
			usage();
		}

	if (optind >= ac)
		usage();
	idx = 0;
	if((kmem = open(KMEM,0)) == -1) {
		perror("open");
		exit(1);
	}
	while (optind < ac)
		idx |= prtvtoc(av[optind++]);
	exit(idx);
	/* NOTREACHED */
}

/*
 * fatal()
 *
 * Print an error message and exit.
 */
static void
fatal(what, why)
	register char *what;
	register char *why;
{
	fprintf (stderr, "%s: %s: %s\n", myname, what, why);
	exit(1);
}

/*
 * findfree()
 *
 * Find free space on a disk. Returns a pointer to the static result.
 */
static Freemap *
findfree(vh)
	register struct volume_header *vh;
{
	register struct partition_table	*part;
	register struct partition_table	**list;
	register struct partition_table	*sorted[NPARTAB + 1];
	register Freemap		*freeidx;
	static Freemap		freemap[NPARTAB + 1];
	long fullsize;
	int endblk;

	fullsize = vh->vh_dp.dp_cyls * 
		(vh->vh_dp.dp_secs * vh->vh_dp.dp_trks0);
	list = sorted;
	for (part = vh->vh_pt; part < &vh->vh_pt[NPARTAB]; ++part) {
		if (part->pt_nblks > 0 && part->pt_type != PTYPE_VOLUME) {
			*list++ = part;
		}
	}
	
	*list = 0;
	qsort((char *) sorted, (uint) (list - sorted), sizeof(*sorted),
		partcmp);
	freeidx = freemap;
	freeidx->fr_start = 0;
	for (list = sorted; part = *list; ++list) {
		endblk = part->pt_firstlbn + part->pt_nblks;
		if (part->pt_firstlbn <= freeidx->fr_start) {
			if (freeidx->fr_start < endblk)
				freeidx->fr_start = endblk;
		}
		else {
			freeidx->fr_size = part->pt_firstlbn - 
				freeidx->fr_start;
			(++freeidx)->fr_start = endblk;
		}
	}
	if (freeidx->fr_start < fullsize) {
		freeidx->fr_size = fullsize - freeidx->fr_start;
		++freeidx;
	}
	freeidx->fr_start = freeidx->fr_size = 0;
	return (freemap);
}

/*
 * getmntpt()
 *
 * Get the filesystem mountpoint of each partition on the disk
 * from the fstab. Returns a pointer to an array of pointers to
 * directory names (indexed by partition number).
 */
static char **
getmntpt(slot, drive)
int		slot;
int		drive;
{
	reg char	*item;
	reg char	*line;
	reg char	*next;
	reg int		idx;
	reg int		fd;
	auto char 	*buf;
	auto char	devbuf[40];
	static char	*delimit = " \t";
	static char	devblk[] = "/dev/";
	static char	devraw[] = "/dev/r";
	static char	*list[NPARTAB];

	for (idx = 0; idx < NPARTAB; ++idx)
		list[idx] = 0;

	if (stat(fstab, &sb) < 0) {
		return (0);
	}
	if ((buf = (char *)malloc(sb.st_size + 1)) == NULL) {
		fatal(fstab, "Out of memory");
	}
	if ((fd = open(fstab, O_RDONLY)) < 0) {
		return (0);
	}
	idx = read(fd, buf, sb.st_size);
	close(fd);
	if (idx < 0) {
		free(buf);
		fatal(syserr(), fstab);
	}
	if (idx == 0) {
		free(buf);
		return(0);
	}
	if (idx != sb.st_size) {
		free(buf);
		fatal(fstab, "Changed size");
	}
	buf[idx] = '\0';
	for (line = buf; next = strchr(line, '\n'); line = next) {
		*next++ = '\0';
		if ((item = strtok(line, delimit))
		  && *item != '#'
		  && strncmp(item, devblk, strsize(devblk)) == 0
		  && stat(strcat(strcpy(devbuf, devraw),
		    item + strsize(devblk)), &sb) == 0
		  && (sb.st_mode & S_IFMT) == S_IFCHR
		  && major(sb.st_rdev) == slot
		  && iddn(minor(sb.st_rdev)) == drive
		  && (item = strtok((char *) 0, delimit))
		  && *item == '/')
			list[idslice(minor(sb.st_rdev))] = memstr(item);
	}
	free(buf);
	return (list);
}

/*
 * getopt()
 *
 * Parse options. Stolen from libc, with changes
 * to avoid standard I/O.
 */
static int
getopt(ac, av, options)
int		ac;
reg char	**av;
char		*options;
{
	reg int		c;
	reg char	*cp;
	static int	sp = 1;

	if (sp == 1)
		if (optind >= ac
		    || av[optind][0] != '-' || av[optind][1] == '\0')
			return (-1);
		else if (strcmp(av[optind], "--") == 0) {
			optind++;
			return (-1);
		}
	c = av[optind][sp];
	if (c == ':' || (cp = strchr(options, c)) == 0)
		usage();
	if (*++cp == ':') {
		if (av[optind][sp+1] != '\0')
			optarg = &av[optind++][sp+1];
		else if (++optind >= ac)
			usage();
		else
			optarg = av[optind++];
		sp = 1;
	} else {
		if (av[optind][++sp] == '\0') {
			sp = 1;
			optind++;
		}
		optarg = 0;
	}
	return (c);
}

/*
 * memstr()
 *
 * Copy a string into dynamic memory. Returns a pointer
 * to the new instance of the given string.
 */
static char *
memstr(str)
reg char	*str;
{
	reg char	*mem;

	if ((mem = (char *) malloc((uint) strlen(str) + 1)) == 0)
		fatal(str, "Out of memory");
	return (strcpy(mem, str));
}

/*
 * partcmp()
 *
 * Qsort() key comparison of partitions by starting sector numbers.
 */
static int
partcmp(one, two)
register struct partition_table	**one;
register struct partition_table	**two;
{
	return ((*one)->pt_firstlbn - (*two)->pt_firstlbn);
}

/*
 * prtvtoc()
 *
 * Read and print a VTOC.
 */
static int
prtvtoc (name)
char		*name;
{
	reg int		fd;
	reg int		idx;
	reg Freemap	*freemap;
	struct stat	sb;
	struct volume_header vh;
	struct io_arg	ia;

	if (stat(name, &sb) < 0)
		return (warn(name, syserr()));
	current_target = (dev_t) TARGET(sb.st_rdev);
#ifdef notdef
	if ((sb.st_mode & S_IFMT) != S_IFCHR)
		return (warn(name, "Not a raw device"));
#endif notdef
	if ((fd = open(name, O_RDONLY)) < 0)
		return (warn(name, syserr()));

	ia.retval = ia.sectst = 0;
	ia.memaddr = (unsigned long)&vh;
	ia.datasz = sizeof(vh);
	if (ioctl(fd, DIOCGETVH, &ia) < 0) {
perror("ioctl");
		(void) close(fd);
		return (warn (name, "Can't read volume header"));
	}
	if (ioctl(fd, DIOCPARTITION, &pu[0]) < 0) {
		for(idx = 0; idx < 16; idx++)
			pu[idx].status = 0;
	} 
	if(lflag) {
		scan_for_swap();
		scan_for_mount();
	}

	(void) close(fd);

	if (vh.vh_magic != VHMAGIC) {
		return (warn (name, "Does not contain a valid volume header"));
	}
	freemap = findfree (&vh);
	if (fflag) {
		putfree (&vh, freemap);
	}
	else {
		puttable(&vh, freemap, name, getmntpt(major(sb.st_rdev), 
			iddn(minor(sb.st_rdev))));
	}
	return (0);
}

/*
 * putfree()
 *
 * Print shell assignments for disk free space. FREE_START and FREE_SIZE
 * represent the starting block and number of blocks of the first chunk
 * of free space. FREE_PART lists the unassigned partitions.
 */
static void
putfree (vh, freemap)
	register struct volume_header *vh;
	register Freemap *freemap;
{
	register struct partition_table *part;
	register Freemap	*freeidx;
	register int		idx;

	printf ("FREE_START=%d FREE_SIZE=%d", freemap->fr_start, 
		freemap->fr_size);
	for (freeidx = freemap; freeidx->fr_size; ++freeidx)
		;
	printf (" FREE_COUNT=%d FREE_PART=", (long)(freeidx - freemap));
	for (idx = 0, part = vh->vh_pt; part < &vh->vh_pt[NPARTAB]; 
	    part++, idx++) {
		if (part->pt_nblks <= 0) {
			printf ("%x ", idx);
		}
	}
	printf("\n");
}

/*
 * puttable()
 *
 * Print a human-readable VTOC.
 */
static void
puttable (vh, freemap, name, mtab)
	register struct volume_header *vh;
	register Freemap *freemap;
	char *name;
	char **mtab;
{
	register struct partition_table *part;
	register struct device_parameters *dp;
	register int	idx;
	register ulong	cylsize;
	register long	unusable;
	int 		i;

	dp = &vh->vh_dp;
	unusable = 0;
	for (part = vh->vh_pt; part < &vh->vh_pt[NPARTAB]; part++) {
		if (part->pt_type == PTYPE_VOLHDR || 
		    part->pt_type == PTYPE_TRKREPL ||
		    part->pt_type == PTYPE_SECREPL) {
		    	unusable += part->pt_nblks;
		}
	}
	cylsize = dp->dp_secs * dp->dp_trks0;
	unusable /= cylsize;
	if (!hflag && !sflag) {
		printf ("* %s", name);
		/*
		 * TODO:
		 *	Normally the volume name is displayed
		 *	here.  Make room in the volume header for
		 *	one.
		 */
		printf (" (bootfile \"%s\")", vh->vh_bootfile);
		printf (" partition map\n*\n* Dimensions:\n");
		printf ("* %7d bytes/sector\n", dp->dp_secbytes);
		printf ("* %7d sectors/track\n", dp->dp_secs);
		printf ("* %7d tracks/cylinder\n", dp->dp_trks0);
		printf ("* %7d cylinders\n", dp->dp_cyls);
		printf ("* %7d accessible cylinders\n", dp->dp_cyls - 
			unusable);
#ifdef notdef
/*
 * Read only file system aren't supported in the MIPS volume
 * header, yet.
 */
 
		printf ("*\n* Flags:\n*  ");
		prn((ulong) V_UNMNT, HEX, 2, 1);
		printf (": unmountable\n*  ");
		prn((ulong) V_RONLY, HEX, 2, 1);
		printf (": read-only\n*\n");
#endif notdef
		if (freemap->fr_size) {
			printf ("* Unallocated space:\n*\tStart\t      Size\n");
			do {
				printf ("*  %10d\t%10d\n", freemap->fr_start,
					freemap->fr_size);
			} while ((++freemap)->fr_size);
			printf ("*\n");
		}
	}
	if(lflag)
		print_partition(vh);
	else {
		if (!hflag)
			printf ("*   Partition  Tag  Flags  First Sector  Sector Count   Mount Directory\n");
	for (idx = 0, part = vh->vh_pt; part < &vh->vh_pt[NPARTAB]; 
	    part++, idx++) {
		if (part->pt_nblks <= 0) {
			continue;
		}
		printf ("\t%2d  %5d  %4x  %10d  \t%10d", idx, 
			part->pt_type, 0, part->pt_firstlbn,
			part->pt_nblks);
		if (mtab && mtab[idx]) {
			printf ("\t %s", mtab[idx]);
		}
		printf ("\n");
		}
	}
}


print_partition(vh)
register struct volume_header *vh;
{
	register struct partition_table *part;
	register i,j;


	if(!hflag) {
		printf ("* Pt. Typ  First Sec.   Sec. Cnt ");
		if(oflag)
			printf("Overlap          ");
		printf("Usage\n");
	}
	for (i = 0, part = vh->vh_pt; part < &vh->vh_pt[NPARTAB]; i++, part++) {
		printf("%3d  %3x  %10d %10d  ",i,
			part->pt_type,
			part->pt_firstlbn,
			part->pt_nblks);
		if(oflag) {
			for(j = 0; j < NPARTAB; j++)
				if(j == i)
					printf("+");
				else if(overlap(vh,i,j))
					printf("%x",j);
				else
					printf("-");
			
			printf(" ");
		}
		if(pu[i].usage.blk)
			printf("Block Dev, ");
		if(pu[i].usage.chr)
			printf("Char  Dev, ");
		if(pu[i].usage.swp)
			printf("Swap  Dev, ");
		if(pu[i].usage.mnt)
			printf("Mounted fs, ");
		if(pu[i].usage.layer)
			printf("Virtual Dev, ");
		if(pu[i].usage.rootdev)
			printf("Root Dev, ");
		if(pu[i].usage.swapdev)
			printf("Primary Swap Dev, ");
		if(mounted[i])
			printf("mounted on %s",mount_point[i]);
		printf("\n");
	}
}

overlap(vh,i,j)
register struct volume_header *vh;
{
	int x1,y1,x2,y2;
	struct partition_table *p1,*p2;

	p1 = &vh->vh_pt[i];
	p2 = &vh->vh_pt[j];
	x1 = p1->pt_firstlbn;
	y1 = x1 + p1->pt_nblks - 1;
	if(y1 < x1) return(0);
	x2 = p2->pt_firstlbn;
	y2 = x2 + p2->pt_nblks - 1;
	if(y2 < x2) return(0);
	if(y1 < x2 || y2 < x1) return(0);
	return(1);
}

/*
 * syserr()
 *
 * Return a pointer to a system error message.
 */
static char *
syserr()
{
	return (errno <= 0 ? "No error (?)"
	    : errno < sys_nerr ? sys_errlist[errno]
	    : "Unknown error (!)");
}

/*
 * usage()
 *
 * Print a helpful message and exit.
 */
static void
usage()
{
	fprintf (stderr,"Usage:	%s [-f] [-h] [-s] [ -t fstab ] [-l [-o]] rawdisk ...\n",
		myname);
	exit(1);
}

/*
 * warn()
 *
 * Print an error message. Always returns -1.
 */
static int
warn(what, why)
reg char	*what;
reg char	*why;
{
	static char	between[] = ": ";
	static char	after[] = "\n";

	fprintf (stderr, "%s: %s: %s\n", myname, what, why);
	return (-1);
}


scan_for_swap()
{
	swpt_t *st,swaptab[MSFILES];
	swpi_t swpi;
	register swpi_t *si;
	struct vnode vbuf;
	int i;

	st = swaptab;
	si = &swpi;
	si->si_cmd = SI_LIST;
	si->si_buf = (char *)st;
	if(sysmips(SMIPSSWPI,si) < 0) {
		perror("swap");
		exit(3);
	}
	for(i = 0; i < MSFILES; i++, st++) {
		if(st->st_ucnt == NULL) continue;
		if(lseek(kmem,(char *)st->st_vp,0) == -1) {
			perror("lseek /dev/kmem");
			return;
		}
		if(read(kmem,(char *)&vbuf, sizeof(vbuf)) != sizeof(vbuf)) {
			perror("read /dev/kmem");
			return;
		}
		if(TARGET(vbuf.v_rdev) == current_target) {
			pu[PART(vbuf.v_rdev)].usage.swp = 1;
		}
	}
}


scan_for_mount()
{
	struct mntent mnt;
	struct mntent *mntp;
	FILE *mnttab;
	struct stat sb1;
	char *p;
	int i;
	
	for(i = 0; i < 16; i++)
		if(mount_point[i])
			free(mount_point[i]);
		mounted[i] = 0;

	if((mnttab = setmntent(MOUNTED,"r")) == NULL)
		return;
	while((mntp = getmntent(mnttab)) != NULL) {
		if(strcmp(mntp->mnt_type,MNTTYPE_IGNORE) == 0)
			continue;
		if (stat(mntp->mnt_fsname, &sb1) < 0) {
			continue;
		}
		if(TARGET(sb1.st_rdev) == current_target) {
			pu[PART(sb1.st_rdev)].usage.blk = 1;
			if(p = mount_point[PART(sb1.st_rdev)] = (char *)malloc(strlen(mntp->mnt_dir) + 1))
				strcpy(p,mntp->mnt_dir);
			mounted[PART(sb1.st_rdev)] = 1;
		}
	}
	endmntent(mnttab);
}
