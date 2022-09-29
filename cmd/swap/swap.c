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
#ident	"$Header: swap.c,v 1.9.1.4.1.2 90/08/20 17:52:29 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	Swap administrative interface
 *	Used to add/delete/list swap devices in use by paging.
 */

#include	<stdio.h>
#include	<fcntl.h>
#include	<sys/types.h>
#include	<sys/param.h>
#include	<dirent.h>
#include	<sys/map.h>
#include	<sys/swap.h>
#include	<sys/immu.h>
#include	<sys/stat.h>
#include	<sys/sysmips.h>
#ifdef RISCOS
#include	<bsd/sys/time.h>
#include	<sys/vnode.h>
#endif RISCOS

/*	This is gross, but it will have
 *	to do for now.  How about a sysadm(2)?
 */

#ifdef mips
#define swapfunc(X)	sysmips(SMIPSSWPI, X)
#endif

#ifdef u3b
#define swapfunc(X)	sys3b(3, X)
#endif

#ifdef u3b2
#define swapfunc(X)	sys3b(S3BSWPI, X)
#endif

#ifdef vax
swapfunc()
{
	asm("	chmk	$58");
	asm("	bcc	.noerror");
	asm("	jmp	cerror");
	asm(".noerror:");
}
#endif

typedef struct SwapDirs {
	char	*sd_name;	/* Name of directory to search.	*/
	DIR	*sd_dirp;	/* pointer to DIR struct */
} SwapDirs_t;

SwapDirs_t	SwapDirs[] = {
	{"/dev/dsk"},
	{"/dev"},
};

#define	NbrSwapDirs	(sizeof(SwapDirs) / sizeof(SwapDirs_t))

#ifdef RISCOS
#define KMEM_NAME "/dev/kmem"

int	kmem;

int	Dpp_mult;		/* number of disk blocks per page */
#endif RISCOS

main(argc, argv)
int	argc;
char	*argv[];
{
	if(argc < 2  ||  argv[1][0] != '-'){
		usage();
		exit(2);
	}

#ifdef RISCOS
	kmem = open(KMEM_NAME,0);
	if (kmem == -1) {
		perror(KMEM_NAME);
		exit(1);
	};
#endif RISCOS
	
	switch(argv[1][1]){
		case 'l':
			list();
			break;
		
		case 'd':
			if(argc < 4){
				usage();
				exit(2);
			}
			delete(argv[2], argv[3]);
			break;
		
		case 'a':
			if(argc < 5){
				usage();
				exit(2);
			}
			add(argv[2], argv[3], argv[4]);
			break;
		default:
			usage();
			exit(2);
			break;
	}
}


usage()
{
	fprintf(stderr, "usage:\tswap -l\n");
	fprintf(stderr, "\tswap -d <special dev> <low block>\n");
	fprintf(stderr, "\tswap -a <special dev> <low block> <nbr of blocks>\n");
}

list()
{
	register int	swplo;
	register int	blkcnt;
	register swpt_t	*st;
	register swpi_t	*si;
	register int	i;
	dev_t		unit_dev;
	swpt_t		swaptab[MSFILES];
	swpi_t		swpi;
	char		*path;
	int		length;
	char		*GetPath();

	st = swaptab;
	si = &swpi;
	si->si_cmd = SI_LIST;
	si->si_buf = (char *)st;

	if(swapfunc(si) < 0){
		perror("swap");
		exit(3);
	}

	InitPath();

	printf("path                dev  swaplo blocks   free\n");

	for(i = 0  ;  i < MSFILES  ;  i++, st++){
		if(st->st_ucnt == NULL)
			continue;
#ifdef RISCOS
		Dpp_mult = getpagesize()/NBPSCTR;
		path = GetPath(st,&unit_dev);
#else RISCOS
		unit_dev = st->st_dev;
		path = GetPath(unit_dev);
#endif RISCOS
		if(path == NULL){
			printf("%.19s", "??????                  ");
		} else {
			length = 19 -strlen(path);
			if(length < 0)
				length = 0;
			printf("%s%.*s", path, length, "                ");

		}
#ifdef RISCOS
		if (unit_dev == 0) {
			printf("  -   %6d %6d %6d", 
				st->st_swplo,
				st->st_npgs * Dpp_mult,
				st->st_nfpgs * Dpp_mult);
		} else
#endif /* RISCOS */
		printf("%2d,%-2d %6d %6d %6d", 
			unit_dev >> 8,
			unit_dev &  0xFF,
			st->st_swplo,
#ifdef RISCOS
			st->st_npgs * Dpp_mult,
			st->st_nfpgs * Dpp_mult);
#else
			st->st_npgs << DPPSHFT,
			st->st_nfpgs << DPPSHFT);
#endif RISCOS
		if(st->st_flags & ST_INDEL)
			printf(" INDEL\n");
		else
			printf("\n");
	}

}

delete(path, lo)
char	*path;
char	*lo;
{
	register int	swplo;
	register swpi_t	*si;
	swpi_t		swpi;

	swplo = atoi(lo);
	si = &swpi;
	si->si_cmd = SI_DEL;
	si->si_buf = path;
	si->si_swplo = swplo;

	if(swapfunc(si) < 0){
		perror("swap");
		exit(3);
	}
}

add(path, lo, cnt)
char	*path;
char	*lo;
char	*cnt;
{
	register int	swplo;
	register int	blkcnt;
	register swpi_t	*si;
	swpi_t		swpi;
#ifdef RISCOS
	char	mypath[MAXPATHLEN];
	int	pathlen;
#endif /* RISCOS */

	swplo = atoi(lo);
	blkcnt = atoi(cnt);
	si = &swpi;
	si->si_cmd = SI_ADD;
#ifdef RISCOS
	if (path[0] != '/') {
		if (getcwd (mypath, sizeof (mypath))) {
			pathlen = strlen (mypath);
			mypath[pathlen++] = '/';
			strcpy (&mypath[pathlen], path);
			si->si_buf = mypath;
		} else {
			si->si_buf = path;
		}
	} else
#endif /* RISCOS */
	si->si_buf = path;

	si->si_swplo = swplo;
	si->si_nblks = blkcnt;

	if(swapfunc(si) < 0){
		perror("swap");
		exit(3);
	}

}


/*	Initialization for the path search routine.
*/

InitPath()
{
	register int	i;

	for(i = 0  ;  i < NbrSwapDirs  ;  i++){
		SwapDirs[i].sd_dirp = opendir(SwapDirs[i].sd_name);
		if(SwapDirs[i].sd_dirp == 0) {
			fprintf(stderr, "Couldn't open directory %s",
				SwapDirs[i].sd_name);
			fprintf(stderr, " - not being searched.\n");
			perror("swap");
		}
	}
}


/*	Get path name corresponding to a device.
*/

#define	MAXPATH	200

char	PathBuf[MAXPATH];

char	*
#ifdef RISCOS
GetPath(st,devp)
	register swpt_t	*st;
	dev_t	*devp;
#else RISCOS
GetPath(dev)
register int	dev;
#endif RISCOS
{
#ifdef RISCOS
	struct vnode vbuf;
	dev_t	dev;
#endif RISCOS
	register int	sdndx;
	register char	*path;
	char		*SearchDir();

#ifdef RISCOS
	if (lseek(kmem,(char *) st->st_vp,0) == -1) {
		perror(KMEM_NAME);
		return(NULL);
	};
	if (read(kmem,(char *) &vbuf, sizeof(vbuf)) != sizeof(vbuf)) {
		perror(KMEM_NAME);
		return(NULL);
	};
	dev = vbuf.v_rdev;
	*devp = dev;

	if (st->st_len > 0) {
		if (lseek(kmem,(char *) st->st_name,0) == -1) {
			perror(KMEM_NAME);
			return(NULL);
		};
		if (read(kmem, PathBuf, st->st_len) != st->st_len) {
			perror(KMEM_NAME);
			return(NULL);
		};
		return (PathBuf);
	};
#endif /* RISCOS */

	for(sdndx = 0  ;  sdndx < NbrSwapDirs  ;  sdndx++){

		/*	Skip any directories which we
		**	couldn't open.
		*/

		if(SwapDirs[sdndx].sd_dirp == 0)
			continue;
		
		/*	Change to directory we want to search.
		*/

		if(chdir(SwapDirs[sdndx].sd_name) < 0){
			fprintf(stderr, "Couldn't chdir to %s",
				SwapDirs[sdndx].sd_name);
			fprintf(stderr, " - directory not searched.\n");
			perror("swap");
			SwapDirs[sdndx].sd_dirp = 0;
			continue;
		}

		if(path = SearchDir(&SwapDirs[sdndx], dev))
			return(path);
	}
	return(NULL);
}


char	*
SearchDir(sdp, dev)
register SwapDirs_t	*sdp;
register int		dev;
{
	register int	i;
	register struct	dirent *dp;
	struct stat	statbuf;

	(void) rewinddir(sdp->sd_dirp);
	while (dp = readdir(sdp->sd_dirp)) {
		sprintf(PathBuf, "%s", dp->d_name);
		if(stat(PathBuf, &statbuf) <0){
			fprintf(stderr,
				"Could not stat %s\n",
				PathBuf);
			perror("swap");
			continue;
		}
		if(statbuf.st_rdev != dev)
			continue;
		if((statbuf.st_mode & S_IFMT) != S_IFBLK)
			continue;
		strcpy(PathBuf, sdp->sd_name);
		strcat(PathBuf, "/");
		sprintf(&PathBuf[strlen(PathBuf)], "%s", dp->d_name);
		return(PathBuf);
	}
	return(NULL);
}
