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
#ident	"$Header: main.c,v 1.8.1.7.1.1.1.2 90/12/20 19:16:00 beacker Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)main.c	1.6 88/05/23 4.0NFSSRC SMI	from UCB 5.4 3/5/86
 *	@(#) from SUN 1.32
 */

#include <stdio.h>
#include <sys/param.h>
#include <ufs/fs.h>
#include <sys/time.h>
#include <sys/vnode.h>
#include <ufs/inode.h>
#include <sys/stat.h>
#include <sys/wait.h>
#define KERNEL
#include <ufs/fsdir.h>
#undef KERNEL
#include <mntent.h>
#include <strings.h>

#include "fsck.h"
#ifdef RISCOS
#include <sysv/sys/uadmin.h>
#include <sysv/sys.s>

int check_only_dirty_fs = 0;
int all = 0;
#endif

int	exitstat;	/* exit status (set to 8 if 'No' response) */
char	*rawname(), *unrawname(), *blockcheck();
int	catch(), catchquit(), voidquit();
int	returntosingle;
int	(*signal())();
int	rootfs = 0;		/* am I checking the root? */

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int pid, passno, anygtr, sumstatus;
	char *name;

	sync();
	while (--argc > 0 && **++argv == '-') {
		switch (*++*argv) {

#if defined(RISCOS)
		case 'a':	/* fsck all fs as fast as possible excluding */
			all++;	/* root.  Done this way, so -p could operate */
				/* in its default manner.  Root will almost  */
				/* always be dirty, so don't bother fsck'ing */
#endif
		case 'p':
			preen++;
			break;

		case 'b':
			if (argv[0][1] != '\0') {
				bflag = atoi(argv[0]+1);
			} else {
				bflag = atoi(*++argv);
				argc--;
			}
			printf("Alternate super block location: %d\n", bflag);
			break;

		case 'd':
			debug++;
			break;

#if defined(RISCOS)
	        case 'C':
			check_only_dirty_fs++;
			break;

		case 'r':
			rebflg++;
			break;

#endif
		case 'n':	/* default no answer flag */
		case 'N':
			nflag++;
			yflag = 0;
			break;

		case 'y':	/* default yes answer flag */
		case 'Y':
			yflag++;
			nflag = 0;
			break;

		default:
			errexit("%c option?\n", **argv);
		}
	}
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void)signal(SIGINT, catch);
	if (preen)
		(void)signal(SIGQUIT, catchquit);
	if (argc) {
		while (argc-- > 0) {
#if defined(RISCOS)
			rootfs = is_root(*argv);
#else
			if (strcmp (*argv, "/") == 0)
				rootfs = 1;
#endif
			checkfilesys(*argv);
			rootfs = 0;
			argv++;
			}
		exit(exitstat);
	}
	sumstatus = 0;
	passno = 1;
	do {
		FILE *fstab;
		struct mntent *mnt;
		char	*raw;

		anygtr = 0;
		if ((fstab = setmntent(MNTTAB, "r")) == NULL)
			errexit("Can't open checklist file: %s\n", MNTTAB);
		while ((mnt = getmntent(fstab)) != 0) {
#ifdef RISCOS
			if (! IS_RISCOS_FS_TYPE(mnt->mnt_type)) {
#else /* !RISCOS */
			if (strcmp(mnt->mnt_type, MNTTYPE_43)) {
#endif /* RISCOS */
				continue;
			}
			if (!hasmntopt(mnt,MNTOPT_RW) &&
			    !hasmntopt(mnt,MNTOPT_RO) &&
			    !hasmntopt(mnt,MNTOPT_QUOTA)){
				continue;
			}
			mnt = mntdup(mnt);
			if (preen == 0 ||
			  passno == 1 && mnt->mnt_passno == passno) {
				name = blockcheck(mnt->mnt_fsname);
				if (name != NULL) {
					checkfilesys(name);
				} else if (preen)
					exit(8);
			} else if (mnt->mnt_passno > passno) 
				anygtr = 1;
			else if (mnt->mnt_passno == passno) {
				pid = fork();
				if (pid < 0) {
					perror("fork");
					exit(8);
				}
				if (pid == 0) {
					(void)signal(SIGQUIT, voidquit);
					name = blockcheck(mnt->mnt_fsname);
					if (name == NULL)
						exit(8);
					checkfilesys(name);
					exit(exitstat);
				}
			}
		}
		endmntent(fstab);
		if (preen) {
			union wait status;
			while (wait(&status) != -1)
				sumstatus |= status.w_retcode;
		}
		passno++;
	} while (anygtr);
	if (sumstatus)
		exit(8);
	(void)endfsent();
	if (returntosingle)
		exit(2);
	exit(exitstat);
}

#if defined(RISCOS)
int
is_root(name)
	char *name;
{
	struct stat stslash, stblock;

	if (stat("/", &stslash) < 0){
		printf("Can't stat root\n");
		return (0);
	}

	if (stat(name, &stblock) < 0){
		printf("Can't stat %s\n", name);
		return (0);
	}
	if ((stblock.st_mode & S_IFMT) == S_IFBLK ||
	    (stblock.st_mode & S_IFMT) == S_IFCHR) {
		if (stslash.st_dev == stblock.st_rdev) {
			return(1);
		}
	}
	return (0);
}
#endif /* RISCOS */


checkfilesys(filesys)
	char *filesys;
{
	daddr_t n_ffree, n_bfree;
	struct dups *dp;
	struct zlncnt *zlnp;
#ifdef RISCOS
#define NRETRIES	3
	int retries;

	for (retries = NRETRIES; retries > 0; retries--) {
#endif /* RISCOS */
	mountedfs = 0;
	if ((devname = setup(filesys)) == 0) {
		if (preen)
			pfatal("CAN'T CHECK FILE SYSTEM.");
		return;
	}

#ifdef RISCOS
	if (all && rootfs)
		return;
	if (check_only_dirty_fs) {	/* which are unmounted */
	    union wait status;
	    char cmd[MNTMAXSTR];

	    sprintf(cmd,"2>&1 /etc/fsstat %s > /dev/null", unrawname(filesys));
	    status.w_status = system(cmd);
	    switch (status.w_T.w_Retcode) {
	      case 1: /* dirty root/non-root, so fsck */
	        break;
	      case 0: /* clean & unmounted non-root or clean root */
	      case 2: /* mounted non-root file system */
	        return;
	      case 3: /* unexpected failure */
	        fprintf(stderr, "fsck.ffs: fsstat gave unexpected failure\n");
	        break;
	      default: /* unknown code */
	        fprintf(stderr, "fsck.ffs: fsstat gave unknown return code\n");
	        break;
	    }
	    if (status.w_T.w_Retcode != 1)
		return;			/* filesystem clean or invalid */
	}

	/*
	 * Mark fs dirty while working.
	 */
	{
		daddr_t tmp_bno;
		int tmp_mod;

		tmp_bno = sblk.b_bno;
		tmp_mod = dfile.mod;
		sblock.fs_clean = 0;
		sbdirty();
		flush(&dfile, &sblk);
		sblk.b_bno = tmp_bno;
		dfile.mod = tmp_mod;
	}
#endif /* RISCOS */

	/*
	 * 1: scan inodes tallying blocks used
	 */
	if (preen == 0) {
		if (mountedfs)
			printf("** Currently Mounted on %s\n", sblock.fs_fsmnt);
		else

			printf("** Last Mounted on %s\n", sblock.fs_fsmnt);
#ifdef RISCOS
		if (rootfs)
			printf("** Root file system\n");
#endif
		printf("** Phase 1 - Check Blocks and Sizes\n");
	}
	pass1();

	/*
	 * 1b: locate first references to duplicates, if any
	 */
	if (duplist) {
		if (preen)
			pfatal("INTERNAL ERROR: dups with -p");
		printf("** Phase 1b - Rescan For More DUPS\n");
		pass1b();
	}

	/*
	 * 2: traverse directories from root to mark all connected directories
	 */
	if (preen == 0)
		printf("** Phase 2 - Check Pathnames\n");
	pass2();

	/*
	 * 3: scan inodes looking for disconnected directories
	 */
	if (preen == 0)
		printf("** Phase 3 - Check Connectivity\n");
	pass3();

	/*
	 * 4: scan inodes looking for disconnected files; check reference counts
	 */
	if (preen == 0)
		printf("** Phase 4 - Check Reference Counts\n");
	pass4();

	/*
	 * 5: check and repair resource counts in cylinder groups
	 */
	if (preen == 0)
		printf("** Phase 5 - Check Cyl groups\n");
	pass5();

	/*
	 * print out summary statistics
	 */
	n_ffree = sblock.fs_cstotal.cs_nffree;
	n_bfree = sblock.fs_cstotal.cs_nbfree;
	pwarn("%d files, %d used, %d free ",
	    n_files, n_blks, n_ffree + sblock.fs_frag * n_bfree);
	printf("(%d frags, %d blocks, %.1f%% fragmentation)\n",
	    n_ffree, n_bfree, (float)(n_ffree * 100) / sblock.fs_dsize);
	if (debug && (n_files -= imax - ROOTINO - sblock.fs_cstotal.cs_nifree))
		printf("%d files missing\n", n_files);
	if (debug) {
		n_blks += sblock.fs_ncg *
			(cgdmin(&sblock, 0) - cgsblock(&sblock, 0));
		n_blks += cgsblock(&sblock, 0) - cgbase(&sblock, 0);
		n_blks += howmany(sblock.fs_cssize, sblock.fs_fsize);
		if (n_blks -= fmax - (n_ffree + sblock.fs_frag * n_bfree))
			printf("%d blocks missing\n", n_blks);
		if (duplist != NULL) {
			printf("The following duplicate blocks remain:");
			for (dp = duplist; dp; dp = dp->next)
				printf(" %d,", dp->dup);
			printf("\n");
		}
		if (zlnhead != NULL) {
			printf("The following zero link count inodes remain:");
			for (zlnp = zlnhead; zlnp; zlnp = zlnp->next)
				printf(" %d,", zlnp->zlncnt);
			printf("\n");
		}
	}
	zlnhead = (struct zlncnt *)0;
	duplist = (struct dups *)0;
#ifdef RISCOS
	if ((retries <= 0) && dfile.mod) {
#else
	if (dfile.mod) {
#endif /* RISCOS */
		(void)time(&sblock.fs_time);
		sbdirty();
	}

#ifdef RISCOS
	/*
	 * Now that we're done, mark fs clean.
	 */
	{
		daddr_t tmp_bno;
		int tmp_mod;

		tmp_mod = dfile.mod;
		tmp_bno = sblk.b_bno;
		sblock.fs_clean = 0;
		sblock.fs_clean |= UFS_CLEAN;
		sbdirty();
		flush(&dfile, &sblk);
		sblk.b_bno = tmp_bno;
		dfile.mod = tmp_mod;
	}
#endif /* RISCOS */

	ckfini();
	bflag = 0;	/* The default super block should be okay now. */
	free(blockmap);
	free(statemap);
	free((char *)lncntp);
#ifdef RISCOS
	if (!dfile.mod) {
		if (retries == NRETRIES) {
			return;
		} else {
			retries = 0;
		}
	} else {
		if (retries > 1) {
			if (preen) {
		 		printf("%s: FILE SYSTEM MODIFIED, VERIFYING\n",filesys);
			} else {
		 		printf("**** FILE SYSTEM MODIFIED, VERIFYING\n");
			}
		}
	}	
	}

	if (dfile.mod) {
		pfatal ("**** UNABLE TO GET A CLEAN FSCK AFTER %d RETRIES\n", NRETRIES);
		return;
	}
#else
	if (!dfile.mod)
		return;
#endif /* RISCOS */
#ifdef RISCOS
	if (rootfs) {
		int uadmin();
		int error;

		sync();
		printf("\n");
		printf("***** ROOT FILE SYSTEM WAS MODIFIED *****\n");
		if (!rebflg && ((error=uadmin(A_REMOUNT, 0, 0) == 0))) {
			printf("  *** ROOT REMOUNTED ***\n");
			sleep(2);
		} else {
			printf("***** REBOOTING UNIX AUTOMATICALLY *****\n\n");
			sleep(2);
			uadmin(A_REBOOT, AD_BOOT);
		}
	} else {
		if (!preen) {
			printf("\n***** FILE SYSTEM WAS MODIFIED *****\n");
		}
	}
#else /* !RISCOS */
	if (!preen) {
		printf("\n***** FILE SYSTEM WAS MODIFIED *****\n");
		if (mountedfs || rootfs)
			printf("\n***** REBOOT THE SYSTEM *****\n");
	}
	if (mountedfs || rootfs) {
		/* doesn't make sense to sync here w/ non-block devices */
		exit(4);
	}
#endif /* RISCOS */
}

/*
 * NOTE: blockcheck() will return the unrawed name if passed the root
 *	device.  Otherwise, it returns the rawname.  Why?
 * NOTE II: Changed to always return the block device because it's
 *	faster to fsck the block device.
 */
char *
blockcheck(name)
	char *name;
{
	struct stat stslash, stblock, stchar;
	char *raw;
	int looped = 0;

	rootfs = 0;
	if (stat("/", &stslash) < 0){
		printf("fsck.ffs: Can't stat root\n");
		return (0);
	}
retry:
	if (stat(name, &stblock) < 0){
		printf("fsck.ffs: Can't stat %s\n", name);
		return (0);
	}
	if (stblock.st_mode & S_IFBLK) {
		raw = rawname(name);
		if (stat(raw, &stchar) < 0){

#ifdef RISCOS
			printf("fsck.ffs: WARNING: Can't stat %s.", raw);
			printf("  Will check: %s\n", name);
			return(name);
#else
			printf("Can't stat %s.\n", raw);
			return (0);
#endif
		}
		if (stchar.st_mode & S_IFCHR) {
			if (stslash.st_dev == stblock.st_rdev) {
				rootfs++;
#ifndef RISCOS
				raw = unrawname(name);
#endif
			}
#ifdef RISCOS
			raw = unrawname(name);
#endif
			return (raw);
		} else {
			printf("fsck.ffs: %s is not a character device\n",raw);
			return (0);
		}
	} else if (stblock.st_mode & S_IFCHR) {
		if (looped) {
			printf("fsck.ffs: Can't make sense out of name %s\n",
			       name);
			return (0);
		}
		name = unrawname(name);
		looped++;
		goto retry;
	}
	printf("Can't make sense out of name %s\n", name);
	return (0);
}


char *
unrawname(cp)
	char *cp;
{
	char *dp = rindex(cp, '/');
	struct stat stb;

	if (dp == 0)
		return (cp);
	if (stat(cp, &stb) < 0)
		return (cp);
	if ((stb.st_mode&S_IFMT) != S_IFCHR)
		return (cp);
#ifdef RISCOS
	if ((dp = index(cp, '/')) == cp) {	/* dp can't equal 0 */
		char *dp2;

		dp++;
		if ((dp2 = index(dp, '/')) != 0)
			dp = dp2;
	}
	if (*(dp+1) != 'r')
		return (cp);
	(void) strcpy(dp+1, dp+2);
#else
	if (*(dp+1) != 'r')
		return (cp);
	(void)strcpy(dp+1, dp+2);
#endif /* RISCOS */
	return (cp);
}

char *
rawname(cp)
	char *cp;
{
	static char rawbuf[MAXPATHLEN];
#ifdef RISCOS
	char *dp2, *dp = index(cp, '/');

	if (dp == 0)
		return (0);
	if (dp == cp) {					/* initial / */
		if ((dp2 = index(dp+1, '/')) != 0)
			dp = dp2;
	}
#else
	char *dp = rindex(cp, '/');

	if (dp == 0)
		return (0);
#endif
	*dp = 0;
	(void)strcpy(rawbuf, cp);
	*dp = '/';
	(void)strcat(rawbuf, "/r");
	(void)strcat(rawbuf, dp+1);
	return (rawbuf);
}

#ifdef RISCOS
/*
 * Implement the SysV call for BSD-land.  The last argument may be
 * trash, but that's how the original system call was made.
 */
int uadmin(cmd, fcn, mdep)
int cmd, fcn, mdep;
{
	int error;

	error = syscall(SYS_uadmin, cmd, fcn, mdep);
	return(error);
}
#endif
