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
#ident	"$Header: quotacheck.c,v 1.2.1.3 90/05/09 18:22:43 wje Exp $"
/*----- COPYRIGHT (END) -----------------------------------------------------*\

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)quotacheck.c	1.2 88/03/15 4.0NFSSRC
 *	from 5.6 (Berkeley) 11/3/85
 *	and from 1.12 88/02/08 SMI
 */

/*
 * Fix up / report on disc quotas & usage
 */
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/vnode.h>
#include <ufs/inode.h>
#include <ufs/fs.h>
#include <ufs/quota.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <mntent.h>
#include <pwd.h>

#if defined(RISCOS)
extern int errno;
#endif

#define MAXMOUNT	50

union {
	struct	fs	sblk;
	char	dummy[MAXBSIZE];
} un;
#define	sblock	un.sblk

#define	ITABSZ	256
struct	dinode	itab[ITABSZ];
struct	dinode	*dp;

#define LOGINNAMESIZE	8
struct fileusage {
	struct fileusage *fu_next;
	u_long fu_curfiles;
	u_long fu_curblocks;
	u_short	fu_uid;
	char fu_name[LOGINNAMESIZE + 1];
};
#define FUHASH 997
struct fileusage *fuhead[FUHASH];
struct fileusage *lookup();
struct fileusage *adduid();
int highuid;

int fi;
ino_t ino;
struct	dinode	*ginode();
char *malloc(), *makerawname();
#ifdef RISCOS
char *unrawname();
#endif

int	vflag;		/* verbose */
int	aflag;		/* all file systems */
int	pflag;		/* fsck like parallel check */

#define QFNAME "quotas"
char *listbuf[MAXMOUNT];
struct dqblk zerodqbuf;
struct fileusage zerofileusage;

main(argc, argv)
	int argc;
	char **argv;
{
	register struct mntent *mntp;
	register struct fileusage *fup;
	char **listp;
	int listcnt;
	char quotafile[MAXPATHLEN + 1];
	FILE *mtab, *fstab;
	int errs = 0;
	struct passwd *pw;

again:
	argc--, argv++;
	if (argc > 0 && strcmp(*argv, "-v") == 0) {
		vflag++;
		goto again;
	}
	if (argc > 0 && strcmp(*argv, "-a") == 0) {
		aflag++;
		goto again;
	}
	if (argc > 0 && strcmp(*argv, "-p") == 0) {
		pflag++;
		goto again;
	}
	if (argc <= 0 && !aflag) {
		fprintf(stderr, "Usage:\n\t%s\n\t%s\n",
			"quotacheck [-v] [-p] -a",
			"quotacheck [-v] [-p] filesys ...");
		exit(1);
	}

	(void) setpwent();
	while ((pw = getpwent()) != 0) {
		fup = lookup((u_short)pw->pw_uid);
		if (fup == 0) {
			fup = adduid((u_short)pw->pw_uid);
			strncpy(fup->fu_name, pw->pw_name,
				sizeof(fup->fu_name));
		}
	}
	(void) endpwent();

	if (quotactl(Q_SYNC, NULL, 0, NULL) < 0 && errno == EINVAL && vflag)
		printf( "Warning: Quotas are not compiled into this kernel\n");
	sync();

	if (aflag) {
		/*
		 * Go through fstab and make a list of appropriate
		 * filesystems.
		 */
		listp = listbuf;
		listcnt = 0;
		if ((fstab = setmntent(MNTTAB, "r")) == NULL) {
			fprintf(stderr, "Can't open ");
			perror(MNTTAB);
			exit(8);
		}
		while (mntp = getmntent(fstab)) {
#if defined(RISCOS)
			if (! IS_RISCOS_FS_TYPE(mntp->mnt_type)	||
			    ! HAS_RISCOS_QUOTAS(mntp)		||
			      hasmntopt(mntp, MNTOPT_RO))
#else
			if (strcmp(mntp->mnt_type, MNTTYPE_43) != 0 ||
			    !hasmntopt(mntp, MNTOPT_QUOTA) ||
			    hasmntopt(mntp, MNTOPT_RO))
#endif /* RISCOS */
				continue;
			*listp = malloc(strlen(mntp->mnt_fsname) + 1);
			strcpy(*listp, mntp->mnt_fsname);
			listp++;
			listcnt++;
		}
		endmntent(fstab);
		*listp = (char *)0;
		listp = listbuf;
	} else {
		listp = argv;
		listcnt = argc;
	}
	if (pflag) {
		errs = preen(listcnt, listp);
	} else {
		if ((mtab = setmntent(MOUNTED, "r")) == NULL) {
			fprintf(stderr, "Can't open ");
			perror(MOUNTED);
			exit(8);
		}
		while (mntp = getmntent(mtab)) {
#if defined(RISCOS)
			if (IS_RISCOS_FS_TYPE(mntp->mnt_type)	&&
#else
			if (strcmp(mntp->mnt_type, MNTTYPE_43) == 0 &&
#endif /* RISCOS */
			    !hasmntopt(mntp, MNTOPT_RO) &&
			    (oneof(mntp->mnt_fsname, listp, listcnt) ||
			     oneof(mntp->mnt_dir, listp, listcnt)) ) {
				sprintf(quotafile,
				    "%s/%s", mntp->mnt_dir, QFNAME);
				errs +=
				    chkquota(mntp->mnt_fsname,
					mntp->mnt_dir, quotafile);
			}
		}
		endmntent(mtab);
	}
	while (listcnt--) {
		if (*listp)
			fprintf(stderr, "Cannot check %s\n", *listp);
	}
	exit(errs);
}

preen(listcnt, listp)
	int listcnt;
	char **listp;
{
	register struct mntent *mntp;
	register int passno, anygtr;
	register int errs;
	FILE *mtab;
	union wait status;
	char quotafile[MAXPATHLEN + 1];

	passno = 1;
	errs = 0;
	if ((mtab = setmntent(MOUNTED, "r")) == NULL) {
		fprintf(stderr, "Can't open ");
		perror(MOUNTED);
		exit(8);
	}
	do {
		rewind(mtab);
		anygtr = 0;

		while (mntp = getmntent(mtab)) {
			if (mntp->mnt_passno > passno)
				anygtr = 1;

			if (mntp->mnt_passno != passno)
				continue;

#if defined(RISCOS)
			if (! IS_RISCOS_FS_TYPE(mntp->mnt_type)	||
#else
			if (strcmp(mntp->mnt_type, MNTTYPE_43) != 0 ||
#endif /* RISCOS */
			    hasmntopt(mntp, MNTOPT_RO) ||
			    (!oneof(mntp->mnt_fsname, listp, listcnt) &&
			     !oneof(mntp->mnt_dir, listp, listcnt)) )
				continue;

			switch (fork()) {
			case -1:
				perror("fork");
				exit(8);
				break;

			case 0:
				sprintf(quotafile, "%s/%s",
					mntp->mnt_dir, QFNAME);
				exit(chkquota(mntp->mnt_fsname,
					mntp->mnt_dir, quotafile));
			}
		}

		while (wait(&status) != -1) 
			errs += status.w_retcode;

		passno++;
	} while (anygtr);
	endmntent(mtab);

	return (errs);
}

chkquota(fsdev, fsfile, qffile)
	char *fsdev;
	char *fsfile;
	char *qffile;
{
	register struct fileusage *fup;
	dev_t quotadev;
	FILE *qf;
	register u_short uid;
	register u_short highquota;
	int cg, i;
	char *rawdisk;
	struct stat statb;
	struct dqblk dqbuf;
	extern int errno;

	rawdisk = makerawname(fsdev);
#ifdef RISCOS
	rawdisk = unrawname(rawdisk);
#endif
	if (vflag)
		printf("*** Checking quotas for %s (%s)\n", rawdisk, fsfile);
	fi = open(rawdisk, 0);
	if (fi < 0) {
		perror(rawdisk);
		return (1);
	}
	qf = fopen(qffile, "r+");
	if (qf == NULL) {
		perror(qffile);
		close(fi);
		return (1);
	}
	if (fstat(fileno(qf), &statb) < 0) {
		perror(qffile);
		fclose(qf);
		close(fi);
		return (1);
	}
	quotadev = statb.st_dev;
	if (stat(fsdev, &statb) < 0) {
		perror(fsdev);
		fclose(qf);
		close(fi);
		return (1);
	}
	if (quotadev != statb.st_rdev) {
		fprintf(stderr, "%s dev (0x%x) mismatch %s dev (0x%x)\n",
			qffile, quotadev, fsdev, statb.st_rdev);
		fclose(qf);
		close(fi);
		return (1);
	}
	bread(SBLOCK, (char *)&sblock, SBSIZE);
	ino = 0;
	for (cg = 0; cg < sblock.fs_ncg; cg++) {
		dp = NULL;
		for (i = 0; i < sblock.fs_ipg; i++)
			acct(ginode());
	}
	highquota = 0;
	for (uid = 0; uid <= highuid; uid++) {
		(void) fseek(qf, (long)dqoff(uid), 0);
		(void) fread(&dqbuf, sizeof(struct dqblk), 1, qf);
		if (feof(qf))
			break;
		fup = lookup(uid);
		if (fup == 0)
			fup = &zerofileusage;
		if (dqbuf.dqb_bhardlimit || dqbuf.dqb_bsoftlimit ||
		    dqbuf.dqb_fhardlimit || dqbuf.dqb_fsoftlimit) {
			highquota = uid;
		} else {
			fup->fu_curfiles = 0;
			fup->fu_curblocks = 0;
		}
		if (dqbuf.dqb_curfiles == fup->fu_curfiles &&
		    dqbuf.dqb_curblocks == fup->fu_curblocks) {
			fup->fu_curfiles = 0;
			fup->fu_curblocks = 0;
			continue;
		}
		if (vflag) {
			if (pflag || aflag)
				printf("%s: ", rawdisk);
			if (fup->fu_name[0] != '\0')
				printf("%-10s fixed:", fup->fu_name);
			else
				printf("#%-9d fixed:", uid);
			if (dqbuf.dqb_curfiles != fup->fu_curfiles)
				printf("  files %d -> %d",
				    dqbuf.dqb_curfiles, fup->fu_curfiles);
			if (dqbuf.dqb_curblocks != fup->fu_curblocks)
				printf("  blocks %d -> %d",
#if defined(RISCOS)
#define kb(n)	(howmany(dbtob(n), 1024))
				       kb(dqbuf.dqb_curblocks),
				       kb(fup->fu_curblocks));
#else
				    dqbuf.dqb_curblocks, fup->fu_curblocks);
#endif /* RISCOS */
			printf("\n");
		}
		dqbuf.dqb_curfiles = fup->fu_curfiles;
		dqbuf.dqb_curblocks = fup->fu_curblocks;
		(void) fseek(qf, (long)dqoff(uid), 0);
		(void) fwrite(&dqbuf, sizeof(struct dqblk), 1, qf);
		(void) quotactl(Q_SETQUOTA, fsdev, uid, &dqbuf);
		fup->fu_curfiles = 0;
		fup->fu_curblocks = 0;
	}
	(void) fflush(qf);
	(void) ftruncate(fileno(qf), (highquota + 1) * sizeof(struct dqblk));
	fclose(qf);
	close(fi);
	return (0);
}

acct(ip)
	register struct dinode *ip;
{
	register struct fileusage *fup;

	if (ip == NULL)
		return;
	if (ip->di_mode == 0)
		return;
	fup = adduid((u_short)ip->di_uid);
	fup->fu_curfiles++;
	if ((ip->di_mode & IFMT) == IFCHR || (ip->di_mode & IFMT) == IFBLK)
		return;
	fup->fu_curblocks += ip->di_blocks;
}

oneof(target, listp, n)
	char *target;
	register char **listp;
	register int n;
{

	while (n--) {
		if (*listp && strcmp(target, *listp) == 0) {
			*listp = (char *)0;
			return (1);
		}
		listp++;
	}
	return (0);
}

struct dinode *
ginode()
{
	register unsigned long iblk;

	if (dp == NULL || ++dp >= &itab[ITABSZ]) {
		iblk = itod(&sblock, ino);
		bread((u_long)fsbtodb(&sblock, iblk),
		    (char *)itab, sizeof itab);
		dp = &itab[ino % INOPB(&sblock)];
	}
	if (ino++ < ROOTINO)
		return(NULL);
	return(dp);
}

bread(bno, buf, cnt)
	long unsigned bno;
	char *buf;
{
	extern off_t lseek();
	register off_t pos;

	pos = (off_t)dbtob(bno);
	if (lseek(fi, pos, 0) != pos) {
		perror("lseek");
		exit(1);
	}

	(void) lseek(fi, (long)dbtob(bno), 0);
	if (read(fi, buf, cnt) != cnt) {
		perror("read");
		exit(1);
	}
}

struct fileusage *
lookup(uid)
	register u_short uid;
{
	register struct fileusage *fup;

	for (fup = fuhead[uid % FUHASH]; fup != 0; fup = fup->fu_next)
		if (fup->fu_uid == uid)
			return (fup);
	return ((struct fileusage *)0);
}

struct fileusage *
adduid(uid)
	register u_short uid;
{
	struct fileusage *fup, **fhp;
	extern char *calloc();

	fup = lookup(uid);
	if (fup != 0)
		return (fup);
	fup = (struct fileusage *)calloc(1, sizeof(struct fileusage));
	if (fup == 0) {
		fprintf(stderr, "out of memory for fileusage structures\n");
		exit(1);
	}
	fhp = &fuhead[uid % FUHASH];
	fup->fu_next = *fhp;
	*fhp = fup;
	fup->fu_uid = uid;
	if (uid > highuid)
		highuid = uid;
	return (fup);
}

char *
makerawname(name)
	char *name;
{
	register char *cp;
	char tmp, ch, *rindex();
	static char rawname[MAXPATHLEN];

#if defined(RISCOS)
	/*
	 * GOALS:	/dev/usr	   ->  /dev/rusr
	 *		/dev/dsk/isc0d0s6  ->  /dev/rdsk/isc0d0s6
	 *		dev/usr		   ->  dev/rusr
	 *		dev/dsk/isc0d0s6   ->  dev/rdsk/isc0d0s6
	 *		fubar		   ->  fubar
	 */
	register char *cp2;
	char *index();

	strcpy(rawname, name);
	cp = index(rawname, '/');
	if (cp == NULL)			/* no slash at all */
		return(name);
	else if (cp == rawname)	{	/* slash at start */
		cp++;
		cp2 = index(cp, '/');
		if (cp2 != NULL)
			cp = cp2 + 1;
	} else
		cp++;
	for (ch = 'r'; *cp != '\0'; ) {
		tmp = *cp;
		*cp++ = ch;
		ch = tmp;
	}
	*cp++ = ch;
	*cp = '\0';
#else /* !RISCOS */
	strcpy(rawname, name);
	cp = rindex(rawname, '/');
	if (cp == NULL)
		return (name);
	else
		cp++;
	for (ch = 'r'; *cp != '\0'; ) {
		tmp = *cp;
		*cp++ = ch;
		ch = tmp;
	}
	*cp++ = ch;
	*cp = '\0';
#endif /* RISCOS */
	return (rawname);
}

/* Stolen from fsck/main.c */
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
