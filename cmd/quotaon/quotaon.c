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
#ident	"$Header: quotaon.c,v 1.3.1.2 90/05/09 18:22:56 wje Exp $"
/*----- COPYRIGHT (END) -----------------------------------------------------*\

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 * @(#)quotaon.c	1.2 88/03/15 4.0NFSSRC
 * from 5.2 (Berkeley) 8/8/85
 * and from 1.10 SMI 88/02/08
 */

/*
 * Turn quota on/off for a filesystem.
 */
#include <sys/param.h>
#include <sys/file.h>
#include <ufs/quota.h>
#include <stdio.h>
#include <mntent.h>

int	vflag;		/* verbose */
int	aflag;		/* all file systems */

#define QFNAME "quotas"
char	quotafile[MAXPATHLEN + 1];
char	*listbuf[50];
char	*index(), *rindex();
char	*malloc();

main(argc, argv)
	int argc;
	char **argv;
{
	register struct mntent *mntp;
	char **listp;
	int listcnt;
	FILE *mtab, *fstab, *tmp;
	char *whoami, *rindex();
	int offmode = 0;
	int errs = 0;
	char *tmpname = "/etc/quotaontmpXXXXXX";

	whoami = rindex(*argv, '/') + 1;
	if (whoami == (char *)1)
		whoami = *argv;
	if (strcmp(whoami, "quotaoff") == 0)
		offmode++;
	else if (strcmp(whoami, "quotaon") != 0) {
		fprintf(stderr, "Name must be quotaon or quotaoff not %s\n",
			whoami);
		exit(1);
	}
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
	if (argc <= 0 && !aflag) {
		fprintf(stderr, "Usage:\n\t%s [-v] -a\n\t%s [-v] filesys ...\n",
			whoami, whoami);
		exit(1);
	}
	/*
	 * If aflag go through fstab and make a list of appropriate
	 * filesystems.
	 */
	if (aflag) {
		listp = listbuf;
		listcnt = 0;
		fstab = setmntent(MNTTAB, "r");
		while (mntp = getmntent(fstab)) {
#if defined(RISCOS)
			if (! IS_RISCOS_FS_TYPE(mntp->mnt_type)	||
#else
			if (strcmp(mntp->mnt_type, MNTTYPE_43) != 0 ||
#endif /* RISCOS */
			    !hasmntopt(mntp, MNTOPT_QUOTA) ||
			    hasmntopt(mntp, MNTOPT_RO))
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


	/*
	 * Open temporary version of mtab inorder to maintain file
	 * consistency.  By overwriting the /etc/mtab file by using
	 * a rename, anyone who was reading the previous version of
	 * the file will continue to see a consistent version.
	 */
	mktemp(tmpname);
	tmp = setmntent(tmpname, "a");
	if (tmp == NULL) {
		perror(tmpname);
		exit(1);
	}

	/*
	 * Open real mtab
	 */
	mtab = setmntent(MOUNTED, "r");
	if (mtab == NULL) {
		perror(MOUNTED);
		exit(1);
	}

	/*
	 * Loop through mtab writing mount record to temp mtab.
	 * If a file system gets turn on or off modify the mount
	 * record before writing it.
	 */
	while ((mntp = getmntent(mtab)) != NULL) {
#if defined(RISCOS)
		if (IS_RISCOS_FS_TYPE(mntp->mnt_type) &&
#else
		if (strcmp(mntp->mnt_type, MNTTYPE_43) == 0 &&
#endif
		    !hasmntopt(mntp, MNTOPT_RO) &&
		    (oneof(mntp->mnt_fsname, listp, listcnt) ||
		     oneof(mntp->mnt_dir, listp, listcnt)) ) {
			errs += quotaonoff(mntp, offmode);
		}
		addmntent(tmp, mntp);
	}
	endmntent(mtab);
	endmntent(tmp);

	/*
	 * Move temp mtab to mtab
	 */
	if (rename(tmpname, MOUNTED) < 0) {
		perror(MOUNTED);
		exit(1);
	}
	while (listcnt--) {
		if (*listp)
			fprintf(stderr, "Cannot do %s\n", *listp);
		listp++;
	}
	exit(errs);
}

quotaonoff(mntp, offmode)
	register struct mntent *mntp;
	int offmode;
{

	if (offmode) {
		if (quotactl(Q_QUOTAOFF, mntp->mnt_fsname, 0, NULL) < 0)
			goto bad;
		if (vflag)
			printf("%s: quotas turned off\n", mntp->mnt_dir);
	} else {
		(void) sprintf(quotafile, "%s/%s", mntp->mnt_dir, QFNAME);
		if (quotactl(Q_QUOTAON, mntp->mnt_fsname, 0, quotafile) < 0)
			goto bad;
		if (vflag)
			printf("%s: quotas turned on\n", mntp->mnt_dir);
	}
	fixmntent(mntp, offmode);
	return (0);
bad:
	fprintf(stderr, "quotactl: ");
	perror(mntp->mnt_fsname);
#if defined(RISCOS)
	fprintf(stderr, "or: ");
	perror(quotafile);
#endif
	return (1);
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

char opts[1024];

fixmntent(mntp, offmode)
	register struct mntent *mntp;
	int offmode;
{
	register char *qst, *qend;

	if (offmode) {
		if (hasmntopt(mntp, MNTOPT_NOQUOTA))
			return;
		qst = hasmntopt(mntp, MNTOPT_QUOTA);
	} else {
		if (hasmntopt(mntp, MNTOPT_QUOTA))
			return;
		qst = hasmntopt(mntp, MNTOPT_NOQUOTA);
	}
	if (qst) {
		qend = index(qst, ',');
		if (qst != mntp->mnt_opts)
			qst--;			/* back up to ',' */
		if (qend == NULL)
			*qst = '\0';
		else
			while (*qst++ = *qend++);
	}
	sprintf(opts, "%s,%s", mntp->mnt_opts,
	    offmode? MNTOPT_NOQUOTA : MNTOPT_QUOTA);
	mntp->mnt_opts = opts;
}
