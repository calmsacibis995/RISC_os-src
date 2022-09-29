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
#ident	"$Header: repquota.c,v 1.2.1.2 90/05/09 18:23:34 wje Exp $"
/*----- COPYRIGHT (END) -----------------------------------------------------*\

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)repquota.c	1.2 88/03/15 4.0NFSSRC
 *	from	5.4 (Berkeley) 3/5/86
 */

/*
 * Quota report
 */
#include <stdio.h>
#include <errno.h>
#include <sys/param.h>
#include <ufs/quota.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <mntent.h>
#include <pwd.h>

#if defined(RISCOS)
extern int errno;
#endif

#define LOGINNAMESIZE	8
struct username {
	struct username *u_next;
	u_short u_uid;
	char u_name[LOGINNAMESIZE + 1];
};
#define UHASH 997
struct username *uhead[UHASH];
struct username *lookup();
struct username *adduid();
int highuid;

char		*malloc();

int	vflag;		/* verbose */
int	aflag;		/* all file systems */
char *listbuf[50];

#define QFNAME "quotas"

main(argc, argv)
	int argc;
	char **argv;
{
	register struct mntent *mntp;
	register struct passwd *pwp;
	register struct username *up;
	char **listp;
	int listcnt;
	char quotafile[MAXPATHLEN + 1];
	FILE *mtab, *fstab;
	int errs = 0;

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
		fprintf(stderr, "Usage:\n\t%s\n\t%s\n",
			"repquota [-v] -a",
			"repquota [-v] filesys ...");
		exit(1);
	}
	(void) setpwent();
	while ((pwp = getpwent()) != 0) {
		up = lookup((u_short)pwp->pw_uid);
		if (up == 0) {
			up = adduid((u_short)pwp->pw_uid);
			strncpy(up->u_name, pwp->pw_name, sizeof(up->u_name));
		}
	}
	(void) endpwent();
	if (quotactl(Q_SYNC, NULL, 0, NULL) < 0 && errno == EINVAL && vflag) {
		printf("Warning: Quotas are not compiled into this kernel\n");
	}
	/*
	 * If aflag go through fstab and make a list of appropriate
	 * filesystems.
	 */
	if (aflag) {
		listp = listbuf;
		listcnt = 0;
		if ((fstab = setmntent(MNTTAB, "r")) == NULL) {
			fprintf(stderr, "Can't open ");
			perror(MNTTAB);
			exit(8);
		}
		while (mntp = getmntent(fstab)) {
#if defined(RISCOS)
			if (! IS_RISCOS_FS_TYPE(mntp->mnt_type) ||
			    ! HAS_RISCOS_QUOTAS(mntp) ||
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
		(void) endmntent(fstab);
		*listp = (char *)0;
		listp = listbuf;
	} else {
		listp = argv;
		listcnt = argc;
	}
	if ((mtab = setmntent(MOUNTED, "r")) == NULL) {
		fprintf(stderr, "Can't open ");
		perror(MOUNTED);
		exit(8);
	}
	while (mntp = getmntent(mtab)) {
#if defined(RISCOS)
		if (IS_RISCOS_FS_TYPE(mntp->mnt_type) &&
#else
		if (strcmp(mntp->mnt_type, MNTTYPE_43) == 0 &&
#endif
		    !hasmntopt(mntp, MNTOPT_RO) &&
		    (oneof(mntp->mnt_fsname, listp, listcnt) ||
		     oneof(mntp->mnt_dir, listp, listcnt)) ) {
			sprintf(quotafile, "%s/%s", mntp->mnt_dir, QFNAME);
			errs +=
			    repquota(mntp->mnt_fsname,mntp->mnt_dir,quotafile);
		}
	}
	(void) endmntent(mtab);
	while (listcnt--) {
		if (*listp)
			fprintf(stderr, "Cannot report on %s\n", *listp);
	}
	exit(errs);
}

repquota(fsdev, fsfile, qffile)
	char *fsdev;
	char *fsfile;
	char *qffile;
{
	FILE *qf;
	u_short uid;
	struct dqblk dqbuf;
	struct stat statb;
	extern int errno;

	if (vflag || aflag)
		printf("*** Quota report for %s (%s):\n", fsdev, fsfile);
	qf = fopen(qffile, "r");
	if (qf == NULL) {
		perror(qffile);
		return (1);
	}
	if (fstat(fileno(qf), &statb) < 0) {
		perror(qffile);
		fclose(qf);
		return (1);
	}
	header();
	for (uid = 0; ; uid++) {
		(void) fread(&dqbuf, sizeof(struct dqblk), 1, qf);
		if (feof(qf))
			break;
		if (!vflag &&
		    dqbuf.dqb_curfiles == 0 && dqbuf.dqb_curblocks == 0)
			continue;
		prquota(uid, &dqbuf);
	}
	fclose(qf);
	return (0);
}

header()
{
#if defined(RISCOS) && defined(TAHOE_QUOTA)
	printf(
"                        Block limits               File limits\n");
	printf(
"User            used    soft    hard  warn    used  soft  hard  warn\n");
#else /* !RISCOS */
	printf(
"                      Block limits                      File limits\n"
	);
	printf(
"User           used   soft   hard    timeleft    used   soft   hard    timeleft\n"
	);
#endif /* RISCOS */
}

prquota(uid, dqp)
	u_short uid;
	struct dqblk *dqp;
{
	struct timeval tv;
	register struct username *up;
	char ftimeleft[80], btimeleft[80];

	if (dqp->dqb_bsoftlimit == 0 && dqp->dqb_bhardlimit == 0 &&
	    dqp->dqb_fsoftlimit == 0 && dqp->dqb_fhardlimit == 0)
		return;
#if defined(RISCOS) && defined(TAHOE_QUOTA)
	up = lookup(uid);
	if (up)
		printf("%-10s", up->u_name);
	else
		printf("#%-9d", uid);
	printf("%c%c%8d%8d%8d %5d   %5d %5d %5d %5d\n",
		dqp->dqb_bsoftlimit && 
		    dqp->dqb_curblocks >= 
		    dqp->dqb_bsoftlimit ? '+' : '-',
		dqp->dqb_fsoftlimit &&
		    dqp->dqb_curfiles >=
		    dqp->dqb_fsoftlimit ? '+' : '-',
		dbtob(dqp->dqb_curblocks) / 1024,
		dbtob(dqp->dqb_bsoftlimit) / 1024,
		dbtob(dqp->dqb_bhardlimit) / 1024,
		dqp->dqb_bwarn,
		dqp->dqb_curfiles,
		dqp->dqb_fsoftlimit,
		dqp->dqb_fhardlimit,
		dqp->dqb_fwarn
	);
#else /* !RISCOS */
	(void) gettimeofday(&tv, NULL);
	up = lookup(uid);
	if (up)
		printf("%-10s", up->u_name);
	else
		printf("#%-9d", uid);
	if (dqp->dqb_bsoftlimit && dqp->dqb_curblocks>=dqp->dqb_bsoftlimit) {
		if (dqp->dqb_btimelimit == 0) {
			strcpy(btimeleft, "NOT STARTED");
		} else if (dqp->dqb_btimelimit > tv.tv_sec) {
			fmttime(btimeleft,
			    (long)(dqp->dqb_btimelimit - tv.tv_sec));
		} else {
			strcpy(btimeleft, "EXPIRED");
		}
	} else {
		btimeleft[0] = '\0';
	}
	if (dqp->dqb_fsoftlimit && dqp->dqb_curfiles>=dqp->dqb_fsoftlimit) {
		if (dqp->dqb_ftimelimit == 0) {
			strcpy(ftimeleft, "NOT STARTED");
		} else if (dqp->dqb_ftimelimit > tv.tv_sec) {
			fmttime(ftimeleft,
			    (long)(dqp->dqb_ftimelimit - tv.tv_sec));
		} else {
			strcpy(ftimeleft, "EXPIRED");
		}
	} else {
		ftimeleft[0] = '\0';
	}
	printf("%c%c%7d%7d%7d%12s %7d%7d%7d%12s\n",
		dqp->dqb_bsoftlimit && 
		    dqp->dqb_curblocks >= 
		    dqp->dqb_bsoftlimit ? '+' : '-',
		dqp->dqb_fsoftlimit &&
		    dqp->dqb_curfiles >=
		    dqp->dqb_fsoftlimit ? '+' : '-',
		dbtob(dqp->dqb_curblocks) / 1024,
		dbtob(dqp->dqb_bsoftlimit) / 1024,
		dbtob(dqp->dqb_bhardlimit) / 1024,
		btimeleft,
		dqp->dqb_curfiles,
		dqp->dqb_fsoftlimit,
		dqp->dqb_fhardlimit,
		ftimeleft
	);
#endif /* RISCOS */
}

#if !(defined(RISCOS) && defined(TAHOE_QUOTA))
fmttime(buf, time)
	char *buf;
	register long time;
{
	int i;
	static struct {
		int c_secs;		/* conversion units in secs */
		char * c_str;		/* unit string */
	} cunits [] = {
		{60*60*24*28, "months"},
		{60*60*24*7, "weeks"},
		{60*60*24, "days"},
		{60*60, "hours"},
		{60, "mins"},
		{1, "secs"}
	};

	if (time <= 0) {
		strcpy(buf, "EXPIRED");
		return;
	}
	for (i = 0; i < sizeof(cunits)/sizeof(cunits[0]); i++) {
		if (time >= cunits[i].c_secs)
			break;
	}
	sprintf(buf, "%.1f %s", (double)time/cunits[i].c_secs, cunits[i].c_str);
}
#endif /* !RISCOS */

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

struct username *
lookup(uid)
	u_short uid;
{
	register struct username *up;

	for (up = uhead[uid % UHASH]; up != 0; up = up->u_next)
		if (up->u_uid == uid)
			return (up);
	return ((struct username *)0);
}

struct username *
adduid(uid)
	u_short uid;
{
	struct username *up, **uhp;
	extern char *calloc();

	up = lookup(uid);
	if (up != 0)
		return (up);
	up = (struct username *)calloc(1, sizeof(struct username));
	if (up == 0) {
		fprintf(stderr, "out of memory for username structures\n");
		exit(1);
	}
	uhp = &uhead[uid % UHASH];
	up->u_next = *uhp;
	*uhp = up;
	up->u_uid = uid;
	if (uid > highuid)
		highuid = uid;
	return (up);
}
