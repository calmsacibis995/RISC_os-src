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
#ident	"$Header: fstab.c,v 1.2.1.2 90/05/07 20:36:25 wje Exp $"

#include <stdio.h>
#include <ctype.h>
#ifdef SYSTYPE_BSD43
#include <fstab.h>
#include <mntent.h>
#endif
#ifdef SYSTYPE_SYSV
#include <sun/fstab.h>
#include <sun/mntent.h>
#endif

static	struct fstab fs;
static	char line[BUFSIZ+1];
static	FILE *fs_file = 0;

static
fstabscan(fs)
	struct fstab *fs;
{
	struct mntent *mnt;

	while (((mnt = getmntent(fs_file)) != NULL)
	    && (strcmp(mnt->mnt_type, MNTTYPE_NFS) == 0));
	if (mnt == NULL)
		return (EOF);
	fs->fs_spec = mnt->mnt_fsname;
	fs->fs_file = mnt->mnt_dir;
	if (strcmp(mnt->mnt_type, MNTTYPE_IGNORE) == 0) {
		strcpy(mnt->mnt_opts, FSTAB_XX);
	} else if (strcmp(mnt->mnt_type, MNTTYPE_SWAP) == 0) {
		strcpy(mnt->mnt_opts, FSTAB_SW);
	} else if (hasmntopt(mnt, MNTOPT_RO)) {
		strcpy(mnt->mnt_opts, FSTAB_RO);
	} else if (hasmntopt(mnt, MNTOPT_QUOTA)) {
		strcpy(mnt->mnt_opts, FSTAB_RQ);
	} else {
		strcpy(mnt->mnt_opts, FSTAB_RW);
	}
	fs->fs_type = mnt->mnt_opts;
	fs->fs_freq = mnt->mnt_freq;
	fs->fs_passno = mnt->mnt_passno;
	return (5);
}
	
setfsent()
{

	if (fs_file)
		endfsent();
	if ((fs_file = setmntent(FSTAB, "r")) == NULL) {
		fs_file = 0;
		return (0);
	}
	return (1);
}

endfsent()
{

	if (fs_file) {
		endmntent(fs_file);
		fs_file = 0;
	}
	return (1);
}

struct fstab *
getfsent()
{
	int nfields;

	if ((fs_file == 0) && (setfsent() == 0))
		return ((struct fstab *)0);
	nfields = fstabscan(&fs);
	if (nfields == EOF || nfields != 5)
		return ((struct fstab *)0);
	return (&fs);
}

struct fstab *
getfsspec(name)
	char *name;
{
	register struct fstab *fsp;

	if (setfsent() == 0)	/* start from the beginning */
		return ((struct fstab *)0);
	while((fsp = getfsent()) != 0)
		if (strcmp(fsp->fs_spec, name) == 0)
			return (fsp);
	return ((struct fstab *)0);
}

struct fstab *
getfsfile(name)
	char *name;
{
	register struct fstab *fsp;

	if (setfsent() == 0)	/* start from the beginning */
		return ((struct fstab *)0);
	while ((fsp = getfsent()) != 0)
		if (strcmp(fsp->fs_file, name) == 0)
			return (fsp);
	return ((struct fstab *)0);
}

struct fstab *
getfstype(type)
	char *type;
{
	register struct fstab *fs;

	if (setfsent() == 0)
		return ((struct fstab *)0);
	while ((fs = getfsent()) != 0)
		if (strcmp(fs->fs_type, type) == 0)
			return (fs);
	return ((struct fstab *)0);
}
