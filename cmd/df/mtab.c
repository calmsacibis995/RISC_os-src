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
#ident	"$Header: mtab.c,v 1.3.2.2 90/05/09 15:43:12 wje Exp $"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sun/mntent.h>
#include <sys/param.h>
#include <errno.h>

extern char	MTAB[];

struct hentry {
	char hostname[MAXHOSTNAMELEN + 1];
	struct mentry *m_ent;
	struct hentry *hnext;
};

struct mentry {
	struct mntent mnt;
	dev_t device;
	int status;	/* 0 if no stat yet, -1 if error, 1 if OK and statted */
	struct mentry *mnext;
};

static struct hentry Head;

#ifdef DEBUG
static void dump_list();
#endif

char *
emalloc(size)
	int size;
{
	char *new;

	new = (char *)malloc(size);
	if (new == NULL) {
		fprintf(stderr, "df: Out of memory\n");
		exit(1);
	}

	return new;
}

/*
 * get_mtab() reads the mount table and sets up the initial lists.
 */

get_mtab()
{
	struct mntent *mntp;
	FILE *mtabp;
	struct mentry *mnew;
	struct hentry *hprev;
	struct hentry *hcur;
	char *colon;
	char curhost[MAXHOSTNAMELEN + 1];

	Head.hnext = NULL;
	Head.m_ent = NULL;
	Head.hostname[0] = '\0';

	if ((mtabp = setmntent(MTAB, "r")) == NULL) {
		return df_perror("", MTAB);
	}

	while ((mntp = getmntent(mtabp)) != NULL) {
		if (!strcmp(mntp->mnt_type, MNTTYPE_IGNORE)) {
			continue;
		}
		hcur = &Head;
		if (!strcmp(mntp->mnt_type, MNTTYPE_NFS)) {
			if (!valid_type(MNTTYPE_NFS)) {
				continue;
			}
			strcpy(curhost, mntp->mnt_fsname);
			colon = (char *)strchr(curhost, ':');
			if (colon == NULL) {
				fprintf(stderr, "df: Bad NFS mount %s\n",
					curhost);
				continue;
			}
			*colon = '\0';
			hprev = hcur;
			hcur = Head.hnext;
			while (hcur) {
				if (!strcmp(curhost, hcur->hostname)) {
					break;
				}
				hprev = hcur;
				hcur = hcur->hnext;
			}
			if (hcur == NULL) {
				hcur = (struct hentry *)emalloc(sizeof(struct hentry));
				hcur->m_ent = NULL;
				hcur->hnext = NULL;
				hprev->hnext = hcur;
				strcpy(hcur->hostname, curhost);
			}
		}

		/*
		 * hcur now points at a list of mentry structures for this
		 * hostname.
		 */

		mnew = (struct mentry *)emalloc(sizeof(struct mentry));
		mnew->mnt.mnt_freq = mntp->mnt_freq;
		mnew->mnt.mnt_passno = mntp->mnt_passno;

		mnew->mnt.mnt_fsname = (char *)emalloc(strlen(mntp->mnt_fsname) + 1);
		strcpy(mnew->mnt.mnt_fsname, mntp->mnt_fsname);

		mnew->mnt.mnt_dir = (char *)emalloc(strlen(mntp->mnt_dir) + 1);
		strcpy(mnew->mnt.mnt_dir, mntp->mnt_dir);

		mnew->mnt.mnt_type = (char *)emalloc(strlen(mntp->mnt_type) + 1);
		strcpy(mnew->mnt.mnt_type, mntp->mnt_type);

		mnew->mnt.mnt_opts = (char *)emalloc(strlen(mntp->mnt_opts) + 1);
		strcpy(mnew->mnt.mnt_opts, mntp->mnt_opts);

		mnew->status = 0;

		mnew->mnext = hcur->m_ent;
		hcur->m_ent = mnew;
	}
	endmntent(mtabp);
#ifdef DEBUG
	dump_list();
#endif
}

struct mntent *
get_ment(ddev)
	dev_t ddev;
{
	struct mentry *mcur;
	struct hentry *hcur;
	struct stat stb;

	mcur = Head.m_ent;
	while (mcur) {
		if (mcur->status == 0) {
			if (stat(mcur->mnt.mnt_dir, &stb) < 0) {
				mcur->status = -1;
				df_perror("Bad mount", mcur->mnt.mnt_dir);
			} else {
				mcur->status = 1;
				mcur->device = stb.st_dev;
			}
		}
		if (mcur->status == -1) {
			mcur = mcur->mnext;
			continue;
		}
		if (mcur->device == ddev) {
#ifdef DEBUG
			dump_list();
#endif
			return (&(mcur->mnt));
		}
		mcur = mcur->mnext;
	}

	if (!valid_type(MNTTYPE_NFS)) {
		return ((struct mntent *)NULL);
	}

	hcur = Head.hnext;
	while (hcur) {
		if (is_down(hcur->hostname) || !is_up(hcur->hostname)) {
			hcur = hcur->hnext;
			continue;
		}
		mcur = hcur->m_ent;
		while (mcur) {
			if (mcur->status == -1) {
				mcur = mcur->mnext;
				continue;
			}
			if (stat(mcur->mnt.mnt_dir, &stb) < 0) {
				mcur->status = -1;
				if (errno == ETIMEDOUT) {
					mark_down(hcur->hostname);
					break;
				}
				df_perror("Bad mount", mcur->mnt.mnt_dir);
			} else {
				mcur->status = 1;
				mcur->device = stb.st_dev;
			}
			if (mcur->status == -1) {
				mcur = mcur->mnext;
				continue;
			}
			if (mcur->device == ddev) {
#ifdef DEBUG
				dump_list();
#endif
				return (&(mcur->mnt));
			}
			mcur = mcur->mnext;
		}
		hcur = hcur->hnext;
	}

	return ((struct mntent *)NULL);
}

#ifdef DEBUG
static void
dump_list()
{
	struct mentry *mcur;
	struct hentry *hcur;

	hcur = &Head;

	while (hcur) {
		if (hcur->hostname[0] == '\0') {
			printf("Entries for local host:\n");
		} else {
			printf("\nEntries for host %s:\n", hcur->hostname);
		}
		mcur = hcur->m_ent;
		while (mcur) {
			printf("\n\tFilesystem: %s\n", mcur->mnt.mnt_fsname);
			printf("\tPath      : %s\n",
				mcur->mnt.mnt_dir ? mcur->mnt.mnt_dir : "");
			printf("\tType      : %s\n", mcur->mnt.mnt_type);
			printf("\tStatus    : %d\n", mcur->status);
			printf("\tDevice    : %d\n", mcur->device);
			mcur = mcur->mnext;
		}
		hcur = hcur->hnext;
	}
}
#endif
