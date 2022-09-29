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
#ident	"$Header: all_ents.c,v 1.2.2.2 90/05/09 15:42:06 wje Exp $"

#include <stdio.h>
#include <sun/mntent.h>

extern char	MTAB[];


all_entries()
{
	FILE *mtabp;
	struct mntent *mntp;
	char *colon;	/* end of hostname: in NFS mount */

	if ((mtabp = setmntent(MTAB, "r")) == NULL) {
		return df_perror("", MTAB);
	}

	put_head();
	while ((mntp = getmntent(mtabp)) != NULL) {
		if (!strcmp(mntp->mnt_type, MNTTYPE_IGNORE)) {
			continue;
		}
		if (!valid_type(mntp->mnt_type)) {
			continue;
		}

		/*
		 * If the filesystem type is NFS, check to see if the host is
		 * known to be down.  If not, check to see if it is up, and quit
		 * otherwise.
		 */
		if (strcmp(mntp->mnt_type, MNTTYPE_NFS) == 0) {
			colon = (char *)strchr(mntp->mnt_fsname, ':');
			if (colon == NULL) {	/* NFS, yet not NFS? */
				fprintf(stderr, "Bad NFS mount: %s\n",
					mntp->mnt_fsname);
				continue;
			}
			*colon = '\0';
			if (is_down(mntp->mnt_fsname)) {
				fprintf(stderr,
					"df: Server %s not responding\n",
					mntp->mnt_fsname);
				*colon = ':';
				continue;
			}
			if (!is_up(mntp->mnt_fsname)) {
				*colon = ':';
				continue;
			}
			*colon = ':';
		}
		put_line(mntp);
	}
	endmntent(mtabp);
}
