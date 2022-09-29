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
#ident	"$Header: types.c,v 1.2.2.2 90/05/09 15:43:18 wje Exp $"

#include <stdio.h>
#include <sun/mntent.h>

struct typent {
	char *tname;	/* Type name */
	struct typent *next;
};

struct typent *Typehead = NULL;

/*
 * The routine add_types() adds the given items to the list of
 * valid types.  The argument should be a pointer to a list of
 * comma-separated filesystem types.  Note that the contents of
 * the list will be destroyed, the commas replaced by nulls.
 *
 * The linked list is not sorted, and is maintained by adding to
 * the head.
 */

add_types(list)
	char *list;
{
	struct typent *cur_ent;
	char *end;
	char savec;

	while(*list) {
		end = list;
		while (*end && *end != ',') {
			end++;
		}
		savec = *end;  /* not put back */
		*end = '\0';
#if	RISCOS
		if (strcmp(list, "4.3") == 0)
			strncpy(list, "ffs", 3);
#endif	RISCOS
		cur_ent = Typehead;
		while (cur_ent) {
			if (strcmp(cur_ent->tname, list) == 0) {
				return;
			}
			cur_ent = cur_ent->next;
		}
		cur_ent = (struct typent *)malloc(sizeof(struct typent));
		if (cur_ent != NULL) {
			cur_ent->tname = list;
			cur_ent->next = Typehead;
			Typehead = cur_ent;
		}
		list = end;
		if (savec == ',') {
			list++;
		}
	}
}

/*
 * The routine valid_type() returns a 1 if the given type name is found
 * in the list or the list is empty, and 0 if not.  The special list type
 * "local" matches any type except for NFS.
 */

int
valid_type(typename)
	char *typename;
{
	struct typent *cur_ent;
	int local;

	if (Typehead == NULL) {
		return 1;
	}

	cur_ent = Typehead;
	while(cur_ent) {
		if (strcmp(cur_ent->tname, typename) == 0) {
			return 1;
		}
		if (strcmp(cur_ent->tname, "local") == 0) {
			if (strcmp(MNTTYPE_NFS, typename) != 0) {
				return 1;
			}
		}
		cur_ent = cur_ent->next;
	}
	return 0;
}
