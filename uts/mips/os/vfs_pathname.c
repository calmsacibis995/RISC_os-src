/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: vfs_pathname.c,v 1.5.1.2.1.1.1.2 90/11/15 13:32:34 beacker Exp $"

/*	@(#)vfs_pathname.c	2.3 88/06/19 4.0NFSSRC SMI;  from SMI 2.13 86/12/05	*/

/* Originally included param.h systm.h uio.h errno.h pathname.h dirent.h */

#include "sys/types.h"
#include "sys/param.h"
#include "sys/uio.h"
#include "sys/errno.h"
#include "sys/pathname.h"
#include "bsd43/sys/dirent.h"
#include "sys/kmem.h"

/*
 * Pathname utilities.
 *
 * In translating file names we copy each argument file
 * name into a pathname structure where we operate on it.
 * Each pathname structure can hold MAXPATHLEN characters
 * including a terminating null, and operations here support
 * allocating and freeing pathname structures, fetching
 * strings from user space, getting the next character from
 * a pathname, combining two pathnames (used in symbolic
 * link processing), and peeling off the first component
 * of a pathname.
 */

static char *pn_freelist;

/*
 * Allocate contents of pathname structure.
 * Structure itself is typically automatic
 * variable in calling routine for convenience.
 */
pn_alloc(pnp)
	register struct pathname *pnp;
{

	if (pn_freelist) {
		pnp->pn_buf = pn_freelist;
		pn_freelist = *(char **) pnp->pn_buf;
	} else {
		pnp->pn_buf = (char *)kmem_alloc((u_int)MAXPATHLEN);
	}
	pnp->pn_path = (char *)pnp->pn_buf;
	pnp->pn_pathlen = 0;
}

/*
 * Pull a pathname from user user or kernel space
 */
int
pn_get(str, seg, pnp)
	register char *str;
	int seg;
	register struct pathname *pnp;
{
	register int error;

	pn_alloc(pnp);
	if (seg == UIO_USERSPACE) {
		error =
		    copyinstr(str, pnp->pn_path, MAXPATHLEN, &pnp->pn_pathlen);
	} else {
		error =
		    copystr(str, pnp->pn_path, MAXPATHLEN, &pnp->pn_pathlen);
	}
	if (pnp->pn_pathlen != 0)
		pnp->pn_pathlen--;		/* don't count null byte */
	if (error)
		pn_free(pnp);
	return (error);
}

#ifdef notneeded
/*
 * Get next character from a path.
 * Return null at end forever.
 */
pn_getchar(pnp)
	register struct pathname *pnp;
{

	if (pnp->pn_pathlen == 0)
		return (0);
	pnp->pn_pathlen--;
	return (*pnp->pn_path++);
}
#endif notneeded

/*
 * Set pathname to argument string.
 */
pn_set(pnp, path)
	register struct pathname *pnp;
	register char *path;
{
	register int error;

	pnp->pn_path = pnp->pn_buf;
	error = copystr(path, pnp->pn_path, MAXPATHLEN, &pnp->pn_pathlen);
	pnp->pn_pathlen--;		/* don't count null byte */
	return (error);
}

/*
 * Combine two argument pathnames by putting
 * second argument before first in first's buffer,
 * and freeing second argument.
 * This isn't very general: it is designed specifically
 * for symbolic link processing.
 */
pn_combine(pnp, sympnp)
	register struct pathname *pnp;
	register struct pathname *sympnp;
{

	if (pnp->pn_pathlen + sympnp->pn_pathlen >= MAXPATHLEN)
		return (ENAMETOOLONG);
	ovbcopy(pnp->pn_path, pnp->pn_buf + sympnp->pn_pathlen,
	    (u_int)pnp->pn_pathlen);
	bcopy(sympnp->pn_path, pnp->pn_buf, (u_int)sympnp->pn_pathlen);
	pnp->pn_pathlen += sympnp->pn_pathlen;
	pnp->pn_path = pnp->pn_buf;
	return (0);
}

/*
 * Strip next component off a pathname and leave in
 * buffer comoponent which should have room for
 * MAXNAMLEN bytes and a null terminator character.
 */
pn_getcomponent(pnp, component)
	register struct pathname *pnp;
	register char *component;
{
	register char *cp;
	register int l;
	register int n;
	register char c;

	cp = pnp->pn_path;
	l = pnp->pn_pathlen;
	n = MAXNAMLEN;
	while ((l > 0) && (*cp != '/')) {
		if (--n < 0)
			return(ENAMETOOLONG);
		c = *cp++;
#ifndef RISCOS
		if (c & 0x80)
			return (EINVAL);
#endif
		*component++ = c;
		--l;
	}
	pnp->pn_path = cp;
	pnp->pn_pathlen = l;
	*component = 0;
	return (0);
}

/*
 * skip over consecutive slashes in the pathname
 */
void
pn_skipslash(pnp)
	register struct pathname *pnp;
{
	while ((pnp->pn_pathlen > 0) && (*pnp->pn_path == '/')) {
		pnp->pn_path++;
		pnp->pn_pathlen--;
	}
}

/*
 * Free pathname resources.
 */
void
pn_free(pnp)
	register struct pathname *pnp;
{

	/* kmem_free((caddr_t)pnp->pn_buf, (u_int)MAXPATHLEN); */
	*(char **) pnp->pn_buf = pn_freelist;
	pn_freelist = pnp->pn_buf;
	pnp->pn_buf = 0;
}

/*
 * Eliminate any trailing slashes in the pathname.
 */
void
pn_fixslash(pnp)
        register struct pathname *pnp;
{
        register char *buf = pnp->pn_buf;
        register char *path = pnp->pn_path + pnp->pn_pathlen - 1;

        while (path > buf && *path == '/')
                --path;
        *(path + 1) = '\0';
        pnp->pn_pathlen = path - pnp->pn_path + 1;
}
