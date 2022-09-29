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
/* $Header: nfs_compat.h,v 1.12.1.3 90/05/10 06:16:35 wje Exp $ */

#ifndef	_SYS_FS_NFS_COMPAT_
#define	_SYS_FS_NFS_COMPAT_

/*
 * Sun nfs compatibility with bsd and with at&t.
 */
#ifndef BSD
# include "sys/param.h"
# include "sys/cmn_err.h"
# define iprintf	printf
# define ilock(ip)	plock(ip)
# define iunlock(ip)	prele(ip)
# define iuse(ip)	((ip)->i_count++)
# define iunuse(ip)	(plock(ip), iput(ip))
# define IHOLD(ip)	(iuse(ip), ilock(ip))
# ifdef ILOCKED
#  undef ILOCKED
# endif
# define ILOCKED	ILOCK
# define IXSAVED	ITEXT
# define xflush(ip)	xrele(ip)
# define USERPATH	upath
# define BLKDEV_IOSIZE	4096	/* XXX see fs/efs/bmap.c */
# define howmany(x, y)  (((unsigned)((x)+((y)-1)))/(unsigned)(y))
# define roundup(x, y)	((howmany((x), (y)))*(y))

/* AT&T-compatible inode private data pointer type */
typedef	int	*fsptr_t;
#else
/* for SVR3 MP kernel compatibility */
# define ALLOC_LOCK(X)
# define EXTERN_LOCK(X)
# define INITLOCK(X,Y)
# define SPSEMA(X)
# define SVSEMA(X)
# define PSEMA(X,Y)
# define VSEMA(X,Y)

# define appsema(a,b)	1
# define apvsema(a,b)	1
# define psema(a,b)	sleep(a,b)
# define vsema(a,b)	wakeup(a)
# define initsema(a,b)	*a = b
# define initlock(a,b)	*a = b
# define cvsema(a)	wakeup(a)
# define splrf()	spl1()

/* a better inode private data pointer type */
typedef	char	*fsptr_t;
#endif

/*
 * Kernel memory allocation functions.  Use kmem_allocmbuf() and
 * kmem_freembuf() if the memory you're manipulating will be wrapped in
 * an mbuf and shipped to a network interface.
 *	char	*p;
 *	u_int	nbytes;
 */
char	*kmem_alloc(/* nbytes */);
#ifdef NOTDEF
char	*kmem_realloc(/* p, nbytes */);
#endif
void	kmem_free(/* p, nbytes */);

#ifndef BSD
# define kmem_allocmbuf(nbytes)	 kmem_alloc(nbytes)
# define kmem_freembuf(p,nbytes) kmem_free(p,nbytes)
#else
char	*kmem_allocmbuf(/* nbytes */);
int	kmem_freembuf(/* p, nbytes */);
#endif

/* user credentials */

struct ucred {
	u_short	cr_ref;			/* reference count */
	ushort  cr_uid;			/* effective user id */
	ushort  cr_gid;			/* effective group id */
	ushort  cr_groups[NGROUPS];	/* groups, 0 terminated */
	ushort  cr_ruid;		/* real user id */
	ushort  cr_rgid;		/* real group id */
};

#ifdef KERNEL
extern int	nfsdebug;

struct ucred *crget();
struct ucred *crcopy();
struct ucred *crdup();
#endif

/*
 * Get an mbuf which points at non-mbuf memory.
 *	int (*fun)(), len, wait;
 *	long arg;
 *	caddr_t addr;
 */
struct mbuf	*mclgetx(/* fun, arg, addr, len, wait */);

#endif	_SYS_FS_NFS_COMPAT_
