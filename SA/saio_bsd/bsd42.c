#ident "$Header: bsd42.c,v 1.2 90/03/06 18:16:04 chungc Exp $"
/*	%Q%	%I%	%M%	*/

/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

#ifndef PROM
/*
 * bsd42.c -- 4.2 bsd file system routines
 * Swiped with minor mods from 4.2BSD standalone code.
 */

#include "sys/errno.h"
#include "sys/param.h"
#include "sys/inode.h"
#include "sys/fs.h"
#define KERNEL
#include "sys/dir.h"
#undef KERNEL
#include "machine/dvh.h"
#include "saio/saio.h"

#define cfs(x)		((struct fs *)(x->i_fs_tape))
#define cinode(x)	((struct inode *)(x->i_ino_dir))

static ino_t dlook();
static struct direct * readdir();

struct dirstuff {
	int loc;
	struct iob *io;
};

/*
 * small buffer pool for caching inode indirect blocks
 */
#define	NBUFS	4
char b[NBUFS][MAXBSIZE+64];
daddr_t blknos[NBUFS];

static
openi(n, io)
register struct iob *io;
{
	struct dinode *dp;
	int cc;

	io->i_bn = fsbtodb(cfs(io), itod(cfs(io), n));
	io->i_cc = cfs(io)->fs_bsize;
	io->i_ma = io->i_buf;
	cc = DEVREAD(io);
	dp = (struct dinode *)io->i_buf;
	cinode(io)->i_ic = dp[itoo(cfs(io), n)].di_ic;
	return (cc);
}

static
find(path, io)
char *path;
struct iob *io;
{
	register char *q;
	char c;
	int n;

	if (path==NULL || *path=='\0') {
		printf("null path\n");
		return (0);
	}

	if (openi((ino_t) ROOTINO, io) < 0) {
		printf("can't read root inode\n");
		return (0);
	}
	while (*path) {
		while (*path == '/')
			path++;
		q = path;
		while(*q != '/' && *q != '\0')
			q++;
		c = *q;
		*q = '\0';

		if ((n = dlook(path, io)) != 0) {
			if (c == '\0')
				break;
			if (openi(n, io) < 0)
				return (0);
			*q = c;
			path = q;
			continue;
		} else {
			printf("%s not found\n", path);
			return (0);
		}
	}
	return (n);
}

static daddr_t
sbmap(io, bn)
register struct iob *io;
daddr_t bn;
{
	register struct inode *ip;
	register int i, j, sh;
	register char* addr; 
	register unsigned int temp;
	daddr_t nb, *bap;

	ip = cinode(io);
	if (bn < 0) {
		printf("bn negative\n");
		return ((daddr_t)0);
	}

	/*
	 * blocks 0..NDADDR are direct blocks
	 */
	if(bn < NDADDR) {
		nb = ip->i_db[bn];
		return (nb);
	}

	/*
	 * addresses NIADDR have single and double indirect blocks.
	 * the first step is to determine how many levels of indirection.
	 */
	sh = 1;
	bn -= NDADDR;
	for (j = NIADDR; j > 0; j--) {
		sh *= NINDIR(cfs(io));
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0) {
		printf("bn ovf %d\n", bn);
		return ((daddr_t)0);
	}

	/*
	 * fetch the first indirect block address from the inode
	 */
	nb = ip->i_ib[NIADDR - j];
	if (nb == 0) {
		printf("bn void %d\n",bn);
		return ((daddr_t)0);
	}

	/*
	 * fetch through the indirect blocks
	 */
	for (; j <= NIADDR; j++) {
		if (blknos[j] != nb) {
			io->i_bn = fsbtodb(cfs(io), nb);

			/* make sure the blocks are 64-byte aligned */
			addr = b[j];
			if ((unsigned int)addr & 0x3f) {
				temp = ((unsigned int)addr & 0x3f);
				addr += (0x40-temp);
			}
			io->i_ma = addr;

			io->i_cc = cfs(io)->fs_bsize;
			if (DEVREAD(io) != cfs(io)->fs_bsize) {
				printf("bn %d: read error\n", io->i_bn);
				return ((daddr_t)0);
			}
			blknos[j] = nb;
		} else {
			addr = b[j];
			if ((unsigned int)addr & 0x3f) {
				temp = ((unsigned int)addr & 0x3f);
				addr += (0x40-temp);
			}
		}
		bap = (daddr_t *)addr;
		sh /= NINDIR(cfs(io));
		i = (bn / sh) % NINDIR(cfs(io));
		nb = bap[i];
		if(nb == 0) {
			printf("bn void %d\n",bn);
			return ((daddr_t)0);
		}
	}
	return (nb);
}

static ino_t
dlook(s, io)
char *s;
struct iob *io;
{
	register struct direct *dp;
	struct inode *ip;
	int len;
	struct dirstuff dirp;

	if (s == NULL || *s == '\0')
		return (0);
	ip = cinode(io);
	if ((ip->i_mode&IFMT) != IFDIR) {
		printf("not a directory\n");
		return (0);
	}
	if (ip->i_size == 0) {
		printf("zero length directory\n");
		return (0);
	}
	len = strlen(s);
	dirp.loc = 0;
	dirp.io = io;
	for (dp = readdir(&dirp); dp != NULL; dp = readdir(&dirp)) {
		if(dp->d_ino == 0)
			continue;
		if (dp->d_namlen == len && !strcmp(s, dp->d_name))
			return (dp->d_ino);
	}
	return (0);
}

/*
 * get next entry in a directory.
 */
static struct direct *
readdir(dirp)
struct dirstuff *dirp;
{
	register struct direct *dp;
	register struct iob *io;
	int off;
	daddr_t lbn, d;

	io = dirp->io;
	for(;;) {
		if (dirp->loc >= cinode(io)->i_size)
			return (NULL);
		off = blkoff(cfs(io), dirp->loc);
		if (off == 0) {
			lbn = lblkno(cfs(io), dirp->loc);
			d = sbmap(io, lbn);
			if(d == 0)
				return NULL;
			io->i_bn = fsbtodb(cfs(io), d);
			io->i_ma = io->i_buf;
			io->i_cc = blksize(cfs(io), cinode(io), lbn);
			if (DEVREAD(io) < 0) {
				printf("bn %d: read error\n", io->i_bn);
				return (NULL);
			}
		}
		dp = (struct direct *)(io->i_buf + off);
		dirp->loc += dp->d_reclen;
		if (dp->d_ino == 0)
			continue;
		return (dp);
	}
}

_bsd42read(io, buf, count)
register struct iob *io;
char *buf;
int count;
{
	int i;
	struct fs *fs;
	int lbn, off, size, pbn;
	int ocount;

	/*
	 * if read beyond eof, reduce count
	 */
	if (io->i_offset + count > cinode(io)->i_size)
		count = cinode(io)->i_size - io->i_offset;
	if (count <= 0)		/* already at or beyond eof */
		return (0);

	ocount = count;
	while (count > 0) {
		fs = cfs(io);
		lbn = lblkno(fs, io->i_offset);
		pbn = fsbtodb(fs, sbmap(io, lbn));
		off = blkoff(fs, io->i_offset);
		size = blksize(fs, cinode(io), lbn);
		if (io->i_bn != pbn) {
			io->i_bn = pbn;
			io->i_ma = io->i_buf;
			io->i_cc = size;
			if (DEVREAD(io) < 0)
				return (-1);
		}
		i = _min(size - off, count);
		if (i <= 0)
			_io_abort("bsd42read screw-up 2");
		bcopy(&io->i_buf[off], buf, i);
		count -= i;
		buf += i;
		io->i_offset += i;
	}
	return (ocount);
}

_bsd42open(io, file, how)
register struct iob *io;
char *file;
int how;
{
	struct device_table *dp;
	char *cp;
	int i;
	extern char *_get_iobbuf();
	extern struct device_table _device_table[];

	io->i_buf = _get_iobbuf();
	if (io->i_flgs & F_WRITE) {
		printf("Can't write files yet.. Sorry\n");
		io->i_errno = EIO;
		goto bad;
	}
	cinode(io)->i_dev = io->i_dp - _device_table;
	io->i_ma = (char *)(cfs(io));
	io->i_cc = SBSIZE;
	io->i_bn = SBLOCK;
	io->i_offset = 0;
	if (DEVREAD(io) != SBSIZE) {
		printf("super block read error\n");
		goto bad;
	}
	if ((i = find(file, io)) == 0) {
		io->i_errno = ESRCH;
		goto bad;
	}
	if (openi(i, io) < 0)
		goto bad;
	return(0);

bad:
	_free_iobbuf(io->i_buf);
	return(-1);
}

_bsd42close(io)
struct iob *io;
{
	_free_iobbuf(io->i_buf);
}

_bsd42init()
{
	if (sizeof (struct inode) > IOB_INODE || sizeof (struct fs) > IOB_FS)
		_io_abort("bad sizes in iob for bsd file system");
	bzero(blknos, sizeof(blknos));
}
#endif !PROM
