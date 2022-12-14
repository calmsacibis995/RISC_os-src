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
#ident	"$Header: ufs_bmap.c,v 1.3.1.4 90/05/22 18:29:12 wje Exp $"

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)ufs_bmap.c	2.2 88/06/09 4.0NFSSRC SMI";  from UCB 7.1 6/5/86 */

/* Originally included param.h systm.h ../ufs/fsdir.h user.h vnode.h buf.h 
 * proc.h ../ufs/inode.h ../ufs/fs.h 
 */

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/vnode.h"
#include "sys/buf.h"
#include "sys/errno.h"
#include "sys/fs/ufs_fsdir.h"
#include "sys/fs/ufs_inode.h"
#include "sys/fs/ufs_fs.h"

/*
 * Bmap defines the structure of file system storage
 * by returning the physical block number on a device given the
 * inode and the logical block number in a file.
 * When convenient, it also leaves the physical
 * block number of the next block of the file in rablock
 * for use in read-ahead.
 */
/*VARARGS3*/
daddr_t
bmap(ip, bn, rwflg, size, sync)
	register struct inode *ip;
	daddr_t bn;
	int rwflg;
	int size;	/* supplied only when rwflg == B_WRITE */
	int *sync;	/* supplied only when rwflg == B_WRITE */
{
	int nmra = 0;

	return (mbmap(ip, bn, rwflg, &nmra, NULL, NULL, size, sync));
}

daddr_t
mbmap(ip, bn, rwflg, nmra, rablock, rasize, size, sync)
	register struct inode *ip;
	daddr_t bn;
	int rwflg;
	int *nmra;
	daddr_t *rablock;
	int	*rasize;
	int size;	/* supplied only when rwflg == B_WRITE */
	int *sync;	/* supplied only when rwflg == B_WRITE */
{
	register int i;
	int osize, nsize;
	struct buf *bp, *nbp;
	struct fs *fs;
	int j, sh;
	daddr_t nb, lbn, *bap, pref, blkpref();
	int nrahead = 0, k;

	if (bn < 0) {
		u.u_error = EFBIG;
		return ((daddr_t)0);
	}
	fs = ip->i_fs;

	/*
	 * If the next write will extend the file into a new block,
	 * and the file is currently composed of a fragment
	 * this fragment has to be extended to be a full block.
	 */
	nb = lblkno(fs, ip->i_size);
	if (rwflg == B_WRITE && nb < NDADDR && nb < bn && ip->i_db[nb] != 0) {
		osize = blksize(fs, ip, nb);
		if (osize < fs->fs_bsize && osize > 0) {
			bp = realloccg(ip, ip->i_db[nb],
				blkpref(ip, nb, (int)nb, &ip->i_db[0]),
				osize, (int)fs->fs_bsize);
			if (bp == NULL)
				return ((daddr_t)-1);
			ip->i_size = (nb + 1) * fs->fs_bsize;
			ip->i_db[nb] = dbtofsb(fs, bp->b_blkno);
			ip->i_flag |= IUPD|ICHG;
			/*
			 * if syncronous operation is specified, then
			 * write out the new block synchronously, then
			 * update the inode to make sure it points to it
			 */
			if (sync) {
				bwrite(bp);
				iupdat(ip, 1);
			} else {
				bdwrite(bp);
			}
		}
	}
	/*
	 * The first NDADDR blocks are direct blocks
	 */
	if (bn < NDADDR) {
		nb = ip->i_db[bn];
		if (rwflg == B_READ) {
			if (nb == 0)
				return ((daddr_t)-1);
			goto gotit;
		}
		if (nb == 0 || ip->i_size < (bn + 1) * fs->fs_bsize) {
			if (nb != 0) {
				/* consider need to reallocate a frag */
				osize = fragroundup(fs, blkoff(fs, ip->i_size));
				nsize = fragroundup(fs, size);
				if (nsize <= osize)
					goto gotit;
				bp = realloccg(ip, nb,
					blkpref(ip, bn, (int)bn, &ip->i_db[0]),
					osize, nsize);
			} else {
				if (ip->i_size < (bn + 1) * fs->fs_bsize)
					nsize = fragroundup(fs, size);
				else
					nsize = fs->fs_bsize;
				bp = alloc(ip,
					blkpref(ip, bn, (int)bn, &ip->i_db[0]),
					nsize);
			}
			if (bp == NULL)
				return ((daddr_t)-1);
			nb = dbtofsb(fs, bp->b_blkno);
			if (sync) {
				*sync = 1;
			}
			if ((ip->i_mode&IFMT) == IFDIR)
				/*
				 * Write directory blocks synchronously
				 * so they never appear with garbage in
				 * them on the disk.
				 */
				bwrite(bp);
			else
				bdwrite(bp);
			ip->i_db[bn] = nb;
			ip->i_flag |= IUPD|ICHG;
		}
gotit:
		for (k = 1;
		     (k <= *nmra) && (bn + k < NDADDR);
		     k++, rablock++, rasize++, nrahead++) {
			if (ip->i_db[bn+k] == 0)
				break;
			*rablock = fsbtodb(fs, ip->i_db[bn + k]);
			*rasize = blksize(fs, ip, bn + k);
		}
		*nmra = nrahead;
		return (nb);
	}

	/*
	 * Determine how many levels of indirection.
	 */
	pref = 0;
	sh = 1;
	lbn = bn;
	bn -= NDADDR;
	for (j = NIADDR; j>0; j--) {
		sh *= NINDIR(fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0) {
		u.u_error = EFBIG;
		return ((daddr_t)0);
	}

	/*
	 * fetch the first indirect block
	 */
	nb = ip->i_ib[NIADDR - j];
	if (nb == 0) {
		if (rwflg == B_READ)
			return ((daddr_t)-1);
		pref = blkpref(ip, lbn, 0, (daddr_t *)0);
	        bp = alloc(ip, pref, (int)fs->fs_bsize);
		if (bp == NULL)
			return ((daddr_t)-1);
		nb = dbtofsb(fs, bp->b_blkno);
		/*
		 * Write synchronously so that indirect blocks
		 * never point at garbage.
		 */
		bwrite(bp);
		ip->i_ib[NIADDR - j] = nb;
		ip->i_flag |= IUPD|ICHG;
		if (sync) {
			*sync = 1;
		}
	}

	/*
	 * fetch through the indirect blocks
	 */
	for (; j <= NIADDR; j++) {
		bp = bread(ip->i_devvp, fsbtodb(fs, nb), (int)fs->fs_bsize);
		if (bp->b_flags & B_ERROR) {
			brelse(bp);
			return ((daddr_t)0);
		}
		bap = bp->b_un.b_daddr;
		sh /= NINDIR(fs);
		i = (bn / sh) % NINDIR(fs);
		nb = bap[i];
		if (nb == 0) {
			if (rwflg==B_READ) {
				brelse(bp);
				return ((daddr_t)-1);
			}
			if (pref == 0)
				if (j < NIADDR)
					pref = blkpref(ip, lbn, 0,
						(daddr_t *)0);
				else
					pref = blkpref(ip, lbn, i, &bap[0]);
		        nbp = alloc(ip, pref, (int)fs->fs_bsize);
			if (nbp == NULL) {
				brelse(bp);
				return ((daddr_t)-1);
			}
			nb = dbtofsb(fs, nbp->b_blkno);
			if (j < NIADDR || (ip->i_mode&IFMT) == IFDIR)
				/*
				 * Write synchronously so indirect blocks
				 * never point at garbage and blocks
				 * in directories never contain garbage.
				 */
				bwrite(nbp);
			else
				bdwrite(nbp);
			bap[i] = nb;
			if (sync)
				bwrite(bp);
			else
				bdwrite(bp);

		} else
			brelse(bp);
	}

	/*
	 * calculate read-ahead.
	 */
	for (k = 1;
	     (k <= *nmra) && (i + k < NINDIR(fs));
	     k++, rablock++, rasize++, nrahead++) {
		if (bap[i+k] == 0)
			break;
		*rablock = fsbtodb(fs, bap[i + k]);
		*rasize = fs->fs_bsize;
	}
	*nmra = nrahead;
	return (nb);
}
