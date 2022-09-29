#ident "$Header: att52.c,v 1.1 87/08/18 16:32:32 mdove Exp $"
#ifdef SYSV
#ifndef PROM
/*
 * ATT's header files
 */
#include "sysv/param.h"
#include "sysv/types.h"
#include "sysv/errno.h"
#include "sysv/filsys.h"
#include "sysv/inode.h"
#include "sysv/ino.h"
#include "sysv/dir.h"
#include "sysv/fblk.h"
#include "sysv/sysmacros.h"
#include "saio/saio.h"

#define MAXBSIZE IOB_FS	/* same as in 42 */
#define DEV_BSIZE 512
#define WRITABLEFS
#define MAPTODEV(type) (type == 2 ? Fs2BLK : 0)
#define cinode(x)	((struct inode *)(x->i_ino_dir))
#define cfs(x)		((struct filsys *)(x->i_fs_tape))

typedef	struct fblk *FBLKP;

/*
 * Do a little buffering for the direct blocks.
 */
static int IndList[NIDBLKS];
static char IndBlk[NIDBLKS][DEV_BSIZE * 2];

_att52open (io, file, how)
	register struct iob *io;
	char *file;
	int how;
{
	struct device_table *dp;
	int i;
	extern char *_get_iobbuf();
	extern struct device_table _device_table[];

	io->i_buf = _get_iobbuf();

#ifndef WRITABLEFS
	if (io->i_flgs & F_WRITE) {
		printf ("Can't write files yet .... Sorry\n");
		io->i_errno = EIO;
		goto bad;
	}
#endif !WRITABLEFS

	cinode(io)->i_dev = io->i_dp - _device_table;
	io->i_ma = io->i_fs_tape;
	io->i_cc = sizeof (struct filsys);
	io->i_bn = SUPERB;
	if (DEVREAD(io) != sizeof (struct filsys)) {
		printf ("super block read error\n");
		goto bad;
	}

	if (cfs(io)->s_magic != FsMAGIC) {
		printf ("not a System 5 file system\n");
		goto bad;
	}

	if (!findfile (file, io)) {
		io->i_errno = ENOENT;
		goto bad;
	}
	return (0);
bad:
	_free_iobbuf(io->i_buf);
	return (-1);
}

_att52read (io, buf, count)
	register struct iob *io;
	char *buf;
	int count;
{
	register int ocount, pbn, off, i, fstype;

	if (io->i_offset + count > cinode(io)->i_size)
		count = cinode(io)->i_size - io->i_offset;
	if (count <= 0)
		return (0);
	
	fstype = cfs(io)->s_type;
	ocount = count;
	while (count > 0) {
		pbn = lbmap (io, io->i_offset >> FsBSHIFT(MAPTODEV(fstype)));
		off = io->i_offset & FsBMASK(MAPTODEV(fstype));
		if (io->i_bn != pbn) {
			io->i_bn = pbn * (FsBSIZE(MAPTODEV(fstype))/DEV_BSIZE);
			io->i_ma = io->i_buf;
			io->i_cc = FsBSIZE(MAPTODEV(fstype));
			if (DEVREAD(io) != FsBSIZE(MAPTODEV(fstype)))
				return (-1);
		}
		i = _min(FsBSIZE(MAPTODEV(fstype)) - off, count);
		if (i <= 0)
			_io_abort("att52read screw-up");
		bcopy(&io->i_buf[off], buf, i);
		count -= i;
		buf += i;
		io->i_offset += i;
	}
	return (ocount);
}

#ifdef WRITABLEFS
_att52write (io, buf, count)
	register struct iob *io;
	char *buf;
	int count;
{
	register int ocount, pbn, off, i, fstype;

	if (count <= 0)
		return (0);
	
	fstype = cfs(io)->s_type;
	ocount = count;
	while (count > 0) {
		pbn = lbmap (io, io->i_offset >> FsBSHIFT(MAPTODEV(fstype)));
		off = io->i_offset & FsBMASK(MAPTODEV(fstype));
		if (io->i_bn != pbn) {
			io->i_bn = pbn * (FsBSIZE(MAPTODEV(fstype))/DEV_BSIZE);
			io->i_ma = io->i_buf;
			io->i_cc = FsBSIZE(MAPTODEV(fstype));
			if (DEVREAD(io) != FsBSIZE(MAPTODEV(fstype)))
				return (-1);
		}
		i = _min(FsBSIZE(MAPTODEV(fstype)) - off, count);
		if (i <= 0)
			_io_abort("att52write screw-up");
		bcopy(buf, &io->i_buf[off], i);
		if (DEVWRITE(io) != FsBSIZE(MAPTODEV(fstype)))
			return (-1);
		count -= i;
		buf += i;
		io->i_offset += i;

		/*
		 * Write inode when we close the file.
		 * Delayed until then or wait until a new block 
		 * is allocated.
		 */
		if (io->i_offset > cinode(io)->i_size) {
			cinode(io)->i_size = io->i_offset;
			cinode(io)->i_flag |= IUPD|IACC;
		}
	}
	return (ocount);
}
#endif WRITABLEFS

_att52close (io)
	struct iob *io;
{
	register struct filsys *fs;
	int time;

#ifdef WRITABLEFS
	if (cfs(io)->s_fmod) {
		fs = cfs(io);
		fs->s_state = FsOKAY - fs->s_time;
		fs->s_fmod = 0;
		io->i_ma = (char *)fs;
		io->i_cc = sizeof (struct filsys);
		io->i_bn = SUPERB;
		DEVWRITE(io);
	}
	if (cinode(io)->i_flag & (IUPD|IACC|ICHG)) {
		time = get_tod();
		iupdat (io, &time, &time);
	}
#endif WRITABLEFS

	_free_iobbuf (io->i_buf);
}

_att52init()
{	
	if (sizeof (struct inode) > IOB_INODE || 
	    sizeof (struct filsys) > IOB_FS)
		_io_abort("bad sizes in iob for att file system");
	bzero (IndList, sizeof(IndList));
}

/*
**	findfile - Find inode for boot program.
**
**	This routine searches through the file system for the entry named
**	fname and copies the corresponding inode to Binode.
**
**	Return Codes:
**		success => 1 returned and Fndinode is set
**		  to the inode found for the file.
**		fail => 0 returned.
**		   Linode is set to be the inode of the last good directory
**		   in the specified path.
*/

static
findfile(fname, io)
	char	*fname;		/* file name to be searched for */
	register struct iob *io;
{

	struct inode di;		/* intermediate directory inode */
	register struct direct	*dep;	/* ptr to directory entry */
	register struct inode	*dip;	/* ptr to current directory inode */
	register int	i,	/* loop control */
			off,	/* directory file offset */
			found;	/* flag indicating directory search success */
	register char	*sp;	/* ptr to name being searched */
	char		ent[DIRSIZ + 1];/* current path entry wanted */
	int		n,	/* loop control */
			fstype;

	ent[0] = '/';
	ent[1] = 0;
	sp = fname;
	dip = cinode(io);
	fstype = cfs(io)->s_type;
	dip->i_number = ROOTINO;
	liread (dip, io);

	/*
	 * Skip over leading blanks, tabs, and slashes.
	 * All paths are grounded at root.
	 */
	while(*sp && (*sp == ' ' || *sp == '\t' || *sp == '/'))
		sp++;

	/*
	 * Search through directories until the end is found.
	 */
	while(*sp) {

		/* Verify that we have a directory inode. */
		if((dip->i_mode & IFMT) != IFDIR) {

			/* Not a directory. */
			printf("%s not a directory.\n", ent);
			return(0);
		}
		if(dip->i_nlink == 0) {

			/* Zero link count. */
			printf("Link count = 0 for %s.\n", ent);
			return(0);
		}
		if((dip->i_mode & 0111) == 0) {

			/* Directory entry in search path is not a searchable
				directory. */
			printf("Directory %s not searchable.\n", ent);
			return(0);
		}

		/* 
		 * Fetch current path name part and
		 * position sp for next entry.
		 */
		for(i = 0; i < DIRSIZ && *sp && *sp != '/'; ent[i++] = *sp++);
		if(i >= DIRSIZ)
			while(*sp && *sp != '/')
				sp++;
		ent[i] = '\0';
		while(*sp == '/')
			*sp++;

		/* Search current directory for wanted entry. */
		for(off = found = 0; !found && off < dip->i_size;
		    off += FsBSIZE(MAPTODEV(fstype))) {

			/* Read next directory block. */
			if (!rwb(io, off, dip, F_READ)) {
				return(0);
			}

			/* Search it. */
			dep = (struct direct *)io->i_buf;
			for(n = FsBSIZE(MAPTODEV(fstype)) / sizeof(struct direct);
			   !found && n--; ) {
				if(dep->d_ino == 0) {
					dep++;
					continue;
				}
				for(i = 0; i < DIRSIZ; i++) {
					if(ent[i] != dep->d_name[i])
						break;
					if(ent[i] == '\0')
						i = DIRSIZ;
				}
				if(i >= DIRSIZ)
					found = 1;
				else
					dep++;
			}
		}
		if(!found) {
#ifdef WRITABLEFS
			if ((io->i_flgs & F_WRITE) && maknode (ent, io))
				return (1);
			else
#endif WRITABLEFS
			return(0);
		}

		/* Unpack next inode in appropriate place. */
		dip->i_number = dep->d_ino;
		liread(dip, io);
	}
	if((dip->i_mode & IFMT) == IFDIR && dip->i_nlink) {
		return(0);
	}
	return(1);
}

/*
**	lbmap - Little bmap.
**
**	This routine maps a block number within a file to a block number within
**	a file system.  Zero is returned if the block is not allocated.
**
**	This code is a stripped version of bmap(os/subr.c).
*/

static daddr_t
lbmap(io, bn)
	struct iob	*io;	
	register int	bn;	/* block # */
{
	register struct inode	*ip;
	register daddr_t	nb,	/* next block wanted */
				tb,	/* temp block */
				*bap;	/* ptr to indirect block */
	register int		j,	/* indirect block indicator */
				sh,	/* shift count */
				fstype;	/* what block size to use */
	int time;

	ip = cinode(io);
	fstype = cfs(io)->s_type;

	/* 0..NADDR-4 are direct blocks. */
	if(bn < NADDR - NIDBLKS) {
		nb = ip->i_addr[bn];
		if (nb == 0 && io->i_flgs & F_WRITE) {
			nb = alloc(io);
			ip->i_addr[bn] = nb;
			ip->i_flag = IUPD;
			time = get_tod();
			iupdat(io, &time, &time);
		}
		return(nb);
	}

	/*
	 * Addresses NADDR-3, NADDR-2, and NADDR-1 have single, 
	 * double, and triple indirect blocks.  Determine how 
	 * many levels.
	 */
	sh = 0;
	nb = 1;
	bn -= NADDR - NIDBLKS;
	for(j = NIDBLKS; j > 0; j--) {
		sh += FsNSHIFT(MAPTODEV(fstype));
		nb <<= FsNSHIFT(MAPTODEV(fstype));
		if(bn < nb)
			break;
		bn -= nb;
	}
	if(j == 0) {
		/* File too big. */
                printf ("bn over flow %D\n", bn);
                return ((daddr_t) 0);
        }

	nb = ip->i_addr[NADDR - j];
	if (nb == 0) {
		if (io->i_flgs & F_WRITE) {
			nb = alloc(io);
			ip->i_addr[NADDR - j] = nb;
			ip->i_flag = IUPD;
			time = get_tod();
			iupdat(io, &time, &time);
		}
		else {
			printf ("bn void %d\n", bn);
			return ((daddr_t) 0);
		}
	}

	/* Fetch through the indirect blocks. */
	for(; j <= NIDBLKS; j++) {
		/* Single indirect. */
		if(IndList[j] != nb) {
			IndList[j] = nb;
			io->i_ma = (char *)IndBlk[j];
			io->i_cc = FsBSIZE(MAPTODEV(fstype));
			io->i_bn = (FsBSIZE(MAPTODEV(fstype))/DEV_BSIZE) * nb;
			if (DEVREAD(io) != FsBSIZE(MAPTODEV(fstype))) {
				printf ("bn %D: read error\n", bn);
				return ((daddr_t) 0);
			}
		}
		bap = (int *)IndBlk[j];
		sh -= FsNSHIFT(MAPTODEV(fstype));
		nb = bap[(bn >> sh) & FsNMASK(MAPTODEV(fstype))];
		if (nb == 0) {
			if (io->i_flgs & F_WRITE) {
				nb = alloc(io);
				bap[(bn>>sh) & FsNMASK(MAPTODEV(fstype))] = nb;
				io->i_ma = (char *)bap;
				io->i_cc = FsBSIZE(MAPTODEV(fstype));
				io->i_bn = IndList[j] *
					(FsBSIZE(MAPTODEV(fstype))/DEV_BSIZE);
				DEVWRITE(io);
			}
			else
				return (0);
		}
	}
	return(nb);
}

/*
**	liread - Little iread.
**
**	This routine reads in and unpacks the inode pointed to by ip.  The
**	root file system is assumed.  The inode # is taken from the inode.
**	Other fields are filled in from disk.
**
**	This is a stripped version of iread(os/iget.c).
*/

static
liread(ip, io)
	register struct inode	*ip;	/* ptr to inode to fill in */
	register struct iob 	*io;
{
	register struct dinode	*dp;	/* ptr to disk version of inode */
	register int		i;	/* loop control */
	register int		fstype;
	register char		*p1,	/* pointers for unpacking address */
				*p2;	/*	fields of inode		  */
	int			b;	/* inode block # */
	char			INODE[BSIZE * 2];

	fstype = cfs(io)->s_type;
	/* Get inode block. */
	b = FsITOD(MAPTODEV(fstype), ip->i_number);
	io->i_ma = (char *)INODE;
	io->i_cc = FsBSIZE(MAPTODEV(fstype));
	io->i_bn = (FsBSIZE(MAPTODEV(fstype))/DEV_BSIZE) * b;
	DEVREAD(io);

	/* Set up ptr to the disk copy of the inode. */
	dp = (struct dinode *)(INODE);
	dp += FsITOO(MAPTODEV(fstype), ip->i_number);

	/* Unpack the inode. */
	ip->i_mode = dp->di_mode;
	ip->i_nlink = dp->di_nlink;
	ip->i_size = dp->di_size;
	p1 = (char *)ip->i_addr;
	p2 = (char *)dp->di_addr;
	for(i = 0; i < NADDR; i++) {
#ifdef MIPSEB
		*p1++ = '\0';
#endif MIPSEB
		*p1++ = *p2++;
		*p1++ = *p2++;
		*p1++ = *p2++;
#ifdef MIPSEL
		*p1++ = '\0';
#endif MIPSEL
	}
}

/*
**	rwb - Read a block.
**
**	This routine reads the block from the inode pointed to by ip that
**	contains file byte offset faddr into the I/O buffer given by maddr.
*/

static
rwb (io, faddr, ip, flag)
	register struct iob *io;
	off_t		faddr;	/* file byte offset */
	struct inode	*ip;	/* ptr to inode */
	register int	flag;
{
	register int	bn,	/* file system block # */
			*addr,	/* physical buffer address */
			i,	/* loop control */
			fstype;	/* type of file system */

	fstype = cfs(io)->s_type;
	if(bn = lbmap(io, faddr >> FsBSHIFT(MAPTODEV(fstype)))) {
		io->i_ma = io->i_buf;
		io->i_cc = FsBSIZE(MAPTODEV(fstype));
		io->i_bn = (FsBSIZE(MAPTODEV(fstype))/DEV_BSIZE) * bn;
		if (flag == F_READ) {
			if (DEVREAD(io) != FsBSIZE(MAPTODEV(fstype)))
				return (0);
		}
		else {
			if (DEVWRITE(io) != FsBSIZE(MAPTODEV(fstype)))
				return (0);
		}
	}
	else {
		printf("block %d not mapped.\n",
		       faddr >> FsBSHIFT(MAPTODEV(fstype)));
		return(0);
	}
	return(1);
}

#ifdef WRITABLEFS

/*
 * Make a new node with uid, gid and mode set to that of the parent.
 * This routine assumes that io.i_ino is the parent to be.
 */

static
maknode (fname, io)
	register char *fname;
	register struct iob *io;
{
	register struct inode pino, cino;
	register int i_number;

	/*
	 * Save the parent inode information which will be used 
	 * when we have allocated a new inode.
	 */
	bcopy(io->i_ino_dir, &pino, sizeof (struct inode));
	if (!ialloc(io))
		return (0);
	
	/*
	 * Save the new inode information and store the
	 * parent's inode into the iob so that we can
	 * update the directory.
	 */
	i_number = cinode(io)->i_number;
	bcopy (io->i_ino_dir, &cino, sizeof (struct inode));
	bcopy (&pino, io->i_ino_dir, sizeof (struct inode));

	if (!wdir(io, i_number, fname))
		return (0);
	bcopy (&cino, io->i_ino_dir, sizeof (struct inode));
	return (1);
}

/*
 * alloc will obtain the next available free disk block from the free list
 * of the specified device.
 * The super block has up to NICFREE remembered free blocks;
 * the last of these is read to obtain NICFREE more . . .
 *
 * no space on dev x/y -- when the free list is exhausted.
 */
static
alloc(io)
	register struct iob *io;
{
	daddr_t bno;
	register struct filsys *fs;
	register struct buf *bp;
	register int fstype;

	fs = cfs(io);
	fstype = fs->s_type;
	io->i_ma = _get_iobbuf();
	do {
		if (fs->s_nfree <= 0)
			goto nospace;
		bno = fs->s_free[--fs->s_nfree];
		if (bno == 0)
			goto nospace;
	} while (badblock(fs, bno, cinode(io)->i_dev));

	io->i_cc = FsBSIZE(MAPTODEV(fstype));
	io->i_bn = bno * (FsBSIZE(MAPTODEV(fstype))/DEV_BSIZE);
	if (fs->s_nfree <= 0) {
		DEVREAD(io);
		fs->s_nfree = ((FBLKP)(io->i_ma))->df_nfree;
		bcopy((caddr_t)((FBLKP)(io->i_ma))->df_free,
		    (caddr_t)fs->s_free, sizeof(fs->s_free));
	}
	if (fs->s_nfree <= 0 || fs->s_nfree > NICFREE) {
		printf("Bad free count %d\n", 
			cinode(io)->i_dev);
		goto nospace;
	}

	/*
	 * Clear the block
	 */
	bzero (io->i_ma, MAXBSIZE);
	DEVWRITE(io);
	_free_iobbuf(io->i_ma);
	if (fs->s_tfree) fs->s_tfree--;
	fs->s_fmod = 1;
	return(bno);

nospace:
	_free_iobbuf(io->i_ma);
	fs->s_nfree = 0;
	fs->s_tfree = 0;
	printf("no space on %d\n", cinode(io)->i_dev);
	return(NULL);
}

/*
 * Check that a block number is in the range between the I list
 * and the size of the device.
 * This is used mainly to check that a
 * garbage file system has not been mounted.
 *
 * bad block on dev x/y -- not in range
 */
static
badblock(fs, bn, dev)
	register struct filsys *fs;
	daddr_t bn;
	dev_t dev;
{

	if (bn < fs->s_isize || bn >= fs->s_fsize) {
		printf("bad block %d on %d\n", dev);
		return(1);
	}
	return(0);
}

/*
 * Allocate an unused I node on the specified device.
 * Used with file creation.
 * The algorithm keeps up to NICINOD spare I nodes in the
 * super block. When this runs out, a linear search through the
 * I list is instituted to pick up NICINOD more.
 */
static 
ialloc (io)
	register struct iob *io;
{
	register struct filsys *fs;
	register struct inode *ip;
	register i, uid, gid, fstype;
	register struct buf *bp;
	struct dinode *dp;
	ino_t ino;
	daddr_t adr;
	int time;

	ip = cinode(io);
	uid = ip->i_uid;
	gid = ip->i_gid;
	fs = cfs(io);
	fstype = fs->s_type;

loop:
	if (fs->s_ninode > 0
	    && (ino = fs->s_inode[--fs->s_ninode])) {
		ip = cinode(io);
		ip->i_number = ino;
		liread(ip, io);
		if (ip == NULL)
			return(0);
		if (ip->i_mode == 0) {
			/* found inode: update now to avoid races */
			ip->i_mode = IFREG | 0664;
			ip->i_nlink = 1;
			ip->i_uid = uid;
			ip->i_gid = gid;
			ip->i_size = 0;
			for (i=0; i<NADDR; i++)
				ip->i_addr[i] = 0;
			if (fs->s_tinode) fs->s_tinode--;
			fs->s_fmod = 1;
			time = get_tod();
			ip->i_flag = IACC | IUPD | ICHG;
			if (!iupdat(io, &time, &time))
				return (NULL);
			else
				return(ino);
		}
	}
	fs->s_ninode = NICINOD;
	ino = FsINOS(MAPTODEV(fstype), fs->s_inode[0]);
	for(adr = FsITOD(MAPTODEV(fstype), ino); adr < fs->s_isize; adr++) {
		io->i_ma = _get_iobbuf();
		io->i_cc = FsBSIZE(MAPTODEV(fstype));
		io->i_bn = (FsBSIZE(MAPTODEV(fstype))/DEV_BSIZE) * adr;
		if (DEVREAD(io) != FsBSIZE(MAPTODEV(fstype))) {
			_free_iobbuf(io->i_ma);
			ino += FsINOPB(MAPTODEV(fstype));
			continue;
		}
		dp = (struct dinode *)io->i_ma;
		for(i=0; i<FsINOPB(MAPTODEV(fstype)); i++,ino++,dp++) {
			if (fs->s_ninode <= 0)
				break;
			if (dp->di_mode == 0)
				fs->s_inode[--fs->s_ninode] = ino;
		}
		_free_iobbuf(io->i_ma);
		if (fs->s_ninode <= 0)
			break;
	}
	if (fs->s_ninode > 0) {
		fs->s_inode[fs->s_ninode-1] = 0;
		fs->s_inode[0] = 0;
	}
	if (fs->s_ninode != NICINOD) {
		fs->s_ninode = NICINOD;
		goto loop;
	}
	printf("Out of inodes on %d\n", 
		cinode(io)->i_dev);
	return(NULL);
}

static
iupdat(io, ta, tm)
	register struct iob 	*io;
	int *ta, *tm;
{
	register struct inode	*ip;
	register struct dinode	*dp;	/* ptr to disk version of inode */
	register int		i;	/* loop control */
	register int		fstype;
	register char		*p1,	/* pointers for unpacking address */
				*p2;	/*	fields of inode		  */
	int			b,	/* inode block # */
				cur_time;
	char			INODE[BSIZE * 2];

	cur_time = get_tod();
	fstype = cfs(io)->s_type;
	ip = cinode(io);
	/* Get inode block. */
	b = FsITOD(MAPTODEV(fstype), ip->i_number);
	io->i_ma = (char *)INODE;
	io->i_cc = FsBSIZE(MAPTODEV(fstype));
	io->i_bn = (FsBSIZE(MAPTODEV(fstype))/DEV_BSIZE) * b;
	DEVREAD(io);

	/* Set up ptr to the disk copy of the inode. */
	dp = (struct dinode *)(INODE);
	dp += FsITOO(MAPTODEV(fstype), ip->i_number);

	/* Pack the inode. */
	dp->di_mode = ip->i_mode;
	dp->di_nlink = ip->i_nlink;
	dp->di_size = ip->i_size;
	p1 = (char *)dp->di_addr;
	p2 = (char *)ip->i_addr;
	for(i = 0; i < NADDR; i++) {
#ifdef MIPSEB
		*p2++ = '\0';
#endif MIPSEB
		*p1++ = *p2++;
		*p1++ = *p2++;
		*p1++ = *p2++;
#ifdef MIPSEL
		*p2++ = '\0';
#endif MIPSEL
	}

	if(ip->i_flag&IACC)
		dp->di_atime = *ta;
	if(ip->i_flag&IUPD)
		dp->di_mtime = *tm;
	if(ip->i_flag&ICHG)
		dp->di_ctime = cur_time;

	ip->i_flag &= ~(IACC|IUPD|ICHG);
	if (DEVWRITE(io) != FsBSIZE(MAPTODEV(fstype))) {
		printf ("inode write error!!!!\n");
		return (NULL);
	}
	else
		return (1);
}

wdir (io, ino, ent)
	register struct iob *io;
	register int ino;
	char *ent;
{
	register struct inode *dip;
	register struct direct *dep;
	register int off, found, fstype, n, i;
	int time;

	dip = cinode(io);
	fstype = cfs(io)->s_type;

	for(off = found = 0; !found; off += FsBSIZE(MAPTODEV(fstype))) {

		/*
		 * Read next directory block.
		 * (allocating blocks as necessary)
		 */
		if (!rwb(io, off, dip, F_READ)) {
			return(0);
		}

		/* Search it. */
		dep = (struct direct *)io->i_buf;
		for(n = FsBSIZE(MAPTODEV(fstype)) / sizeof(struct direct);
		   !found && n--; ) {
			if(dep->d_ino != 0) {
				dep++;
				continue;
			}
			for(i = 0; i < DIRSIZ; i++) {
				dep->d_name[i] = ent[i];
				if(ent[i] == '\0')
					break;
			}
			found = 1;
			dep->d_ino = ino;
			/*
			 * Only update size of directory if entry was not
			 * placed into a hole.
			 */
			if ((off + ((int)dep - (int)io->i_buf)) >= dip->i_size)
				dip->i_size += sizeof(struct direct);
			dip->i_flag = (IACC|IUPD);
			time = get_tod();
			iupdat(io, &time, &time);
		}
	}

	if (!found)
		return (0);
	else {
		/*
		 * Need to fudge the offset because it's been bumped
		 * just before we dropped out of the loop.
		 */
		rwb (io, off - FsBSIZE(MAPTODEV(fstype)), dip, F_WRITE);
		return (1);
	}
}
#endif WRITABLEFS
#endif !PROM
#endif SYSV
