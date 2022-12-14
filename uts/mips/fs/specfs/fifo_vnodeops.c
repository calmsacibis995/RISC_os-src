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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: fifo_vnodeops.c,v 1.4.1.6.1.2 90/08/13 17:16:23 hawkes Exp $"
/*
 * System V-compatible FIFO implementation.
 */

/* Original includes:
 * param.h systm.h time.h proc.h user.h uio.h vfs.h vnode.h file.h errno.h
 * signal.h ../specfs/fifo.h ../specfs/snode.h ../specfs/fifonode.h
 * ../krpc/lockmgr.h
 */
#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/systm.h"
#include "sys/uio.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/errno.h"
#include "sys/file.h"
#include "sys/signal.h"
#include "sys/numips.h"
#include "sys/fs/ufs_inode.h"
#include "sys/kmem.h"

#include "sys/fs/fifo.h"
#include "sys/fs/snode.h"
#include "sys/fs/fifonode.h"
#include "../klm/lockmgr.h"

#if RISCOS
#include "sys/stream.h"
#define selwait pollwait		/* combine streams & select wait */
#endif

#define PIPE_BUF FIFO_BUF
#define PIPE_MAX FIFO_MAX
#define PIPE_BSZ FIFO_BSZ
#define PIPE_MNB FIFO_MNB

#define	MIN(a,b) (((a)<(b))?(a):(b))
#define SANITY		/* do sanity checks */

static struct fifo_bufhdr *fifo_bufalloc();
static struct fifo_bufhdr *fifo_buffree();

static	int fifo_open();
static	int fifo_close();
static	int fifo_rdwr();
#if RISCOS
/* BUG XXX ~~~ Fix this!!! */
int fifo_select();
#else
static	int fifo_select();
#endif
static	int fifo_getattr();
static	int fifo_inactive();
static	int fifo_invalop();
static	int fifo_cmp();
static	int fifo_badop();

extern int spec_setattr();
extern int spec_access();
extern int spec_link();
extern int spec_lockctl();
extern int spec_fsync();
extern int spec_fid();
extern int spec_realvp();

struct vnodeops fifo_vnodeops = {
	fifo_open,
	fifo_close,
	fifo_rdwr,
	fifo_badop,	/* ioctl */
	fifo_select,
	fifo_getattr,
	spec_setattr,
	spec_access,
	fifo_invalop,	/* lookup */
	fifo_invalop,	/* create */
	fifo_invalop,	/* remove */
	spec_link,
	fifo_invalop,	/* rename */
	fifo_invalop,	/* mkdir */
	fifo_invalop,	/* rmdir */
	fifo_invalop,	/* readdir */
	fifo_invalop,	/* symlink */
	fifo_invalop,	/* readlink */
	spec_fsync,
	fifo_inactive,
	fifo_invalop,	/* bmap		*/
	fifo_invalop,	/* strategy	*/
	fifo_invalop,	/* bread	*/
	fifo_invalop,	/* brelse	*/
	spec_lockctl,
	spec_fid,
	fifo_invalop,	/* dump */
	fifo_cmp,
	spec_realvp,
};


/*
 * open a fifo -- sleep until there are at least one reader & one writer
 */
/*ARGSUSED*/
static int
fifo_open(vpp, flag, cred)
	struct vnode **vpp;
	int flag;
	struct ucred *cred;
{
	register struct fifonode *fp;

	/*
	 * Setjmp in case open is interrupted.
	 * If it is, close and return error.
	 */
#ifdef RISCOS
	if (setjmp(u.u_qsav)) {
#else
	if (setjmp(&u.u_qsave)) {
#endif
		(void) fifo_close(*vpp, flag & FMASK, 1, cred);
		return (EINTR);
	}
	fp = VTOF(*vpp);

	if (flag & FREAD) {
		if (fp->fn_rcnt++ == 0)
			/* if any writers waiting, wake them up */
			wakeup((caddr_t) &fp->fn_rcnt);
	}

	if (flag & FWRITE) {
		if ((flag & (FNDELAY)) && (fp->fn_rcnt == 0))
			return (ENXIO);
		if (fp->fn_wcnt++ == 0)
			/* if any readers waiting, wake them up */
			wakeup((caddr_t) &fp->fn_wcnt);
	}

	if (flag & FREAD) {
		while (fp->fn_wcnt == 0) {
			/* if no delay, or data in fifo, open is complete */
			if ((flag & (FNDELAY)) || fp->fn_size)
				return (0);
			(void) sleep((caddr_t) &fp->fn_wcnt, PPIPE);
		}
	}

	if (flag & FWRITE) {
		while (fp->fn_rcnt == 0)
			(void) sleep((caddr_t) &fp->fn_rcnt, PPIPE);
	}
	return (0);
}

/*
 * close a fifo
 * On final close, all buffered data goes away
 */
/*ARGSUSED*/
static int
fifo_close(vp, flag, count, cred)
	struct vnode *vp;
	int flag;
	int count;
	struct ucred *cred;
{
	register struct fifonode *fp;
	register struct fifo_bufhdr *bp;

	if (count > 1)
		return (0);

	fp = VTOF(vp);

	if (flag & FREAD) {
		if (--fp->fn_rcnt == 0) {
			if (fp->fn_flag & FIFO_WBLK) {
				fp->fn_flag &= ~FIFO_WBLK;
				wakeup((caddr_t) &fp->fn_wcnt);
			}
			/* wake up any sleeping exception select()s */
			if (fp->fn_xsel) {
				curpri = PPIPE;
				selwakeup(fp->fn_xsel, fp->fn_flag&FIFO_XCOLL);
				fp->fn_flag &= ~FIFO_XCOLL;
				fp->fn_xsel = (struct proc *)0;
			}
		}
	}

	if (flag & FWRITE) {
		if ((--fp->fn_wcnt == 0) && (fp->fn_flag & FIFO_RBLK)) {
			fp->fn_flag &= ~FIFO_RBLK;
			wakeup((caddr_t) &fp->fn_rcnt);
		}
	}

	if ((fp->fn_rcnt == 0) && (fp->fn_wcnt == 0)) {
		/* free all buffers associated with this fifo */
		for (bp = fp->fn_buf; bp != NULL; ) {
			bp = fifo_buffree(bp, fp);
		}

		/* update times only if there were bytes flushed from fifo */
		if (fp->fn_size != 0)
			FIFOMARK(fp, SUPD|SCHG);

		fp->fn_buf = (struct fifo_bufhdr *) NULL;
		fp->fn_rptr = 0;
		fp->fn_wptr = 0;
		fp->fn_size = 0;
	}
	return (0);
}


/*
 * read/write a fifo
 */
/*ARGSUSED*/
static int
fifo_rdwr(vp, uiop, rw, ioflag, cred)
	struct vnode *vp;
	struct uio *uiop;
	enum uio_rw rw;
	int ioflag;
	struct ucred *cred;
{
	register struct fifonode *fp;
	register struct fifo_bufhdr *bp;
	register u_int count;
	register int off;
	register unsigned i;
	register int rval = 0;
	int ocnt = uiop->uio_resid;	/* save original request size */

#ifdef SANITY
	if (uiop->uio_offset != 0)
		printf("fifo_rdwr: non-zero offset: %d\n", uiop->uio_offset);
#endif SANITY

	fp = VTOF(vp);
	FIFOLOCK(fp);

	if (rw == UIO_WRITE) {				/* UIO_WRITE */
		/*
		 * PIPE_BUF: max number of bytes buffered per open pipe
		 * PIPE_MAX: max size of single write to a pipe
		 *
		 * If the count is less than PIPE_BUF, it must occur
		 * atomically.  If it does not currently fit in the
		 * kernel pipe buffer, either: sleep, if no form of no-delay
		 * mode is on; return -1 and EWOULDBLOCK, if 4.2-style
		 * no-delay mode is on (FNDELAY set).
		 *
		 * If the count is greater than PIPE_BUF, it will be
		 * non-atomic (FNDELAY clear).  If FNDELAY 
		 * is set, write as much as will fit into the kernel
		 * pipe buffer and return the number of bytes written.
		 *
		 * If the count is greater than PIPE_MAX, return EINVAL.
		 */
		if ((unsigned)uiop->uio_resid > PIPE_MAX) {
			rval = EINVAL;
			goto rdwrdone;
		}

		while (count = uiop->uio_resid) {
			if (fp->fn_rcnt == 0) {
				/* no readers anymore! */
				psignal(u.u_procp, SIGPIPE);
				rval = EPIPE;
				goto rdwrdone;
			}
			if ((count + fp->fn_size) > PIPE_BUF) {
				if (uiop->uio_fmode & (FNDELAY)) {
					/*
					 * Non-blocking I/O.
					 */
					if (fp->fn_size >= PIPE_BUF) {
					    /*
					     * Write will never be atomic.
					     * At this point, it cannot even be
					     * partial.   However, some portion
					     * of the write may already have
					     * succeeded.  If so, uio_resid
					     * reflects this.
					     */
						if (ocnt == uiop->uio_resid)
							if (SYSV_SYSCALL)
								rval = 0;
							else if (POSIX_SYSCALL)
								rval = EAGAIN;
							else rval = EWOULDBLOCK;
						goto rdwrdone;
					} else 	if (count <= PIPE_BUF) {
						/*
						 * Write will be satisfied
						 * atomically, later.
						 * If 4.2-style non-blocking
						 * I/O, return -1 and
						 * EWOULDBLOCK, otherwise
						 * return 0.
						 */
						if (SYSV_SYSCALL)
							rval = 0;
						else if (POSIX_SYSCALL)
							rval = EAGAIN;
						else rval = EWOULDBLOCK;
						goto rdwrdone;
					}
				} else {
					/*
					 * Blocking I/O.
					 */
					if ((count <= PIPE_BUF) ||
					    (fp->fn_size >= PIPE_BUF)) {
				/*
				 * Sleep until there is room for this request.
				 * On wakeup, go back to the top of the loop.
				 */
						fp->fn_flag |= FIFO_WBLK;
						FIFOUNLOCK(fp);
						if (sleep((caddr_t)&fp->fn_wcnt,
							  PPIPE|PCATCH)) {
							/* guarantee that 
							 * f_offset stays 0 
							 */
							uiop->uio_offset = 0;
							return (EINTR);
						} 
						FIFOLOCK(fp);
						goto wrloop;
					}
				}
				/* at this point, can do a partial write */
				count = PIPE_BUF - fp->fn_size;
			}
			/*
			 * Can write 'count' bytes to pipe now.   Make sure
			 * there is enough space in the allocated buffer list.
			 * If not, try to allocate more.
			 * If allocation does not succeed immediately, go back
			 * to the  top of the loop to make sure everything is
			 * still cool.
			 */

#ifdef SANITY
			if ((fp->fn_wptr - fp->fn_rptr) != fp->fn_size)
			    printf("fifo_write: ptr mismatch...size:%d  wptr:%d  rptr:%d\n",
				fp->fn_size, fp->fn_wptr, fp->fn_rptr);

			if (fp->fn_rptr > PIPE_BSZ)
			    printf("fifo_write: rptr too big...rptr:%d\n",
				fp->fn_rptr);
			if (fp->fn_wptr > (fp->fn_nbuf * PIPE_BSZ))
			    printf("fifo_write: wptr too big...wptr:%d  nbuf:%d\n",
				fp->fn_wptr, fp->fn_nbuf);
#endif SANITY

			while (((fp->fn_nbuf * PIPE_BSZ) - fp->fn_wptr)
			    < count) {
				if ((bp = fifo_bufalloc(fp)) == NULL) {
					goto wrloop;	/* fifonode unlocked */
				}
				/* new buffer...tack it on the of the list */
				bp->fb_next = (struct fifo_bufhdr *) NULL;
				if (fp->fn_buf == (struct fifo_bufhdr *) NULL) {
					fp->fn_buf = bp;
				} else {
					fp->fn_bufend->fb_next = bp;
				}
				fp->fn_bufend = bp;
			}
			/*
			 * There is now enough space to write 'count' bytes.
			 * Find append point and copy new data.
			 */
			bp = fp->fn_buf;
			for (off = fp->fn_wptr; off >= PIPE_BSZ;
			    off -= PIPE_BSZ)
				bp = bp->fb_next;

			while (count) {
				i = PIPE_BSZ - off;
				i = MIN(count, i);
				if (rval =
				    uiomove(&bp->fb_data[off], (int) i,
				    UIO_WRITE, uiop)){
					/* error during copy from user space */
					/* NOTE:LEAVE ALLOCATED BUFS FOR NOW */
					goto rdwrdone;
				}
				fp->fn_size += i;
				fp->fn_wptr += i;
				count -= i;
				off = 0;
				bp = bp->fb_next;
			}
			FIFOMARK(fp, SUPD|SCHG);	/* update mod times */

			/* wake up any sleeping readers */
			if (fp->fn_flag & FIFO_RBLK) {
				fp->fn_flag &= ~FIFO_RBLK;
				curpri = PPIPE;
				wakeup((caddr_t) &fp->fn_rcnt);
			}

			/* wake up any sleeping read selectors */
			if (fp->fn_rsel) {
				curpri = PPIPE;
				selwakeup(fp->fn_rsel, fp->fn_flag&FIFO_RCOLL);
				fp->fn_flag &= ~FIFO_RCOLL;
				fp->fn_rsel = (struct proc *)0;
			}

wrloop: 		/* bottom of write 'while' loop */
			continue;
		}

	} else {					/* UIO_READ */
		/*
		 * Handle zero-length reads specially here
		 */
		if ((count = uiop->uio_resid) == 0) {
			goto rdwrdone;
		}
		while ((i = fp->fn_size) == 0) {
			if (fp->fn_wcnt == 0) {
				/* no data in pipe and no writers...(EOF) */
				goto rdwrdone;
			}
			/*
			 * No data in pipe, but writer is there;
			 * if 4.2-style no-delay, return EWOULDBLOCK,
			 * if S5-style, return 0.
			 */
			if (uiop->uio_fmode & FNDELAY) {
				if (SYSV_SYSCALL)
					rval = 0;
				else if (POSIX_SYSCALL)
					rval = EAGAIN;
				else rval = EWOULDBLOCK;
				goto rdwrdone;
			}
			fp->fn_flag |= FIFO_RBLK;
			FIFOUNLOCK(fp);
			if (sleep((caddr_t) &fp->fn_rcnt, PPIPE|PCATCH)) {
				/* guarantee that f_offset stays 0 */
				uiop->uio_offset = 0;
				return (EINTR);
			}
			FIFOLOCK(fp);
			/* loop to make sure there is still a writer */
		}

#ifdef SANITY
		if ((fp->fn_wptr - fp->fn_rptr) != fp->fn_size)
			printf("fifo_read: ptr mismatch...size:%d  wptr:%d  rptr:%d\n",
			    fp->fn_size, fp->fn_wptr, fp->fn_rptr);

		if (fp->fn_rptr > PIPE_BSZ)
			printf("fifo_read: rptr too big...rptr:%d\n",
			    fp->fn_rptr);

		if (fp->fn_wptr > (fp->fn_nbuf * PIPE_BSZ))
			printf("fifo_read: wptr too big...wptr:%d  nbuf:%d\n",
			    fp->fn_wptr, fp->fn_nbuf);
#endif SANITY

		/*
		 * Get offset into first buffer at which to start getting data.
		 * Truncate read, if necessary, to amount of data available.
		 */
		off = fp->fn_rptr;
		bp = fp->fn_buf;
		count = MIN(count, i);	/* smaller of pipe size and read size */

		while (count) {
			i = PIPE_BSZ - off;
			i = MIN(count, i);
			if (rval =
			    uiomove(&bp->fb_data[off], (int)i, UIO_READ, uiop)){
				goto rdwrdone;
			}
			fp->fn_size -= i;
			fp->fn_rptr += i;
			count -= i;
			off = 0;

#ifdef SANITY
			if (fp->fn_rptr > PIPE_BSZ)
				printf("fifo_read: rptr after uiomove too big...rptr:%d\n",
				    fp->fn_rptr);
#endif SANITY

			if (fp->fn_rptr == PIPE_BSZ) {
				fp->fn_rptr = 0;
				bp = fifo_buffree(bp, fp);
				fp->fn_buf = bp;
				fp->fn_wptr -= PIPE_BSZ;
			}
			/*
			 * At this point, if fp->fn_size is zero, there may be
			 * an allocated, but unused, buffer.  [In this case,
			 * fp->fn_rptr == fp->fn_wptr != 0.]
			 * NOTE: FOR NOW, LEAVE THIS EXTRA BUFFER ALLOCATED.
			 * NOTE: fifo_buffree() CAN'T HANDLE A BUFFER NOT 1ST.
			 */
		}

		FIFOMARK(fp, SACC);	/* update the access times */

		/* wake up any sleeping writers */
		if (fp->fn_flag & FIFO_WBLK) {
			fp->fn_flag &= ~FIFO_WBLK;
			curpri = PPIPE;
			wakeup((caddr_t) &fp->fn_wcnt);
		}

		/* wake up any sleeping write selectors */
		if (fp->fn_wsel) {
			curpri = PPIPE;
			selwakeup(fp->fn_wsel, fp->fn_flag&FIFO_WCOLL);
			fp->fn_flag &= ~FIFO_WCOLL;
			fp->fn_wsel = (struct proc *)0;
		}
	}		/* end of UIO_READ code */

rdwrdone:
	FIFOUNLOCK(fp);
	uiop->uio_offset = 0;		/* guarantee that f_offset stays 0 */
	return (rval);
}

static int
fifo_getattr(vp, vap, cred)
	struct vnode *vp;
	struct vattr *vap;
	struct ucred *cred;
{
	register int error;
	register struct snode *sp;

	sp = VTOS(vp);
#if RISCOS
	if (sp->s_realvp) { 
		if (error = VOP_GETATTR(sp->s_realvp, vap, cred))
			return (error);
	} else /* no file backing "spontaneous" pipe...so set mode here */
		vap->va_mode = IFIFO|IREAD|IWRITE;

	vap->va_fmode = -1; 		/* set to uninitialized state */
	vap->va_fsid = -1;

	/* set times from snode, even if older than vnode */
	vap->va_atime = sp->s_atime;
	vap->va_mtime = sp->s_mtime;
	vap->va_ctime = sp->s_ctime;

	/* size should reflect the number of unread bytes in pipe */
	vap->va_size = (VTOF(vp))->fn_size;
	vap->va_blocksize = PIPE_BUF;
	return (0);
#else /* !RISCOS */
	error = VOP_GETATTR(sp->s_realvp, vap, cred);
	if (!error) {
		/* set current times from snode, even if older than vnode */
		vap->va_atime = sp->s_atime;
		vap->va_mtime = sp->s_mtime;
		vap->va_ctime = sp->s_ctime;

		/* size should reflect the number of unread bytes in pipe */
		vap->va_size = (VTOF(vp))->fn_size;
		vap->va_blocksize = PIPE_BUF;
	}
	return (error);
#endif /* RISCOS */
}

/*
 * test for fifo selections
 */
/*ARGSUSED*/
#ifndef RISCOS
/* BUG XXX ??? ~~~ This should be static after selscan is fixed (see bof) !!! */
static
#endif
int
fifo_select(vp, flag, cred)
	struct vnode *vp;
	int flag;
	struct ucred *cred;
{
	register struct fifonode *fp;

	fp = VTOF(vp);

	switch (flag) {
	case FREAD:
		if (fp->fn_size != 0)		/* anything to read? */
			return (1);
		if (fp->fn_rsel && fp->fn_rsel->p_wchan == (caddr_t)&selwait)
			fp->fn_flag |= FIFO_RCOLL;
		else
			fp->fn_rsel = u.u_procp;
		break;

	case FWRITE:
		/* is there room to write? (and are there any readers?) */
		if ((fp->fn_size < PIPE_BUF) && (fp->fn_rcnt > 0))
			return (1);
		if (fp->fn_wsel && fp->fn_wsel->p_wchan == (caddr_t)&selwait)
			fp->fn_flag |= FIFO_WCOLL;
		else
			fp->fn_wsel = u.u_procp;
		break;

	case 0:
		if (fp->fn_rcnt == 0)		/* no readers anymore? */
			return (1);		/* exceptional condition */
		if (fp->fn_xsel && fp->fn_xsel->p_wchan == (caddr_t)&selwait)
			fp->fn_flag |= FIFO_XCOLL;
		else
			fp->fn_xsel = u.u_procp;
		break;
	}
	return (0);
}

static int
fifo_inactive(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	register struct snode *sp;

	sp = VTOS(vp);
	/* must sunsave() first to prevent a race when spec_fsync() sleeps */
	sunsave(sp);
	(void) spec_fsync(vp, cred);

	/* now free the realvp (no longer done by sunsave()) */
	if (sp->s_realvp) {
		VN_RELE(sp->s_realvp);
		sp->s_realvp = NULL;
	}
	kmem_free((caddr_t)VTOF(vp), (u_int)sizeof (struct fifonode));
	return (0);
}

static int
fifo_cmp(vp1, vp2)
struct vnode *vp1, *vp2;
{
	return (vp1 == vp2);
}

static  int
fifo_invalop()
{
	return (EINVAL);
}

static int
fifo_badop()
{

	panic("fifo_badop");
}

/*
 * allocate a buffer for a fifo
 * return NULL if had to sleep
 */
static struct fifo_bufhdr *
fifo_bufalloc(fp)
	register struct fifonode *fp;
{
	register struct fifo_bufhdr *bp;

	if (fifo_alloc >= PIPE_MNB) {
		/*
		 * Impose a system-wide maximum on buffered data in pipes.
		 * NOTE: This could lead to deadlock!
		 */
		FIFOUNLOCK(fp);
		(void) sleep((caddr_t) &fifo_alloc, PPIPE);
		FIFOLOCK(fp);
		return ((struct fifo_bufhdr *)NULL);
	}

	/* the call to kmem_alloc() might sleep, so leave fifonode locked */

	fifo_alloc += FIFO_BUFFER_SIZE;
	bp = (struct fifo_bufhdr *)kmem_alloc((u_int)FIFO_BUFFER_SIZE);
	fp->fn_nbuf++;
	return ((struct fifo_bufhdr *) bp);
}

/*
 * deallocate a fifo buffer
 */
static struct fifo_bufhdr *
fifo_buffree(bp, fp)
	struct fifo_bufhdr *bp;
	struct fifonode *fp;
{
	register struct fifo_bufhdr *nbp;

	fp->fn_nbuf--;

	/*
	 * NOTE: THE FOLLOWING ONLY WORKS IF THE FREED BUFFER WAS THE 1ST ONE.
	 */
	if (fp->fn_bufend == bp) {
		fp->fn_bufend = (struct fifo_bufhdr *) NULL;
		nbp = (struct fifo_bufhdr *) NULL;
	} else
		nbp = bp->fb_next;

	kmem_free((caddr_t)bp, (u_int)FIFO_BUFFER_SIZE);

	if (fifo_alloc >= PIPE_MNB) {
		curpri = PPIPE;
		wakeup((caddr_t) &fifo_alloc);
	}
	fifo_alloc -= FIFO_BUFFER_SIZE;

	return (nbp);
}

/*
 * construct a fifonode that can masquerade as an snode
 */
struct snode *
fifosp(vp)
	struct vnode *vp;
{
	register struct fifonode *fp;
	struct vattr va;

	fp = (struct fifonode *)kmem_zalloc(sizeof (*fp));
	FTOV(fp)->v_op = &fifo_vnodeops;

	/* init the times in the snode to those in the vnode */
	(void) VOP_GETATTR(vp, &va, u.u_cred);
	FTOS(fp)->s_atime = va.va_atime;
	FTOS(fp)->s_mtime = va.va_mtime;
	FTOS(fp)->s_ctime = va.va_ctime;
	return (FTOS(fp));
}

#if RISCOS
/*
 * In order to meet the SVID most easily, in RISCOS we
 * use this code for "spontaneous" pipes created through
 * the "pipe()" system call (which is actually implemented
 * in the "sysv_pipe()" routine in os/pipe.c).  In this case,
 * there is no vnode to shadow, but otherwise, this is similar
 * to calling makespecvp() and/or fifosp().
 */
struct vnode *
makefifovp()
{
	register struct fifonode *fp;
	register struct snode *sp;
	register struct vnode *vp;

	fp = (struct fifonode *)kmem_zalloc(sizeof (*fp));

	/* Init vp fields; all other fields are 0 */
	vp = FTOV(fp);
	vp->v_count = 1;
	vp->v_op = &fifo_vnodeops;
	vp->v_type = VFIFO;
	vp->v_data = (caddr_t)fp;

	/* Init sp fields; all other fields are 0 */
	sp = FTOS(fp);
	sp->s_atime = sp->s_mtime = sp->s_ctime = time;
	sp->s_count = 1;

	/*
	 * We could ssave(sp), but what's the point?  Noone
	 * else can get a handle on a spontaneous pipe.
	 */

	return(vp);
}
#endif /* RISCOS */
