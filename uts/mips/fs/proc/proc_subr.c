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
#ident	"$Header: proc_subr.c,v 1.3.1.6.1.3 90/07/17 12:57:45 hawkes Exp $"



#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/uio.h"
#include "sys/dnlc.h"
#include "sys/errno.h"
#include "sys/file.h"
#include "sys/vfs.h"
#include "sys/vfs_stat.h"
#include "sys/vnode.h"
#include "sys/pathname.h"
#include "sys/conf.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/debug.h"
#include "sys/fs/proc_fs.h"
#include "sys/kmem.h"

#include "sys/var.h"
#include "sys/reg.h"
#include "bsd43/sys/dirent.h"


/*
* /proc filesystem support subroutines.
*
*/

extern struct vnodeops proc_vnodeops[];

extern struct proc_state proc_state;	/* some state information	*/
extern pinode_t rootpinode;		/* /proc root pinode (static)	*/

static pinode_t	*pi_active=0;	/* active pinode list rooted here	*/

extern struct proc *procfind();

/* 
 * Define a directory structure for /proc "on disk" directory.
 * This is just like the old SV directory format.
 */

#define	PROC_DIRENT	14
struct	proc_direct
{
	ushort	d_ino;
	char	d_name[PROC_DIRENT];
};

#define	TRUE		1
#define	FALSE		0


/* return TRUE if there are no active process pinodes, else FALSE
* This is used to decide if we can unmount the filesystem.
*/

pi_empty ()
{
	return(pi_active == (pinode_t *)NULL);
}

/* look for an existing pinode in the /proc vnodes.
* The search field is the process ID number of the process
* plus some offset value so that the root inode can have a good value.
* Return a pointer to the pinode if found else NULL.
*/

pinode_t *
pi_search(id)
int	id;	/* The lookup id field -- the process id */
{
	pinode_t *pip;

	/* a singly-linked list scan */
	if (id == PROCROOT) {
		return(&rootpinode);
	}
	for (pip=pi_active; pip != (pinode_t *)NULL; pip = pip->pi_next) {
		if (pip->pi_id == id) {
			break;
		}
	}
	return(pip);
} 

/* put pinode into active list
* This is currently a linear list.
*/

piinsert(pip)
pinode_t *pip;		/* pinode to add to active list */
{
	pip->pi_next = pi_active;
	pi_active = pip;
}

/*
* remove a pinode from the active list.
* Return FALSE if the inode was not in the active list.
*/

/* Boolean */
piremove(rpip)
pinode_t *rpip;		/* pinode to remove from active list */
{
	pinode_t *pip;	/* walk the list			*/

	/* It might be the first item on the list */
	if ((pip=pi_active) == rpip) {
		pi_active = pi_active->pi_next;
		rpip->pi_next = (pinode_t *)NULL;
		goto remout;
	}
	/* if not, look through the list */
	for (pip=pi_active; pip != (pinode_t *)NULL; pip = pip->pi_next) {
		/* a singly-linked list scan */
		if (pip->pi_next == rpip) {
			break;
		}
	}
	if (pip == (pinode_t *)NULL) {
		cmn_err(CE_CONT,"!piremove: no pinode 0x%x (proc %d)\n",
		    rpip,rpip->pi_id);
		return(FALSE);
	}
	pip->pi_next = pip->pi_next->pi_next;
	rpip->pi_next = (pinode_t *)NULL;
remout:
	kmem_free(rpip, sizeof(*rpip));
	return(TRUE);
}


/* get a pinode given a process number
* This stores a pointer to a held vnode (in pinode) for the
* desired process if the process exists.
* If there is an existing pinode for the process, it is held
* and returned.  If not, a pinode is created, held, and returned.
*/
/* Create a pinode for the /proc "root directory" or for
* a previosly "unopened" process.
* The pinode is initialized and the process state is set
* to reflect the fact that the process is "open" via /proc.
*  id - from passed parameter
*  type - set to DIRectory if root dir, else REGular.
*  permissions - 555 if root dir,
*                000 if text region vnode is not readable,
*		  600 if text region vnode is readable.
*  vnode is held by VN_INIT (i.e. count is set to 1).
*  operation vector pointer set for /proc.
*  vnode fs-specific pointer set to point at pinode address
*
* The exclusive-open protocol for processes is not enforced here
* becauase vnodes, hence pinodes, are created to stat or modify
* the vnodes (i.e. ls -l).
* The ex-open is enforced via proc_open() and flag cleared when
* the vnode becomes inactive.
*
*/

piget(proc_no,vpp)
int		proc_no;	/* process number for which pinode is desired */
struct vnode	**vpp;		/* place to return a VNODE pointer to vnode */
{
	struct proc *pp;	/* proc for proc_no			*/
	struct vnode *vp;	/* the vnode in the pinode		*/
	struct vnode *tvp;	/* the vnode for process text		*/
	int	error;		/* return result			*/
	int	mode;		/* construct valid mode bits		*/
	preg_t	*prp;		/* region ptr for proc text region	*/
	pinode_t *pip;		/* pointer to relevant pinode		*/

	error = 0;

	*vpp = (struct vnode *)NULL;	/* paranoia */

	/* see if the process number is valid */
	if ((pp = procfind(proc_no))  ==  (struct proc *)NULL) {
		return(ENOENT);
	}

	/* see if this vnode already exists.
	* If so, return it held.
	*/
	
	if ((pip = pi_search(proc_no)) != NULL) {
		vp = PTOV(pip);
		VN_HOLD(vp);
		*vpp = vp;
		return(0);
	}
	
	/* Allocate a pinode, initialize it, hold it,
	* and hand it back to the caller.
	*/

	pip = (pinode_t *)kmem_zalloc(sizeof(*pip));
	if (pip == (pinode_t *)0) {
		return(ENOMEM);
	}
	pip->pi_id = proc_no;
	vp = PTOV(pip);
	VN_INIT(vp,proc_state.proc_vfsp,VREG,0);
	/* XXX put the following init line into the VN_INIT macro */
	vp->v_vfsmountedhere = (struct vfs *)0;

	/* discover what permissions we should give initially
	*/
	if (((prp = findpreg(pp, PT_TEXT)) && (tvp = prp->p_reg->r_vptr)) ||
	    ((prp = findpreg(pp, PT_DATA)) && (tvp = prp->p_reg->r_vptr))) {
		if ( VOP_ACCESS(tvp, VREAD, u.u_cred) == 0) {
			pip->pi_mode = VREAD|VWRITE;
		}
		/*
	 	* We called VOP_ACCESS only to determine the mode bits;
	 	* No error should be reflected back to the user.
	 	*/
		u.u_error = 0;
	}
	vp->v_op = proc_vnodeops;
	vp->v_data = (caddr_t)pip;
	piinsert(pip);
	*vpp = vp;
	return(0);
}


#define	ADDR(x)	((off_t)&(x))
#define	PCB(x)	(u.u_pcb.x)

/* the writeable portions of the user area */
struct uwrtmap {
	off_t	start; 	/* first writeable word */
	off_t	fin;	/* first non-writeable word */
} uwrtmap[] = {
	{ ADDR(USER_REG(EF_ARGSAVE0)),	ADDR(USER_REG(EF_RA))+sizeof(int) },
	{ ADDR(USER_REG(EF_MDLO)),	ADDR(USER_REG(EF_MDHI))+sizeof(int) },
	{ ADDR(USER_REG(EF_EPC)),	ADDR(USER_REG(EF_EPC))+sizeof(int) },
	{ ADDR(PCB(pcb_fpregs[0])),	ADDR(PCB(pcb_fpc_csr))+sizeof(int) },
	0
};



#ifndef DIRBLKSIZ
#define DIRBLKSIZ	512	/* dir blk size if none defined */
#endif

#define TEXT		0
#define DATA		1
#define STACK		2
#define USERAREA	3
#define	SHAREDMEM	4
#define OTHER		5

extern user_t	*pruvad;		/* Used to map in other uareas. */

extern struct proc *procfind();
extern struct timeval time;
extern struct vnode proc_rootvnode;
extern struct var v;
extern int	piget();
extern pinode_t *picreate();
extern preg_t	*vtopreg();
extern preg_t		*xdup();

#define OWNER(CR, uid)	(((CR)->cr_uid == uid)? 0: (suser()? 0: u.u_error))

/* get uid for pinode
*
* The root directory is statically owned by root.
* The individual processes are owned by the euid.
* Return an error number if there is no appropriate process.
*/
getowner(pip,puid,pgid)
pinode_t *pip;
ushort	*puid;
ushort	*pgid;
{
	struct proc *pp;
	int	error = 0;

	if (pip->pi_id == PROCROOT) {
		*puid = 0;
		*pgid = 0;
	} else if ((pp = procfind(pip->pi_id)) != (struct proc *)0) {
		*puid = pp->p_suid;
		*pgid = pp->p_sgid;
	} else {
		error = ENOENT;
	}
	return(error);
}


/*
* Check mode permission on pinode.
* Mode is READ, WRITE or EXEC.
* In the case of WRITE, the
* read-only status of the file
* system is checked.
* Also in WRITE, prototype text
* segments cannot be written.
* The mode is shifted to select
* the owner/group/other fields.
* The super user is granted all
* permissions.
*/

piaccess(pip, mode)
	pinode_t *pip;		/* pinode being checked			*/
	int	mode;		/* mode to check for			*/
{
	struct vnode *vp;	/* point at vnode assoc with pinode	*/
	gid_t	*gp;		/* walk through group list		*/
	struct proc *proc;	/* proc this pinode is for		*/
	ushort	pi_uid;
	ushort	pi_gid;
	int	error;

	if (mode & VWRITE) {

		vp = PTOV(pip);
		/*
		 * Disallow write attempts on read-only file systems.
		 */
		if ((vp->v_vfsp->vfs_flag & VFS_RDONLY) != 0) {
			return (EROFS);
		}

#ifdef NOTNOW
		/*
		 * If there's shared text associated with
		 * the inode, try to free it up once.  If
		 * we fail, we can't allow writing.
		 */
		if (vp->v_flag & VTEXT) {
			xrele(vp);
		}
		if (vp->v_flag & VTEXT) {
			return (ETXTBSY);
		}
#endif /* NOTNOW */
	}

	/*
	 * If you're the super-user,
	 * you always get access.
	 */

	if (u.u_uid == 0) {
		return (0);
	}
	/*
	 * Access check is based on only
	 * one of owner, group, public.
	 * If not owner, then check group.
	 * If not a member of the group, then
	 * check public access.
	 */
	error = getowner(pip,&pi_uid,&pi_gid);
	if (error) {
		return(error);
	}
	if (u.u_uid != pi_uid) {
		mode >>= 3;
		if (u.u_gid == pi_gid) {
			goto found;
		}
		gp = u.u_groups;
		for (; gp < &u.u_groups[NGROUPS] && *gp != NOGROUP; gp++)
			if (pi_gid == *gp) {
				goto found;
			}
		mode >>= 3;
	}
found:
	if ((pip->pi_mode & mode) == mode) {
		return (0);
	}
	return (EACCES);
}

proc_readroot(vp, uiop, rw, ioflag, cred)
	struct vnode *vp;
	struct uio *uiop;
	enum uio_rw rw;
	int ioflag;
	struct ucred *cred;
{
	int	error = 0;
	pinode_t *pip;
	static struct proc_direct dotbuf[] = {
		{ PROCROOT, "."},
		{ PROCROOT, ".."}
	};
	struct proc_direct dirbuf;
	int i, n, j;
	int minproc, maxproc, modoff;

	pip = VTOP(vp);
readstart:
	/* fake up . .. and the proc inodes */
	if (uiop->uio_offset < 0
	   || uiop->uio_offset >= (v.v_proc + 2 ) * sizeof(struct proc_direct)
	   || uiop->uio_resid <= 0) {
		return(EIO);
	}
	if (uiop->uio_offset < 2*PROCDSIZ) {
		if (error = uiomove(
		    (caddr_t)dotbuf + uiop->uio_offset,
		     min(uiop->uio_resid, 2*PROCDSIZ - uiop->uio_offset),
		     rw,
		     uiop)) {
			return(error);
		}
		
		if (uiop->uio_resid <= 0) {
			return(0);
		}
	}
	minproc = (uiop->uio_offset - 2*PROCDSIZ)/PROCDSIZ;
	maxproc = (uiop->uio_offset + uiop->uio_resid - 1)/PROCDSIZ;
	modoff = uiop->uio_offset % PROCDSIZ;
	for (j = 0; j < sizeof(dirbuf.d_name); j++)
		dirbuf.d_name[j] = 0;
	for (i = minproc; i < min(maxproc, v.v_proc); i++) {
		if (proc[i].p_stat) {	/*no need to prohibit proc0 */
			n = proc[i].p_pid;
			dirbuf.d_ino = n;
			for (j = 4; j >= 0; j--) {
				register int rem;
				rem = n % 10;
				n /= 10;
				dirbuf.d_name[j] = rem + '0';
			}
		} else {
			dirbuf.d_ino = 0;
			for (j = 0; j <= 4; j++)
				dirbuf.d_name[j] = '\0';
		}
		if (error = uiomove((caddr_t)&dirbuf + modoff,
		  min(uiop->uio_resid, PROCDSIZ - modoff),
		  rw, uiop)) {
			return(error);
		}
		if (uiop->uio_resid <= 0) {
			return(0);
		}
		modoff = 0;
	}
}

/* Map a procs user area for access.
* find the user area for the specified process,
* map it into kernel memory, and put the address of the
* area in a local static variable.
*/

proc_umap(pp)
struct proc *pp;
{
	if (winublocked()) {
		return(1);
	}
	winublock();
	pruvad = (user_t *)win_ublk;
	/* Should RG_DONE be set on the new region? */
	uaccess(pp, pruvad);
	return(0);
}

/* unmap the extra mapped user area.
*
*/

proc_unmap(pp)
struct proc *pp;
{
	udeaccess();
	pruvad = (user_t *)0;
	winubunlock();
}

/* read process space
* Read the virtual address space of the process.
* The uarea is available at the kernel address used to map a uarea
* (editorial:  this is ugly because it doesn't hide implementation at all).
*/

proc_readproc(vp,uiop,rw,ioflag,cred)
struct vnode	*vp;
struct uio	*uiop;
enum uio_rw	rw;
int		ioflag;
struct ucred	*cred;
{
	register int i, n, j;
	struct proc *pp;
	int	error=0;
	pinode_t *pip;

	pip = VTOP(vp);
readstart:
	if ((pp = procfind(pip->pi_id)) != (struct proc *)0) {
		if (pp->p_stat == SZOMB) {
			/* XXX is EIO a reasonable error here ? */
			error = EIO;
		} else {
			if (error = prlock(pp)) {
				return(error);
			}
			i = prusrio(pp, uiop, rw);
			prunlock(pp);
			if (i == -1) {
				goto readstart;
			}
		}
	} else {
		error = ENOENT;
	}

	return (error);
}

/* write process space
* Wrote the virtual address space of the process.
* The uarea is available at the kernel address used to map a uarea
* (editorial:  this is ugly because it doesn't hide implementation at all).
*/

proc_writeproc(vp,uiop,rw,ioflag,cred)
struct vnode	*vp;
struct uio	*uiop;
enum uio_rw	rw;
int		ioflag;
struct ucred	*cred;
{
	register int i, n, j;
	struct proc *pp;
	int	error=0;
	pinode_t *pip;

	pip = VTOP(vp);
writestart:
	if ((pp = procfind(pip->pi_id)) != (struct proc *)0) {
		if (pp->p_stat == SZOMB) {
			/* XXX is EIO a reasonable error here ? */
			error = EIO;
		} else {
			if (error = prlock(pp)) {
				return(error);
			}
			i = prusrio(pp, uiop, rw);
			prunlock(pp);
			if (i == -1) {
				goto writestart;
			}
		}
	} else {
		error = ENOENT;
	}

	return (error);
}

static char priobuf[1024];

/*
* Perform I/O to/from a process image.
* 
* I/O can be performed to the memory of the target process space
* as well as the per-process user area.
* The process is accessed via target process virtual addresses and
* the u-area is accessed at the kernel virtual address to which the
* u-area is always mapped in RISC/os.
* A single I/O request can access data in only a single region
* (or the u-area) even if regions are contiguous.
* The region is determined by the offset at the time of the I/O call.
*
* If the address is in one of the target process segment
*
* If the I/O could not be safely attempted
* because a needed region was locked, this routine returns -1 to indicate
* to the caller that a retry is necessary.
* If the I/O was successful, a 0 is returned.
* If the I/O had some error, a non-negative error number is returned.
*/
int
prusrio(pp, uiop, rw)
register struct proc	*pp;
struct uio		*uiop;
enum uio_rw		rw;
{
	register int *regbase;
	register preg_t *hisprp;
	preg_t *myprp;
	reg_t *hisrp;
	caddr_t maxadr;
	int segment, resid;
	int i;
	int	error=0;


	/* Locate I/O region from virtual address.
	 * This is either a region of the target proc or the user-area
	 */

	if ((hisprp = vtopreg(pp, uiop->uio_offset)) != NULL) {
		maxadr = hisprp->p_regva + ctob(hisprp->p_reg->r_pgoff) +
			ctob(hisprp->p_reg->r_pgsz);
		if (hisprp->p_type == PT_TEXT || hisprp->p_type == PT_LIBTXT) {
			segment = TEXT;
		} else {
			segment = OTHER;
		}
	} else if ((u_long)uiop->uio_offset >= (u_long)UADDR
		  && (u_long)uiop->uio_offset <= (u_long)(UADDR + ctob(USIZE)-1)) {
			maxadr = (caddr_t)(UADDR + ctob(USIZE));
			segment = USERAREA;
	} else {
		error = EIO;
	}

	/* Invariants:
	 * maxadr is the highest address that can be accessed in current region.
	 * segment is set to a valid value
	 * error is 0 if no error, nonzero if an error.
	 */

	if (error) {
		goto outahere;
	}

	if (segment == USERAREA) {
		/*
		 * If we're accessing the object process uarea, map it in.
		 * The call to proc_umap can sleep and events can change
		 * under us or we can take a signal.
		 * -1 : try again
		 *  0 : it worked
		 * >0 : error to pass back.
		 */
		if (proc_umap(pp) < 0) {
			return (-1);
		}
	} else {
		hisrp = hisprp->p_reg;
	}

	/* If the request is to write to the process space,
	 * verify that this is permissable and if it is,
	 * make sure that we can do so.
	 * In particular, if writing to a text region it would
	 * be impolite to change the region for all users so
	 * the region has to be duplicated.
	 * Only certain regions in the u-area are writable
	 * to protect the system from being crashed.
	 */
	if (rw == UIO_WRITE) {
		switch (segment) {
		case TEXT:
			/*
			 * Duplicate or steal the text region.  If we've
			 * already done it to this region its type will be
			 * RT_PRIVATE.
			 */
			if (hisrp->r_type == RT_STEXT) {
				if (prreglock(pp, hisrp, 1) == -1) {
						
					error = -1;
					goto outahere;
				}
				if (hisprp = xdup(pp, hisprp)) {
					hisrp = hisprp->p_reg;
					/* XXX I don't think I need this
					 * if (hisrp->r_iptr) {
					 *	prele(hisrp->r_iptr);
					 * }
					 */
					regrele(hisrp);
				}
			}
			break;
		case USERAREA:
			regbase = pruvad->u_ar0;
			/* verify that the address and count fall in a
			 * writeable range
			 */
			for (i=0; uwrtmap[i].start; i++)
				if (uiop->uio_offset >= uwrtmap[i].start &&
					(uiop->uio_offset+u.u_count) <= uwrtmap[i].fin) {
						goto pass;
				}
			error = EIO;	/* not in a writeable range */
		pass:	;
			break;
		}
	} 

	if (error) {
		if (segment == USERAREA) {
			proc_unmap(pp);
		}
		goto outahere;
	}

	if (segment != USERAREA) {
		/*
		 * Map object process region into our address space.
		 */
		if (proc_attach(pp, hisrp, rw, &myprp) == -1) {
			error = -1;
			goto outahere;
		}
		if (myprp == NULL) {
			error = EIO;
			goto outahere;
		}
	}
	resid = uiop->uio_resid;
	if ((u_long)(uiop->uio_offset + uiop->uio_resid) >= (u_long)maxadr) {
		uiop->uio_resid = maxadr - (caddr_t)uiop->uio_offset;
	}
	resid -= uiop->uio_resid;

	if (segment == USERAREA) {
		caddr_t addr = (caddr_t)pruvad +
				((u_long)uiop->uio_offset - (u_long)UADDR);
		uiomove(addr, uiop->uio_resid, rw, uiop);
		if (rw == UIO_WRITE) {
/* XXX should we really futz with these values or let the user beware ? */
			/* Ensure that the necessary regs are word-aligned */
			USER_REG(EF_GP) &= ~(NBPW - 1);
			USER_REG(EF_SP) &= ~(NBPW - 1);
			USER_REG(EF_FP) &= ~(NBPW - 1);
			USER_REG(EF_RA) &= ~(NBPW - 1);
			USER_REG(EF_EPC) &= ~(NBPW - 1);
		}
		proc_unmap(pp);
	} else {
		caddr_t addr = myprp->p_regva
		  + ((u_long)uiop->uio_offset - (u_long)hisprp->p_regva);

		error = uiomove(addr, uiop->uio_resid, rw, uiop);
		proc_detach(myprp);
	}

	uiop->uio_resid += resid;
outahere:
	return(error);
}

/* I/O via /proc returns EBUSY if any of these bits are set in p_flag: */

#define	SPRBUSY	(SLOCK|SPROCIO)

static struct proc *prpidlock, *prpidwant;

/* Lock the process
* The process can't run while locked so
* I/O can be done in the process image safely and
* process state can be changed safely.
*/

prlock(pp)
register struct proc *pp;
{
	int s;

	if (pp != u.u_procp) {
		while (prpidlock == pp) { /* wait if pp has the interlock */
			prpidwant = u.u_procp;
			if (sleep((caddr_t)&prpidlock, PCATCH|(PZERO+1)))
				return(u.u_error = EINTR);
		}
		s = splclock();	/* keep clock out */
		if (pp->p_flag & SPRBUSY || (pp->p_stat != SSLEEP &&
		    pp->p_stat != SRUN && pp->p_stat != SSTOP)) {
			splx(s);
			return(u.u_error = EBUSY);
		}

		/* interlock; process will not run while this flag is on */

		pp->p_flag |= SPROCIO;

		splx(s);
	}

	while (prpidlock) {
		prpidwant = u.u_procp;
		if (sleep((caddr_t)&prpidlock, PCATCH|(PZERO+1))) {
			prunlock(pp);
			return(u.u_error = EINTR);
		}
	}
	prpidlock = u.u_procp;
	return(u.u_error = 0);
}

/* Undo prlock. */

prunlock(pp)
register struct proc *pp;
{
	int s;

	if (pp != u.u_procp) {
		s = splclock(); /* keep clock out during process state change */
		pp->p_flag &= ~SPROCIO;
		splx(s);
	}
	if (prpidlock == u.u_procp) {
		prpidlock = 0;
		if (prpidwant) {
			prpidwant = 0;
			wakeup((caddr_t)&prpidlock);
		}
	}
	return(0);
}


/* Lock a region
*
* Before duplicating or attaching an object process region, the
* subject process (/proc user) must lock the region.  A
* straightforward call to reglock() might deadlock waiting for
* the region since it may already be locked by the object process,
* which will not run as long as SPROCIO is set (indicating /proc
* I/O in progress).  We avoid the deadlock by explicitly checking
* the region flags before attempting the reglock().  If the region
* is already locked, we clear SPROCIO to allow the object process
* to run, go to sleep waiting for the lock, release the lock, set
* SPROCIO again, and return a failure indication so that the caller
* can arrange to start the I/O operation over (since the object
* process might have execed or exited in the interim).  The lock on
* the associated inode pointer is handled similarly (if the caller
* requests it).  The order in which we do the locks (reglock/plock)
* doesn't matter in this case since we never actually try to lock
* both the region and the inode unless we can do so without sleeping,
* therefore there's no deadlock problem.
*
* This isn't pretty, but it's life in the modern world.
*/
int
prreglock(pp, rp, ilock)
register struct proc *pp;
register reg_t *rp;
int ilock;
{
	register struct inode *ip;
	int s;

	if (pp != u.u_procp && rp->r_flags & RG_LOCK) {
		s = splclock();
		pp->p_flag &= ~SPROCIO;
		reglock(rp);
		pp->p_flag |= SPROCIO;
		splx(s);
		regrele(rp);
		return(-1);;
	}
	reglock(rp);
#ifdef NOTNOW
/* XXX */
	if (pp != u.u_procp && ilock && (ip = rp->r_iptr)) {
		if (ip->i_flag & ILOCK) {
			s = splclock();
			pp->p_flag &= ~SPROCIO;
			regrele(rp);
			plock(ip);
			pp->p_flag |= SPROCIO;
			splx(s);
			prele(ip);
			return(-1);
		}
		plock(ip);
	}
#endif /* NOTNOW XXX */
	return(0);
}

/*
* proc_attach():	Attach the given object process region to the subject process at
*		an appropriate address.  Return -1 if the region cannot be
*		safely locked before attaching, 0 otherwise.
*
* proc_detach():  Detach the object process region.
*/

/* MIPS code is similar to 3b2 code, with changes to mirror other MIPS VM code.
* proc_attach and proc_detach look like shmat(), shmdt(), and shmaddr() from
*   mips/io/shm.c.
* prumap and prunmap look like xdup() in mips/os/text.c
*/

int
proc_attach(pp, rp, rw, prpp)
struct proc		*pp;
struct region		*rp;
enum uio_rw		rw;
preg_t			**prpp;
{
	register uint	vaddr;
	register uint	vaddrt;
	register preg_t	*prp;

	vaddr = 0;

	for (prp = u.u_procp->p_region; prp->p_reg; prp++) {
		vaddrt = (uint)(prp->p_regva + ctob(prp->p_reg->r_pgsz));
		if (vaddr < vaddrt && prp->p_type != PT_STACK)
			vaddr = vaddrt;
	}
	vaddr = (vaddr + NBPS - 1) & ~SOFFMASK;
	if (prreglock(pp, rp, 0) == -1)
		return(-1);
	prp = attachreg(rp, &u, (caddr_t)vaddr, PT_SHMEM,
	  (rw == UIO_WRITE) ? SEG_RW : SEG_RO);
	regrele(rp);
	*prpp = prp;
	return(0);
}

proc_detach(prp)
preg_t *prp;
{
	caddr_t vaddr = prp->p_regva;
	reg_t *rp = prp->p_reg;

	reglock(rp);
	detachreg(prp, &u, 1 /* invalidate TLBs */);
}
