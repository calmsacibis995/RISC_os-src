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
#ident	"$Header: exec.c,v 1.37.1.5.1.8.1.3 90/11/20 17:21:37 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/

/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/uio.h"
#include "sys/sysinfo.h"
#include "sys/var.h"
#include "sys/tuneable.h"
#include "sys/acct.h"
#include "sys/map.h"
#include "sys/message.h"
#include "sys/reg.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/vnode.h"
#include "sys/pathname.h"
#include "sys/file.h"
#include "sys/vfs.h"
#include "sys/pfdat.h"
#include "sys/cpu_board.h"

extern dbd_t null_dbd;

/* normal include files */
#include "a.out.h"
#include "elf_abi.h"
#include "elf_mips.h"

#ifndef STYP_LIB
#define STYP_LIB 0x40000000
#endif

extern int	userstack[];

struct execa {
	char	*fname;
	char	**argp;
	char	**envp;
};

#define DOING_EXEC	0
#define DOING_MAP	1

#define	_EM_FORCE	0xff

/*
 * exec system call, without and with environments.
 */

exec()
{
	((struct execa *)u.u_ap)->envp = NULL;
	exece();
}

/*
 * Read the given vnodes header and see if starts with "#!".  If it
 * does, then the first 32 bytes of the file define a program with one
 * optional argument to run as an interpreter with the original file as
 * an argument.
 */
struct vnode *
try_shell_exec(vp, sharg)
	struct vnode *vp;
	char *sharg;
{
	register char *cp;
	char execdata[64];
	int	resid;
	char	*execnamep;

	/*
	 * First read in the first few bytes of the file.
	 */
	execdata[0] = '\0';	/* for zero length files */
	u.u_error =
	    vn_rdwr(UIO_READ, vp, (caddr_t)execdata, sizeof (execdata),
		0, UIO_SYSSPACE, IO_UNIT, &resid);
	if (u.u_error)
		goto bad1;

	/*
	 * If we get a short read, then put a new-line at the end of the
	 * string.  If the string doesn't start with "#!", punt.
	 */
	if (resid != 0) {
		execdata[sizeof(execdata) - resid] = '\n';
	}
	if ((execdata[0] != '#') || (execdata[1] != '!')) {
		goto bad;
	}

	/*
	 * Program starts with #!.  Parse line and try to re-open the
	 * real program.  Some of this code is adapted from Berkeley 4.2BSD
	 * Scan line and stomp tabs into spaces.  If a newline is found,
	 * stomp it into a null, and stop scanning line.  If line doesn't
	 * end in a newline, then its an error.
	 */
	cp = &execdata[2];		/* skip "#!" */
	while (cp < &execdata[sizeof(execdata)]) {
		if (*cp == '\t')
			*cp = ' ';
		else
		if (*cp == '\n') {
			*cp = '\0';
			break;
		}
		cp++;
	}
	if (*cp != '\0')
		goto bad;

	/*
	 * Skip leading spaces after #! up to first word in string.
	 * What follows is the path name of the program to execute.
	 * Then find the first space/null and delimit the word.  If
	 * the word ends in a space, then an argument may be present.
	 * Copy it to sharg so that the outer code can get at it.
	 */
	cp = &execdata[2];
	while (*cp == ' ')			/* skip leading spaces */
		cp++;
	execnamep = cp;				/* remember interpreter */
	while (*cp && *cp != ' ')		/* skip past interpreter */
		cp++;
	sharg[0] = 0;
	if (*cp) {				/* capture one argument */
		*cp++ = '\0';
		while (*cp == ' ')		/* skip spaces */
			cp++;
		if (*cp) {
			strcpy(sharg, cp);
			ASSERT(strlen(sharg) <= 64);
		}
	}
	VN_RELE(vp);

	/* return the interpreters vnode */
	vp = (struct vnode *)0;
	if ((u.u_error = lookupname(execnamep, UIO_SYSSPACE, FOLLOW_LINK,
	     (struct vnode **)0, &vp)) || vp == NULL)
	{
		/* namei will have set error to ENOENT, but this would
		 * imply that the file containing the "#!..." does not
		 * exist.  Returning ENOEXEC would tell the shell to try to
		 * use the file as a shell script, also not right.  So for
		 * no better errno I use EINVAL.
		 */
		u.u_error = EINVAL;
	}
	return (vp);

bad:
	u.u_error = ENOEXEC;
bad1:
	VN_RELE(vp);
	return ((struct vnode *) NULL);
}

exece()
{
	register reg_t   *rp;
	register preg_t  *prp;
	struct vnode *vp;
	register caddr_t vwinadr;
	caddr_t sptalloc();
#ifdef PERFECT_COLORING	
	caddr_t sptalloc_scc();
#endif PERFECT_COLORING	
	int newsp;
	unsigned size;
	uint shlb_scnsz;
	uint shlb_datsz;
	int shlb_buf;
	struct exdata *shlb_dat;
	int i;
	int s;
	char sharg[64];
	char ucomm[PSCOMSIZ];
	int is_shell;
	int uid, gid;
	struct pathname pn;
	struct vattr vattr;
	struct execa *uap = (struct execa *)u.u_ap;

	sysinfo.sysexec++;

	u.u_execsz = USIZE + SINCR + SSIZE + btoc(NCARGS-1);

	u.u_error = pn_get(uap->fname, UIO_USERSPACE, &pn);
	if (u.u_error)
		return;
	if (pn.pn_pathlen > 0) {
	  char *ptr;
	  unsigned len;

	  ptr = &pn.pn_path[pn.pn_pathlen-1];
	  while (*ptr != '/' && ptr != pn.pn_path)
	  	ptr--;
	  if (*ptr == '/') ptr++;
	  len = pn.pn_pathlen - (ptr - pn.pn_path);
	  if (len > PSCOMSIZ)
	  	len = PSCOMSIZ;
	  bcopy((caddr_t)ptr, (caddr_t)ucomm, len+1);
	}

	u.u_error = lookuppn(&pn, FOLLOW_LINK, (struct vnode **)0, &vp);
	pn_free(&pn);
	if (u.u_error)
		return;

	/*
	 * Record setuid and setgid info for later.
	 */
	uid = u.u_uid;
	gid = u.u_gid;
	if (u.u_error = VOP_GETATTR(vp, &vattr, u.u_cred)) {
		VN_RELE(vp);
		return;
	}
	if ((vp->v_vfsp->vfs_flag & VFS_NOSUID) == 0) {
		if (vattr.va_mode & VSUID)
			uid = vattr.va_uid;
		if (vattr.va_mode & VSGID)
			gid = vattr.va_gid;
	} else if ((vattr.va_mode & VSUID) || (vattr.va_mode & VSGID)) {
		struct pathname tmppn;

		u.u_error = pn_get(uap->fname, UIO_USERSPACE, &tmppn);
		if (u.u_error) {
			VN_RELE(vp);
			return;
		}
		uprintf("%s: Setuid execution not allowed\n", tmppn.pn_buf);
		pn_free(&tmppn);
	}
	/*
	 * Open of the file succeeded.  Now see if it is a normal executable.
	 * If its not, see if its an executable script (starts with #!).
	 * If so, try_shell_exec returns the vnode of the scripts interpreter.
	 */
	is_shell = 0;
	if (gethead(vp, &u.u_exdata,DOING_EXEC)) {

		if (u.u_error != ENOEXEC) {
			VN_RELE(vp);
			return;
		}

		/*
		 * Its definitely not a regular executable.  See if
		 * try_shell_exec likes it.
		 */
		bzero(sharg, sizeof(sharg));
		/*
		 * Clear u_error before calling try_shell_exec() because
		 * the above gethead() failed and set it to non-zero.
		 */
		u.u_error = 0;
		vp = try_shell_exec(vp, sharg);
		if (!vp) {
			/* nope, not a program to be run */
			return;
		}
		ASSERT(u.u_error == 0);

		/* Okie dokie. file was an executable shell script */
		is_shell = 1;

		/* now get header info from interpreter */
		if (gethead(vp, &u.u_exdata,DOING_EXEC)) {
			/* oh well, interpreter was bogus */
			VN_RELE(vp);
			return;
		}
	}

	/*
	 * Look at what we got, u.u_exdata.ux_mag = 407/410/413
	 *
	 *  410 is RO text in an "aligned" a.out file, not demanded paged
	 *  (pure executable)
	 *
	 *  413 is RO text in an "aligned" a.out file, demanded paged
	 *  (demand paged )
	 *
	 *  407 is RW text loaded in data section, not demanded paged
	 *  (old impure format)
	 */
	switch (u.u_exdata.ux_mag) {
	    case 0410:
	    case 0413:
		break;
		
	    case 0443:
		u.u_error = ELIBEXEC;
		break;

	    /*
	     * This process won't have a text region.  Adjust the data
	     * sizes accordingly.
	     */
	    case 0407:	  /* {1} */
		u.u_exdata.ux_dsize += u.u_exdata.ux_tsize;
		u.u_exdata.ux_tsize = 0;
		u.u_exdata.ux_datorg = u.u_exdata.ux_txtorg;
		u.u_exdata.ux_txtorg = 0;
		u.u_exdata.ux_doffset = u.u_exdata.ux_toffset;
		break;
		
	    default:
		u.u_error = ENOEXEC;
		break;
	}
	if (u.u_error) {
		VN_RELE(vp);
		return;
	}

	/* Clear defer-signal mask */
	u.u_procp->p_chold = 0;

	/*
	 *	Allocate memory to read the arguments, the shared library
	 *	section, and the amount of memory needed to store the vnode
	 *	pointer and header data for each a.out.
	 *	XXX - add back in when doing shared libs 
	shlb_scnsz = (u.u_exdata.ux_lsize + NBPW) & (~(NBPW - 1));
	shlb_datsz = u.u_exdata.ux_nshlibs * sizeof(struct exdata);
	size = btoc( shlb_scnsz + shlb_datsz + 3*(NCARGS + NBPW) );
	 */
	shlb_scnsz = 0;
	shlb_datsz = 0;
#ifdef mips
	shlb_scnsz = (u.u_exdata.ux_lsize + NBPW) & (~(NBPW - 1));
	shlb_datsz = u.u_exdata.ux_nshlibs * sizeof(struct exdata);
	size = btoc( shlb_scnsz + shlb_datsz + 3*(NCARGS) + EA_SIZE );
#else
	size = btoc( shlb_scnsz + shlb_datsz + 3*(NCARGS) );
#endif

	/* avail[rs]mem can be modified at interrupt level */
	s = splhi();
	if (availrmem - size < tune.t_minarmem
	  || availsmem - size < tune.t_minasmem) {
		splx(s);
		nomemmsg("exece", size, 0, 0);
		u.u_error = EAGAIN;
		VN_RELE(vp);
		return;
	}
	availrmem -= size;
	availsmem -= size;

	/*
	 * vwinadr is pointer in kernel space of area for building exec
	 * arguments
	 */
#ifdef PERFECT_COLORING	
	while ((vwinadr = sptalloc_scc(size, PG_VR|PG_G|PG_M|PG_NDREF|PG_SV,
			0, 0, btoct((int)userstack-size*NBPP))) == NULL) {
#else	  
	while ((vwinadr = sptalloc(size, PG_VR|PG_G|PG_M|PG_NDREF|PG_SV,
				0, 0)) == NULL) {
#endif PERFECT_COLORING
		mapwant(sptmap)++;
		sleep(sptmap, PMEM);
	}
	splx(s);

	/*	Locate and verify any needed shared libraries.
	 *
	 *	Note: ip is unlocked in getshlibs().
	 */

	if (u.u_exdata.ux_nshlibs) {
		/* XXX determine where to put shlib stuff */
		shlb_buf = (int)vwinadr + 3*(NCARGS + NBPW);
		shlb_dat = (struct exdata *)(shlb_buf + shlb_scnsz);

		if (getshlibs(shlb_buf, shlb_dat))
			goto done;
	} else {
		vp->v_flag |= VTEXT;
	}

	if (fuexarg(u.u_ap, vwinadr + (ctob(size) - 3*NCARGS), &newsp,
			    is_shell, sharg)) {
		exec_err(shlb_dat, u.u_exdata.ux_nshlibs);
		goto done;
	}

	/*
	 * Remove the old process image
	 */

	shmexec();	/* insert calls for "execfunc"	*/
	punlock();	/* unlock locked regions before detaching */

	u.u_prof.pr_scale = 0;

	prp = u.u_procp->p_region;
	while (rp = prp->p_reg) {
		reglock(rp);
		detachreg(prp, &u, 0 /* don't invalidate TLBs */);
	}
	u.u_nshmseg = 0;
	ASSERT(u.u_procp->p_rssize == 0);

	/*
	 * The new tlbpid is needed after we're done with the
	 * old process and before we need anything for the new one.
	 * This effectively flushes the tlb of any pages that
	 * were released by execbld.
	 * Getxfile may read in the text of the new process and 
	 * will need to use the new tlbpid.
	 */
	new_tlbpid(u.u_procp);


	/* 
	 * Initialized the wired tlb entries for the new process.
	 */
	setup_wired_tlb(u.u_procp);

	/*
	 * load any shared libraries that are needed
	 */

	if (u.u_exdata.ux_nshlibs) {
		for(i = 0; i < u.u_exdata.ux_nshlibs; i++, shlb_dat++) {
			if (getxfile(shlb_dat, PT_LIBTXT, PT_LIBDAT, 0, 0)) {
				exec_err(++shlb_dat, u.u_exdata.ux_nshlibs - i - 1);
				psignal(u.u_procp, SIGKILL);
				goto done;
			}
		}
	}

	/*
	 * load the a.out's text and data.
	 */

	if (getxfile(&u.u_exdata, PT_TEXT, PT_DATA, uid, gid, DOING_EXEC)) {
		psignal(u.u_procp, SIGKILL);
		goto done;
	}

	/*
	 *	Set up the pages for user's stack. (stack actually built 
	 *	by fuexarg)
	 * 	pass the "start" of stack ptes,  since the stack
	 *	grows down, this is highest pte
	 */

	if (stackbld(kvtokptbl(vwinadr + ctob(size)) - 1, newsp)) {
		psignal(u.u_procp, SIGKILL);
		goto done;
	}

	/*
	 * Remember file name for accounting && ps.
	 */
	u.u_procp->p_flag |= SEXECED;
	u.u_acflag &= ~AFORK;
	bcopy((caddr_t)ucomm, (caddr_t)u.u_comm, sizeof(u.u_comm)-1);
	u.u_comm[sizeof(u.u_comm)-1] = 0;
	setregs();

done:

	sptfree(vwinadr, size, 1);
	/* avail[rs]mem can be modified at interrupt level */
	s = splhi();
	availrmem += size;
	availsmem += size;
	splx(s);

	return;
}

execmap( fname,flag )
char		*fname;
unsigned long	flag;
{
	struct vnode *vp;
	struct exdata exdata;
	struct pathname pn;

	/* this routine maps a dynamic shared object ala exece */

	u.u_error = pn_get(fname, UIO_USERSPACE, &pn);
	if (u.u_error)
		return;

	u.u_error = lookuppn(&pn, FOLLOW_LINK, (struct vnode **)0, &vp);
	pn_free(&pn);
	if (u.u_error)
		return;

	/*
	 * Open of the file succeeded.  Now see if it is a normal executable.
	 * If its not, see if its an executable script (starts with #!).
	 * If so, try_shell_exec returns the vnode of the scripts interpreter.
	 */
	if (gethead(vp, &exdata,DOING_MAP)) {

		u.u_error = ENOEXEC;
		return;
	}

	/*
	 * Look at what we got, u.u_exdata.ux_mag = 407/410/413
	 *
	 *	shared objects must be 410
	 *
	 *  410 is RO text in an "aligned" a.out file, not demanded paged
	 *  (pure executable)
	 *
	 *  413 is RO text in an "aligned" a.out file, demanded paged
	 *  (demand paged )
	 *
	 *  407 is RW text loaded in data section, not demanded paged
	 *  (old impure format)
	 */
	switch (exdata.ux_mag) {
	    case 0413:
		break;
		
	    default:
		u.u_error = ENOEXEC;
		VN_RELE(vp);
		return;
	}

	/*
	 *	No shared libraries allowed in a shared object
	 */
	if (exdata.ux_nshlibs != 0) {
		u.u_error = ENOEXEC;
		VN_RELE(vp);
		return;
	}

		vp->v_flag |= VTEXT;

	/*
	 * load the a.out's text and data.
	 */

	if (getxfile(&exdata, PT_TEXT, PT_DATA, u.u_uid, u.u_gid, DOING_MAP, flag)) {
		u.u_error = ENOEXEC;
		u.u_rval1 = 0;
	} else {
		u.u_rval1 = exdata.ux_txtorg;
	}

	return;
}

gethead(vp, exec_data, doing)
struct   vnode  *vp;
register struct exdata *exec_data;
int	doing;
{
	Elf32_Ehdr elfhdr;
	Elf32_Phdr proghdr;
	struct	filehdr filhdr;
	struct aouthdr aouthdr;
	struct scnhdr  scnhdr;
	int    opt_hdr = 0;
	int    scns    = 0;
	uint   offset;
	int    resid;
	struct vattr vattr;

	if (doing == DOING_EXEC && 
		(u.u_error = VOP_ACCESS(vp, VEXEC, u.u_cred)))
		goto bad;
	if (PTRACED(u.u_procp)
	    && (u.u_error = VOP_ACCESS(vp, VREAD, u.u_cred)))
		goto bad;
	if (doing == DOING_EXEC && 
	    (vp->v_type != VREG ||
	    VOP_GETATTR(vp, &vattr, u.u_cred) ||
	    (vattr.va_mode & (VEXEC|(VEXEC>>3)|(VEXEC>>6))) == 0)) {
		u.u_error = EACCES;
		goto bad;
	}

	bzero(exec_data, sizeof(struct exdata));
	/*
	 * Read the ELF header
	 */
	u.u_error =
	  vn_rdwr(UIO_READ, vp, (caddr_t)&elfhdr, sizeof (Elf32_Ehdr),
		  0, UIO_SYSSPACE, IO_UNIT, &resid);
	
	if (u.u_error || (resid != 0)) {
		u.u_error = ENOEXEC;
		goto bad;
	}
	if (IS_ELF(elfhdr)) {
		/* Sanity checks */
		if ((elfhdr.e_ident[EI_DATA] != ELFDATA2MSB) ||
		    (elfhdr.e_machine != EM_MIPS) ||
/*
  (elfhdr.e_type == ET_REL) || 
  (elfhdr.e_type == ET_DYN) ||
  (elfhdr.e_type == ET_CORE) ||
 */
		    (elfhdr.e_type == ET_NONE) ||
		    ((elfhdr.e_flags & EF_MIPS_OPSEX) == EF_MIPS_OPSEX) ||
		    (elfhdr.e_phoff == 0) ||
		    (elfhdr.e_phentsize == 0) ||
		    (elfhdr.e_phnum <= 0)) { /* is one section ok? */
	  		u.u_error = ENOEXEC;
        		goto bad;
	  	}
	  	/* ux_ tsize, dsize, bsize, lsize, nshlibs, mag, toffset,
		 * doffset, loffset, txtorg, datorg, entloc */
		exec_data->ux_nshlibs = 0;
        	exec_data->ux_mag = ZMAGIC;
	  	exec_data->ux_entloc = elfhdr.e_entry;
	  	/* 
	   	 * Read the program headers
	   	 */
		offset = elfhdr.e_phoff;
		while (elfhdr.e_phnum-- > 0) {
			u.u_error = 
			  vn_rdwr(UIO_READ, vp, (caddr_t)&proghdr,
				  sizeof (proghdr), offset, UIO_SYSSPACE,
				  IO_UNIT, &resid);
		        switch (proghdr.p_type) {
			case PT_NULL:
			case PT_MIPS_REGINFO:
			case PT_DYNAMIC:
			  break;
			case PT_LOAD:
				switch (proghdr.p_flags & (PF_R+PF_W)) {
				case PF_R: /* text */
					exec_data->ux_tsize = proghdr.p_filesz;
					exec_data->ux_toffset = proghdr.p_offset;
					exec_data->ux_txtorg = proghdr.p_vaddr;
					break;
				case PF_R+PF_W:	/* data */
					exec_data->ux_dsize = proghdr.p_filesz;
					exec_data->ux_doffset = proghdr.p_offset;
					exec_data->ux_datorg = proghdr.p_vaddr;
					exec_data->ux_bsize = proghdr.p_memsz-
					  proghdr.p_filesz;
					break;
				}
				break;
			case PT_INTERP:
			case PT_NOTE:
			case PT_PHDR:
			default:
				u.u_error = ENOEXEC;
				goto bad;
			}
			offset += elfhdr.e_phentsize;
		}
		u.u_execsz += btoc(exec_data->ux_tsize + exec_data->ux_dsize +
				   exec_data->ux_bsize);
		goto good;
	}

	/*
	 * Otherwise, read the COFF file header
	 */

	u.u_error =
	    vn_rdwr(UIO_READ, vp, (caddr_t)&filhdr, sizeof (filhdr),
		0, UIO_SYSSPACE, IO_UNIT, &resid);
	if (u.u_error || (resid != 0)) {
		u.u_error = ENOEXEC;
		goto bad;
	}
	switch (filhdr.f_magic) {
	    case OBJMAGIC:
		break;			/* ok for all */
	    case OBJMAGIC_MIPS2:
	    case OBJMAGIC_MIPS2_OLD:	/* emitted by pre-FCS 2.10 */
		if (!IS_M6000) {
		    u.u_error = ENOEXEC;
		    goto bad;
		}
		break;
	    default:
		u.u_error = ENOEXEC;
		goto bad;
	}

	/*
	 * Next, read the optional unix header 
	 * Note: this is mandatory for mips
	 */

	if (filhdr.f_opthdr >= sizeof(aouthdr)) {
		u.u_error =
		    vn_rdwr(UIO_READ, vp, (caddr_t)&aouthdr, sizeof (aouthdr),
			sizeof(filhdr), UIO_SYSSPACE, IO_UNIT, &resid);
		if (u.u_error || resid != 0) {
			u.u_error = ENOEXEC;
			goto bad;
		}

		opt_hdr = 1;
		exec_data->ux_mag = aouthdr.magic;
		exec_data->ux_entloc = aouthdr.entry;
	};

	if (opt_hdr == 0) {
		u.u_error = ENOEXEC;
		goto bad;
	}
	/*
	 * Fill in exdata from optional aouthdr.
	 */

	exec_data->ux_tsize = aouthdr.tsize;
	exec_data->ux_dsize = aouthdr.dsize;
	exec_data->ux_bsize = aouthdr.bsize;
	exec_data->ux_toffset = N_TXTOFF(filhdr,aouthdr);
	exec_data->ux_doffset = exec_data->ux_toffset + aouthdr.tsize;
	exec_data->ux_txtorg = aouthdr.text_start;
	exec_data->ux_datorg = aouthdr.data_start;

	u.u_execsz += btoc(aouthdr.tsize + aouthdr.dsize + aouthdr.bsize);
	

	/*
	 * Next, read the section headers; there had better be at
	 * least three: .text, .data and .bss. The shared library
	 * section is optional, initialize the number needed to 0.
	 *
	 * This code will be used when shared libs are provided by the loader
	 */

	exec_data->ux_nshlibs = 0;

	offset = sizeof(filhdr) + filhdr.f_opthdr;

	while (filhdr.f_nscns-- > 0) {

		u.u_error =
		    vn_rdwr(UIO_READ, vp, (caddr_t)&scnhdr, sizeof (scnhdr),
			offset, UIO_SYSSPACE, IO_UNIT, &resid);
		if (u.u_error || resid != 0) {
			u.u_error = ENOEXEC;
			goto bad;
		}
		offset += sizeof(scnhdr);

		switch ((int) scnhdr.s_flags) {

		case STYP_TEXT:
			scns |= STYP_TEXT;
			break;

		case STYP_RDATA:
			scns |= STYP_RDATA;
			break;

		case STYP_DATA:
			scns |= STYP_DATA;
			break;

		case STYP_SDATA:
			scns |= STYP_SDATA;
			break;

		case STYP_SBSS:
			scns |= STYP_SBSS;
			break;

		case STYP_BSS:
			scns |= STYP_BSS;
			break;

		case STYP_LIB:
			++shlbinfo.shlblnks;

			if ((exec_data->ux_nshlibs = scnhdr.s_paddr) > shlbinfo.shlbs) {
				++shlbinfo.shlbovf;
				u.u_error = ELIBMAX;
				goto bad;
			}

			exec_data->ux_lsize = scnhdr.s_size;
			exec_data->ux_loffset = scnhdr.s_scnptr;
			break;
		}
	}

#ifdef notdef
	/* make sure you have following three */
	if ( (scns & (STYP_TEXT|STYP_DATA|STYP_BSS))
			!= (STYP_TEXT|STYP_DATA|STYP_BSS)) {
		u.u_error = ENOEXEC;
		goto bad;
	}
#endif notdef
good:
	/*
 	 * Check total memory requirements (in clicks) for a new process
	 * against the available memory or upper limit of memory allowed.
	 */

	if (u.u_execsz > tune.t_maxumem) {
		u.u_error = ENOMEM;
		goto bad;
	}

	if (!(vp->v_flag & VTEXT) && vp->v_count != 1) {
		register struct file *fp;

		for (fp = file; fp < (struct file *)v.ve_file; fp++)
			if (fp->f_count && fp->f_type == DTYPE_VNODE &&
			    (struct vnode *)fp->f_data == vp &&
			    (fp->f_flag&FWRITE)) {
				u.u_error = ETXTBSY;
				goto bad;
			}
	}

	exec_data->vp = vp;
	return(0);
bad:
	return(-1);
}

 
/*
 *	This code assumes that vaddr points to an area that is exactly 3*NCARGS
 *	bytes from the start of the user stack
 */
fuexarg(uap, vaddr, newsp, is_shell, sharg)
struct execa		*uap;
int			vaddr;
int			*newsp;
	int is_shell;
	char *sharg;
{

#ifdef mips
	register char *cp = (char *)vaddr + 2*NCARGS - EA_SIZE;
#else
	register char *cp = (char *)vaddr + 2*NCARGS;
#endif
	register char **ap;
	register char *sp;
	register int *statep = (int *)(vaddr);
#ifdef mips
	register int ucp = (int)userstack - NCARGS - EA_SIZE;
	int fudge;		/* Used for stack alignment */
#else
	register int ucp = (int)userstack - NCARGS;
#endif
	register int np = 0;
	int nc = 0;
	int c;
	int na = 0;
	int nac = 0;
	int env = 0;
	int *newp;

	/* collect arglist */

	ap = uap->argp;
	for (;;) {
		if (ap) {
			for (;;) {
				sp = (char *)fuword((caddr_t)ap);
				if (!is_shell && (sp == 0))
					break;
				*statep++ = ucp;
				if (++np > (NCARGS/2)) {
					u.u_error = E2BIG;
					return(-1);
				}
				if (is_shell && (np == 2) && sharg[0]) {
					/*
					 * Copy optional shell argument in.
					 * The shells arguments become:
					 *	uap->argp[0]
					 *	sharg
					 *	uap->fname
					 *	uap->argp[1..N]
					 */
					u.u_error = copystr(sharg, cp,
						NCARGS-nc, &c);
					if (u.u_error == ENAMETOOLONG)
						u.u_error = E2BIG;
					if (u.u_error)
						return(-1);
					/*
					 * spath returns 0 if sharg[0] == \0
					 * NOT length of 1
					 */
					if (c == 0)
						c++;	/* count the NULL */
					nc += c;
					ucp += c;
					cp += c;
					continue;
				}
				if (is_shell && (((np == 2) && !sharg[0]) ||
						 ((np == 3) && sharg[0]))) {
					/*
					 * If this is to be the second argument
					 * and the #! line has no optional
					 * argument, OR, if this is to be the
					 * third argument and the line has
					 * an optional argument, THEN set
					 * the given argument to the name of
					 * the original file being executed.
					 */
					sp = uap->fname;
					is_shell = 0;
				} else {
					ap++;
				}

				u.u_error = copyinstr(sp, cp, NCARGS-nc, &c);
				if (u.u_error == ENAMETOOLONG)
					u.u_error = E2BIG;
				if (u.u_error)
					return(-1);
				/*
				 * upath returns 0 if *sp == \0
				 * NOT length of 1
				 */
				if (c == 0)
					c++;	/* count the NULL */
				nc += c;
				ucp += c;
				cp += c;
			}
		}
		*statep++ = NULL;	/* add terminator */
		if (!env++) {
			na = np;	/* save argc */
			nac = nc;
			ap = uap->envp; /* switch to env */
			continue;
		}
		break;
	}

	/*
	 * copy back arglist -- i.e.
	 *	set up argument and environment pointers
	 *	arg list is built by pushing onto stack.
	 *	a picture is worth a thousand words:
	 *
	 *	The first area is used for floating point emulation
	 *	code space in the mips implementation
	 *
	 * 	At this point the picture looks like this:
	 *
					high
	 userstack(0x7ffff000) ->	|_______________| ___
					|               |  ^
					| Software FP   |  |
					| emulation save|  | EA_SIZE
					| area (32bytes)|  |
	 				|_______________| _v_
					|               |  ^
					| WASTED SPACE  |  |
					|               |  |
					| "e_strn\0"    |  |
			strings		|    ***        |  |
					| "e_str0\0"    |  | NCARGS
					| "a_strn\0"    |  |
					|    ***        |  |
					| "a_str0\0"    |  |
	 		newp->		|_______________| _v_
					|               |  ^
					|               |  |
					|               |  |
					|               |  |
			statep->	|               |  |
					| 0             |  |
					| ENVn          |  |
					| ...           |  |
					| ENV0          |  |
					| 0             |  | ("holding area")
					| ARGn          |  |
					| ...           |  | 2*NCARGS
					| ARG0          |  |
	 		vaddr->		|_______________|  v
					low



	 * 	We will make it look like this
	 * XXX - Rewrite to not waste the space above the strings
					 _______________
	 empty page for kernel copy	|               |
					|               |
	 userstack(0x7ffff000) ->	|_______________| ___
					|               |  ^
					| Software FP   |  |
					| emulation save|  | EA_SIZE
					| area (32bytes)|  |
	 				|_______________| _v_
					|               |  ^
					| WASTED SPACE  |  |
					|               |  |
					| "e_strn\0"    |  |
					|    ***        |  |
					| "e_str0\0"    |  | NCARGS
					| "a_strn\0"    |  |
					|    ***        |  |
					| "a_str0\0"    |  |
	 		strings		|_______________| _v_
					| 0             |
					| env[n]        |  ^
					| ...           |  |
					| env[0]        |  | (real area)
					| 0             |  |
					| argv[n]       |  |
					| ...           |  | 2*NCARGS
					| argv[0]       |  |
			sp ->		| argc          |  |
					|               |  |
					|               |  |
					|               |  |
					|               |  |
					|               |  |
	 				|_______________|  v

	 */
	
	/*
	 * set up the users stackpointer 1 + 2 is for argc & 2 nulls
	 */
#ifdef mips
	*newsp = (int)userstack - NCARGS - (np + 1 + 2)*NBPW - EA_SIZE;
	newp = (int *)(vaddr + 2*NCARGS - EA_SIZE);
	cp = (char *)newp;
	/*
	 * Stack needs to be double word aligned for floating point stuff.
	 * There's talk of needing quad word alignment so in anticipation
	 * of that need forcing the stack to quad word alignment now.
	 */
	if (*newsp & 0xf) {
		fudge = *newsp & 0xf;
		*newsp -= fudge;
		newp -= fudge / sizeof (int *);
	}
	else {
		fudge = 0;
	}
#else
	*newsp = (int)userstack - NCARGS - (np + 1 + 2)*NBPW;
	newp = (int *)(vaddr + 2*NCARGS);
	cp = (char *)newp;
#endif

	/* copy from holding area to real area */
	for ( c = 0; c < np+2; c++)	/* +2 for two null ptrs */
		*--newp = *--statep;

	*--newp = na;	 /*  argc  	*/

	/* do psargs */
	sp = u.u_psargs;
	np = min( nac, PSARGSZ-1);
	while (np--) {
		if ( (*sp = *cp++) == '\0' )
			*sp = ' ';
		sp++;
	}
	*sp = '\0';

	return(0);
}	

getxfile(exec_data, tpregtyp, dpregtyp, uid, gid, doing, flag)
struct exdata *exec_data;
short  tpregtyp;
short  dpregtyp;
int uid, gid, doing;
unsigned long flag;
{
	register reg_t	*rp;
	register preg_t	*prp;
	register struct vnode  *vp = exec_data->vp;
	int doffset = exec_data->ux_doffset;
	int dbase   = exec_data->ux_datorg;
	int dsize   = exec_data->ux_dsize;
	int bsize   = exec_data->ux_bsize;
	int npgs;


	/*	Load text region.  Note that if xalloc returns
	 *	an error, then it has already done an VN_RELE.
	 *
	 *	if doing == DOING_MAP
	 *	if we get an error here, dec by NBPS (2mb) until we
	 *	hit the lowest data region  looking for a good place.
	 */

	if ( doing == DOING_MAP ) {

		/* we want to check the for a region with the lowest possible
		 * text address and the highest possible data address
		 */
	    prp = findpreg(u.u_procp, PT_DATA);
	    if (prp == NULL) {
		u.u_error = ENOEXEC;
		return(-1);
	    }
	    while (chkmap(&u, (exec_data->ux_txtorg & ~SOFFMASK), 
			((dbase+dsize+bsize+NBPS-4) & ~SOFFMASK)-4) != 0 ) {

		/* if flag == _EM_FORCE then we cannot map where we want
		 * so we return an error here and go no farther
		 */

		if ( flag == _EM_FORCE ) {
		    u.u_error = ENOEXEC;
		    return(-1);
		}

		/* otherwise we keep looking for a place to map this object
		 */
		exec_data->ux_txtorg -= NBPS;
		dbase = (exec_data->ux_datorg -= NBPS);
		exec_data->ux_entloc -= NBPS;

		if ( exec_data->ux_txtorg <= (int)prp->p_regva ) {
		    /* only keep check until we hit the lowest data region */
		    u.u_error = ENOEXEC;
		    return(-1);
		}
	    }
	}

	if ( (struct vnode *)xalloc(exec_data, tpregtyp) == NULL) 
	    return(-1);

	if ((dsize+bsize) ||  dpregtyp == PT_DATA) {
	    /*
	     *	Allocate the data region.
	     */

	    if ((rp = allocreg(vp, RT_PRIVATE, 0)) == NULL) {
		    goto out;
	    }

	    /*
	     *	Attach the data region to this process.
	     */

	    prp = attachreg(rp, &u, dbase & ~SOFFMASK, dpregtyp, SEG_RW);
	    if (prp == NULL) {
		freereg(rp);
		goto out;
	    }

	    /*
	     * Load data region
	     */

	    if (dsize) {
		switch (exec_data->ux_mag) {
		case 0413:
		case 0443:
			if (mapreg(prp, dbase, vp, doffset, dsize)) {
				detachreg(prp, &u, 0 /* don't invalidate TLBs */);
				goto out;
			}
			break;
			/* fall thru case if REMOTE */
		default: 
			if (loadreg(prp, dbase,  vp, doffset, dsize)) {
				detachreg(prp, &u, 0 /* don't invalidate TLBs */);
				goto out;
			}
		}
	    }

	    /*
	     * Allocate bss as demand zero
	     */
	    if (npgs = btoc(dbase + dsize + bsize) - btoc(dbase + dsize)) {
		if (growreg(prp, npgs, DBD_DZERO) < 0) {
			detachreg(prp, &u, 0 /* don't invalidate TLBs */);
			goto out;
		}
	    }

	    regrele(rp);
	}

	/* set SUID/SGID protections, if no tracing */

	if (doing == DOING_EXEC && tpregtyp == PT_TEXT) {
		if (!PTRACED(u.u_procp)) {
			if (uid != u.u_uid || gid != u.u_gid)
				u.u_cred = crcopy(u.u_cred);
			u.u_uid = uid;
			u.u_gid = gid;
			u.u_procp->p_suid = u.u_uid;
			u.u_procp->p_sgid = u.u_gid;
		}
		if (u.u_procp->p_flag & SSEXEC)
			u.u_procp->p_flag |= SPRSTOP;
		else if (u.u_procp->p_flag & STRC)
			psignal(u.u_procp, SIGTRAP);
	}

	VN_RELE(vp);
	/*
	 * if we are vfork'ed, wake up parent
	 */
	if (doing == DOING_EXEC)
	    vrelvm();
	return(0);
out:
	if (u.u_error == 0)
		u.u_error = ENOEXEC;

	VN_RELE(vp);

	return(-1);
}


vrelvm()
{
	if (u.u_procp->p_flag & SVFORK) {
		u.u_procp->p_flag &= ~SVFORK;
		wakeup((caddr_t)u.u_procp);
	}
}

  
getshlibs(bp, dat_start)
unsigned *bp;
struct   exdata *dat_start;
{
	struct   vnode  *vp  = u.u_exdata.vp;
	struct   exdata *dat = dat_start;
	unsigned nlibs = u.u_exdata.ux_nshlibs;
	unsigned rem_wrds;
	unsigned n = 0;
	int	resid;

	u.u_error =
	    vn_rdwr(UIO_READ, vp, (caddr_t)bp, u.u_exdata.ux_lsize,
		u.u_exdata.ux_loffset, UIO_SYSSPACE, IO_UNIT, &resid);
	if (u.u_error || resid) {
		VN_RELE(vp);
		return(-1);
	}

	vp->v_flag |= VTEXT;

	while ((bp < (unsigned int *)dat) && (n < nlibs)) {

		/* Check the validity of the shared lib entry. */

		if ((bp[0] > (rem_wrds = (unsigned int *)dat_start - bp)) ||
		    (bp[1] > rem_wrds) || (bp[0] < 3)) {
			u.u_error = ELIBSCN;
			goto bad;
		}

		/* Locate the shared lib and get its header info.  */

		u.u_syscall = DUEXEC;

		u.u_error = lookupname((caddr_t)(bp + bp[1]), UIO_SYSSPACE,
				       FOLLOW_LINK, (struct vnode **)0, &vp);
		bp += bp[0];
		if (u.u_error) {
			u.u_error = ELIBACC;
			goto bad;
		}

		if (gethead(vp, dat,DOING_EXEC)) {
			VN_RELE(vp);
			if (u.u_error == EACCES)
				u.u_error = ELIBACC;
			else if (u.u_error != ENOMEM)
				u.u_error = ELIBBAD;
			goto bad;
		}

		if (dat->ux_mag != 0443) {
			u.u_error = ELIBBAD;
			VN_RELE(vp);
			goto bad;
		}

		vp->v_flag |= VTEXT;
		++dat;
		++n;
	}

	if (n != nlibs) {
		u.u_error = ELIBSCN;
		goto bad;
	}

	return(0);
bad:
	exec_err(dat_start, n);
	return(-1);
}


/*
 * Build the user's stack.
 */


stackbld(stkptp, newsp)
pde_t	*stkptp;	/* Pointer to page table entries to be used to	*/
			/* initialize the stack with the exec arguments	*/
int	newsp;
{
	reg_t   *rp;
	preg_t  *prp;
	register int	nargc, t, offset;
	register pde_t	*pt;
	register pfd_t	*pfd;

	/*	Allocate a region for the stack and attach it to
	 *	the process.
	 */

	if ((rp = allocreg(NULL, RT_PRIVATE, 0)) == NULL)
		return(-1);

	/*
	 * stack region is attached at the begining of 
	 * the 2 Meg segment it is in
	 */

	if ((prp = attachreg(rp, &u, (int)userstack & ~SOFFMASK,
		PT_STACK, SEG_RW)) == NULL){
		freereg(rp);
		return(-1);
	}
	
	/*	Grow the stack but don't actually allocate
	 *	any pages.
	 */
	nargc = btoc( (int)userstack - newsp );
	offset = btoc((caddr_t)userstack - prp->p_regva);
	rp->r_pgoff = offset;

	if (growreg(prp, nargc, DBD_DZERO) < 0) {
		detachreg(prp,&u, 0 /* don't invalidate TLBs */);
		return(-1);
	}
	
	/*	Initialize the stack with the pages containing the
	 *	exec arguments.  We got these from exece().
	 */

	pt = rp->r_list[0];

	/* stack grows down, need to copy highest pages
	 * that kernel init'd.
	 * These pde's go at the HIGH-END of the first page in the
	 * r_list, starting at the page below userstack.
	 * Field r_pgoff is adjusted to reflect this.
	 */
	
	pt += offset - 1;

	for (t = nargc; --t >= 0 ; pt--, stkptp--){
		ASSERT(pg_isvalid(stkptp));
		pfd = pdetopfdat(stkptp);

		pt->pgm.pg_pfn = stkptp->pgm.pg_pfn;
		/* eliminate any virt-to-phys mappings in S-cache tags */
		invalidate_virt_scache( ptosv(stkptp->pgm.pg_pfn), NBPC );
		pde_unlink(stkptp);
		stkptp->pgi.pg_pde = 0;
		stkptp->pgi.dbd = null_dbd;
		reg_change_nvalid(&sysreg,-1);

		*pdetodbd(pt) = null_dbd;
		pfd->pf_dbd = null_dbd;
		pg_setvalid(pt);
		/* no I/O pending....checkpage() needs to know this */
		pfd->pf_flags |= P_DONE;
	}

	reg_change_nvalid(rp,nargc);
	regrele(rp);

	u.u_ssize = rp->r_pgsz;

	u.u_ar0[EF_SP] = newsp;

	/* zero from end of stack to page boundary */
	bzero((ctob(btoct(newsp))), newsp - ctob(btoct(newsp)));

	return(0);
}


exec_err(shlb_data, n)
register struct exdata *shlb_data;
register int    n;
{
	for (; n > 0; --n, ++shlb_data) {
		VN_RELE(shlb_data->vp);
	}
	VN_RELE(u.u_exdata.vp);
}

/*
 * KLUDGE - this does not really belong here, but it makes my life simpler 
 */

/*
 * Check that the region to be mapped is valid.
 * returns 0 if legal, -1 if illegal.
 * Note that the p_regva for a stack region
 * is the lowest address that the stack can grow to in it's currently lowest
 * segment.
 */

chkmap(up, start, end)
register user_t	*up;
register caddr_t start, end;
{
	register size;
	register preg_t *prp1;
	register caddr_t vaddr;

	for ( prp1 = (up->u_procp)->p_region; prp1->p_reg; ++prp1) {
		vaddr = prp1->p_regva;
		if (end<vaddr)
			break;
		if (start == vaddr || (start< vaddr && end>=vaddr))
			return(-1);
		if (start>vaddr) {
			size = ctob((prp1->p_reg)->r_pgsz);
			if (size != 0 && start<vaddr+size)
				return(-1);
		}
	}

	return(0);
}


/*
 *	Run Time Library System Calls
 */

struct libargs	{
	char	*l_path;
	char	*l_buf;
	int	l_flags;
};

struct lib_stat {
	caddr_t		lb_lib;		/* library control */
	unsigned long	lb_status;	/* status of attach */
	unsigned long	lb_flags;	/* library flags */
};

/* lb_status flags */

#define	LS_PREV	0x1	/* Library was previously attached */

#define MAXLIB	256	/*maximum size of a .lib section */

#define L_TOTAL 0	/* total number of words in the .lib entry */
#define L_TOPATH 1	/* number of words to the pathname information */
#define L_EXTRA 2	/* the start of the "extra" structure */

/* Format of an "extra" structure */

#define L_LENGTH 0	/* Length of an "extra" structure */
#define L_CODE	1	/* The "type" of information */
#define L_VALUE 2	/* the start of the value */

/* L_CODE values */

#define L_FLAGS 1	/* the type of a flags entry */
#define L_LBLIB 2	/* the type of a library section */

#if later

/* Format of a .lib section */

struct libscn {
	long	size;	/* size of this entry */
	long	offset;	/* offset from start of entry to target name */
	long	tsize;	/* text size in bytes */
	long	dsize;	/* initialized data size in bytes */
	long	bsize;	/* uninitialized data size in bytes */
	long	text_start;	/* base of text used for this library */
	long	data_start;	/* base of data used for this library */
	long	bss_start;	/* base of bss used for this library */
	char	pathname[1];
};

#endif

libattach()
{
	struct	vnode	*vp;
	struct	exdata	lib_head;
	unsigned long	libdata[MAXLIB/sizeof(unsigned long)];
	struct	lib_stat lb_ret;
	preg_t	*prp;
	unsigned	ep;
	struct pathname pn;
	struct vattr vattr;
	struct libargs *uap = (struct libargs *)u.u_ap;
	int    resid;

	/*
	 * l_flags is available for future system call expansion.
	 */
	if (uap->l_flags != 0) {
		u.u_error = EINVAL;
		return;
	}

	/*
	 * In case the system call goes remote set u_syscall.
	 */
	u.u_syscall = DUEXEC;
	u.u_error = pn_get(uap->l_path, UIO_USERSPACE, &pn);
	if (u.u_error)
		return;
	u.u_error = lookuppn(&pn, FOLLOW_LINK, (struct vnode **)0, &vp);
	pn_free(&pn);
	if (u.u_error)
		return;
	if (u.u_error = VOP_GETATTR(vp, &vattr, u.u_cred)) {
		VN_RELE(vp);
		return;
	}

	if (gethead(vp, &lib_head, DOING_EXEC)) {
		VN_RELE(vp);
		return;
	}

	if (lib_head.ux_mag != 0443) {
		u.u_error = ELIBBAD;
		VN_RELE(vp);
		return;
	}

	/*
	 * Truncate the size of the .lib section if too big and try
	 * to process anyways.
	 */

	if (lib_head.ux_lsize > MAXLIB)
		lib_head.ux_lsize = MAXLIB;

	vp->v_flag |= VTEXT;

	lb_ret.lb_lib = (caddr_t) NULL;
	lb_ret.lb_flags = NULL;
	lb_ret.lb_status = NULL;

	/*
	 * Verify that a .lib section exists.  If it doesn't, continue
	 * with out generating an error.  Otherwise, extract the "extra"
	 * information from the first .lib entry.  Ignore the remaining
	 * .lib entries
	 */
	if (lib_head.ux_loffset != 0 && lib_head.ux_lsize != 0) {
		register int lsize;

		u.u_error =
		    vn_rdwr(UIO_READ, vp, (caddr_t)libdata, lib_head.ux_lsize,
			    lib_head.ux_loffset, UIO_SYSSPACE, IO_UNIT, &resid);
		if (u.u_error || resid) {
			if (u.u_error == 0)
				u.u_error = ELIBSCN;
			goto badatt;
		}

		lsize = lib_head.ux_lsize/sizeof(unsigned long);

#if later
		/* Until we figure out the expected contents of .lib
		 * section, we can't really process it.
		 */
		if (libdata[L_TOPATH] > lsize) {
			u.u_error = ELIBSCN;
			goto badatt;
		}
		ep = L_EXTRA;
		while( ep < libdata[L_TOPATH]) {
			/*
			 * Ignore with out error any entries that aren't
			 * known types
			 */
			switch (libdata[ep + L_CODE]) {
			 case L_FLAGS:
				lb_ret.lb_flags = libdata[ep + L_VALUE];
				break;
			 case L_LBLIB:
				lb_ret.lb_lib = (caddr_t)libdata[ep + L_VALUE];
				break;
			}
			/*
			 * cast to a long to avoid carries on the increment
			 */
			if ((long)libdata[ep + L_LENGTH] <= 0 ||
			    (ep += libdata[ep + L_LENGTH]) > libdata[L_TOPATH]) {
				u.u_error = ELIBSCN;
				goto badatt;
			}
		}
		/*
		 * Check for a malformed .lib entry
		 */
		if (ep != libdata[L_TOPATH]) {
			u.u_error = ELIBSCN;
			goto badatt;
		}
#endif

	}

	/*
	 * Check to see if this library was previously attached
	 */
	for(prp = u.u_procp->p_region;prp->p_reg; prp++)
		if (prp->p_reg->r_vptr == vp) {
			lb_ret.lb_status |= LS_PREV;
			break;
		}

	if (!(lb_ret.lb_status & LS_PREV)) {
		/*
		 * Verify the library can be attached and attach it
		 */
		if (u.u_exdata.ux_nshlibs >= shlbinfo.shlbs) {
			u.u_error = ELIBMAX;
			goto badatt;
		}
		/*
		 * If text origin address is equal to zero, the
		 * library is attached at the first available
		 * address as selected by the system.
		 */
		if (lib_head.ux_txtorg == 0) {
			lib_head.ux_txtorg = findlibaddr();
			lib_head.ux_datorg = lib_head.ux_txtorg + lib_head.ux_datorg;
		}

		if(getxfile(&lib_head, PT_LIBTXT, PT_LIBDAT, u.u_uid, u.u_gid,DOING_EXEC) == -1) {
			libdetacher(vp);
			return;
		}
		u.u_exdata.ux_nshlibs++;
	} else {
		/*
		 * The library is already attached so decrement the use
		 * count.
		 */
		VN_RELE(vp);
	}

	/*
	 * Return the libinfo structure to the caller.  If the
	 * copyout fails, detach the library and return a fault.
	 */
	if(copyout(&lb_ret,((struct libargs *)u.u_ap)->l_buf,sizeof(lb_ret))) {
		if(!(lb_ret.lb_status & LS_PREV)) {
			libdetacher(vp);
			u.u_exdata.ux_nshlibs--;
		}
		u.u_error = EFAULT;
		return;
	}

	u.u_rval1 = lib_head.ux_txtorg;
	shlbinfo.shlbatts++;
	return;

badatt:
	VN_RELE(vp);
}

libdetach()
{
	struct  libdes {
		caddr_t	libdes;
	} *uap;
	register caddr_t addr;

	register preg_t	*prp;
	register reg_t	*rp;
	struct vnode	*lib_vnode = 0;

	prp = u.u_procp->p_region;
	uap = (struct libdes *)u.u_ap;
	addr = uap->libdes;

	/*
	 * Scan the pregion table for the vnode number of the attached
	 * library.  If none, is found, the address is invalid.
	 */

	for (prp = u.u_procp->p_region; prp->p_reg; prp++)
		if ((prp->p_regva == addr) && prp->p_type == PT_LIBTXT) {
			lib_vnode = prp->p_reg->r_vptr;
			break;
		}

	if (lib_vnode && libdetacher(lib_vnode))
		u.u_exdata.ux_nshlibs--;
	else
		u.u_error = EINVAL;
}

libdetacher(lib_vnode)
struct vnode *lib_vnode;
{
	register preg_t	*prp;
	register reg_t	*rp;

	prp = u.u_procp->p_region;

	/*
	 * Scan the pregion table for the inode number of the attached
	 * library.  If none, is found, the address is invalid.
	 */

	for (prp = u.u_procp->p_region; prp->p_reg; prp++)
		if (prp->p_reg->r_vptr == lib_vnode 
		    && (prp->p_type == PT_LIBTXT || prp->p_type == PT_LIBDAT))
			break;

	if (prp->p_reg == 0)
		return(0);

	/*
	 * Scan the pregion table for the library's regions and detach
	 * them.
	 */
	while (rp = prp->p_reg) {
		if (rp->r_vptr != lib_vnode 
		    || (prp->p_type != PT_LIBTXT && prp->p_type != PT_LIBDAT)) {
			prp++;
			continue;
		}
		reglock(rp);
		u.u_execsz -= rp->r_pgsz;
		detachreg(prp, &u, 1 /* invalidate TLBs */);
	}
	return(1);
}

findlibaddr()
{
	register uint	vaddr;
	register uint	vaddrt;
	register preg_t	*prp;

	vaddr = shlbinfo.shlbaddr;

	for (prp = u.u_procp->p_region ; prp->p_reg ; prp++) {
		if (prp->p_type == PT_STACK)
			continue;
		vaddrt = (uint)(prp->p_regva + ctob(prp->p_reg->r_pgsz));
		if (vaddr < vaddrt)
			vaddr = vaddrt;
	}

	vaddr = (vaddr + NBPS -1 ) & ~SOFFMASK;
	return(vaddr);
}
