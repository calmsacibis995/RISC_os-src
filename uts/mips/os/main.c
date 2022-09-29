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
#ident	"$Header: main.c,v 1.27.1.11.1.1.1.4 91/01/09 19:17:57 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/psw.h"
#include "sys/sbd.h"
#include "sys/sysmacros.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "bsd/sys/time.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/vnode.h"
#include "sys/conf.h"
#include "sys/cmn_err.h"
#include "sys/edt.h"
#include "sys/vfs.h"
#include "sys/kmem.h"

extern struct vnode *makespecvp();

int	physmem;	/* Physical memory size in clicks.	*/
int	maxmem;		/* Maximum available memory in clicks.	*/
int	freemem;	/* Current available memory in clicks.	*/
struct vnode *rootdir;
struct vnode *rootvp;	/* vnode of root filesystem */
struct vnode *swapvp;

struct timeval	boottime;		/* time since 1970 */

extern int	icode[];
extern int	eicode[];
extern int	userstack[];

/*
 *	Initialization code.
 *	fork - process 0 to schedule
 *	     - process 1 execute bootstrap
 *
 *	loop at low address in user mode -- /etc/init
 *	cannot be executed.
 */

main()
{
	register int	(**initptr)();
	register struct edt *ep;
	register int	i;
	extern int	(*io_init[])();
	extern int	(*init_tbl[])();
	extern int	sched();
	extern int	vhand();
	extern int	bdflush();
	extern int	dumpsize, dumplo;
	extern int	(*interrupt_init)();
	extern int	cur_tlbpid;
	extern struct	vfsops nfs_vfsops;
	extern char *arg_root;
	extern char *arg_swap;
	extern int arg_use_bootparams;
	extern char *arg_boot_dev;
	extern char swap_hostname[], *swap_path;
	extern char dump_hostname[], *dump_path;

	startup();

	poll_init();
	clkstart();


	/*	Call all system initialization functions.
	*/

	kmeminit();	/* Let drivers allocate dynamic data structures */
	
#if defined(RISCOS) && defined(QUOTA)
	qtinit();
#endif

	for (initptr = &io_init[0]; *initptr; initptr++) (**initptr)();
	for (ep = &edt[0], i = nedt; i > 0; i--, ep++) {
		if (0 != ep->e_init)
			(*ep->e_init)(ep);
	}
	/*
	 * Initialize any interrupts that could not be intialized earlier.
	 */
	if ((int)interrupt_init)
		(*interrupt_init)();

	/*
	 * Initialize the virtual disk, this has to be done after the
	 * all the disk drivers are initialized, and before devinit().
	 */
	vdinit();

	/*
	 * Start up the virtual memory polling (used to be run
	 * at splclock for 1.5ms)
	 */
	vmmeter_poll();

	/*
	 * This was modified to eliminate the possibility of specifying
	 * one of arg_root or arg_swap and not the other.  Now if
	 * either is specified then use_bootparams is ignored.
	 */
	if (arg_use_bootparams) {
		if (!arg_root && !arg_swap) {
			arg_swap = arg_root = ":";
		} else {
			arg_use_bootparams = 0;
		}
	}
	devinit();
	for (initptr = &init_tbl[0]; *initptr; initptr++) (**initptr)();

	/*
	 * See if we need to use bootparams to get root and/or swap
	 * We do this is the prom said use_bootparams=1, or the boot
	 * device was nfs (diskless boot) and either arg_root or arg_swap
	 * were not specified.
	 */
	if (( arg_use_bootparams || 
		(arg_boot_dev && (strcmp(arg_boot_dev,"nfs") == 0))) && 
					(!arg_root || !arg_swap)) {
		chk_bootparams();
	}
	/* 
	 * Since there is no iinit() to do the mount, we do it here.
	 */
	vfs_mountroot();
#if RISCOS
	/* Adjust swplo when root is on the same partition as swapdev */
	if (rootdev != -1 && rootdev == swapdev) { /* -1 is nfs cookie */
	    extern int rootfs_dblks;
	    if (swplo <  rootfs_dblks) {
		nswap -=  (rootfs_dblks - swplo);
		swplo =  rootfs_dblks;
		printf("New swplo: %d  swap size: %dK bytes\n",
				     swplo, (nswap * NBPSCTR) / 1024);
	    }
	}
#endif

	cmn_err(CE_CONT,
		"Available memory   = %d\n\n", ctob(freemem));

	prt_where = PRW_CONS;

#ifdef	notdef
	printf("***********************************************************************\n\n");
	printf("Copyright (c) 1984 AT&T - All Rights Reserved\n\n");
	printf("THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T INC.\n");
	printf("The copyright notice above does not evidence any actual or\n");
	printf("intended publication of such source code.\n\n");
	printf("***********************************************************************\n\n");
#endif

	boottime = time;
	u.u_start = time.tv_sec;
	u.u_bsd43_start = time;

	/*	This call of swapadd must come after devinit in case
	 *	swap is moved by devinit.  It must also come after
	 *	dskinit so that the disk is don'ed.  This call should
	 *	return 0 since the first slot of swaptab should be used.
	 */

        if (swapdev != NODEV && swapdev != -1) { /* -1 is special nfs cookie */
                swapvp = makespecvp(swapdev, VBLK);
                if (swapadd(swapvp, (char *)0, 0, swplo, nswap) != 0)
                        cmn_err(CE_PANIC, "main - swapadd failed");
        } else if (swapdev == -1) {
                /* this code is stolen from NFS4.0 nfs_swapvp() */
                struct vfs *swapvfsp;
		char name[MAXPATHLEN];
		int namelen, totallen;

                swapvfsp = (struct vfs *)kmem_alloc(sizeof (*swapvfsp));
                VFS_INIT(swapvfsp, &nfs_vfsops, (caddr_t)0);
                nfs_swapvp(swapvfsp, &swapvp, (caddr_t)0, &nswap);
		namelen = strlen (swap_hostname);
		bcopy (swap_hostname, name, namelen);
		name[namelen] = ':';
		totallen = namelen + 1;
		if (swap_path) {
			namelen = strlen (swap_path);
			bcopy (&name[totallen], swap_path, namelen);	
			totallen += namelen;
		}
                swapadd(swapvp, name, totallen, swplo, nswap);
        }

	if (dumpdev != NODEV) {
		/* Determine where we are dump to on dumpdev */
		dumpsize = physmem;		/* Phys # pages */

		/* dump from the start of dumpdev */
		if ((dumplo == 0) && bdevsw[major(dumpdev)].d_size) {
			int dumphi, alignment;

			alignment = 0x2000 >> SCTRSHFT;	/* 8k in sectors */
                        dumphi = (*bdevsw[major(dumpdev)].d_size)(dumpdev) - 1;

			/* round dumphi down to an 8K boundary, so that
			 * last read by driver isn't a partial read
			 */
			dumphi -= dumphi % alignment;
			dumplo = dumphi - ((dumpsize<<BPCSHIFT)>>SCTRSHFT);
			if (dumplo < 0) {
				dumpsize = (dumphi << SCTRSHFT) >> BPCSHIFT;
				dumplo = 0;
			} else 
				dumplo -= dumplo % alignment;
		}
	} else {
		dumplo = 0;
		dumpsize = physmem;		/* Phys # pages */
	}
#ifdef	MIPS_LOCAL
	cmn_err(CE_CONT, "dumplo = 0x%x blks, dumpsize = 0x%x pgs\n", 
		dumplo, dumpsize);
#endif	/* MIPS_LOCAL */

	/* kick off timeout driven events by calling first time */
	schedpaging();

	/*
	 * make init process
	 * enter scheduling loop
	 * with system process
	 */

	cur_tlbpid = SYS_TLBPID_UPB;

	if (newproc(0, 0)) {
		register preg_t	*prp;
		register reg_t	*rp;
		register int	npgs;
		register int	szicode = (char *)eicode - (char *)icode;

		u.u_cstime = u.u_stime = u.u_cutime = u.u_utime = 0;

		/*	Set up the text region to do an exec
		**	of /etc/init.  The "icode" is in ml directory
		*/

		rp = allocreg(NULL, RT_PRIVATE, 0);
		prp = attachreg(rp, &u, USRDATA, PT_TEXT, SEG_RW);
		npgs = btoc(szicode);
		growreg(prp, npgs, DBD_DFILL);
		regrele(rp);

		if (copyout((caddr_t)icode, (caddr_t)(USRDATA), szicode))
			cmn_err(CE_PANIC, "main - copyout of icode failed");

#ifdef NOTDEF
		MIPS -- icode doesn't need a stack
		/*	Allocate a stack region and grow it to
		**	SSIZE pages.
		*/

		rp = allocreg(NULL, RT_PRIVATE);
		prp = attachreg(rp, &u, userstack, PT_STACK, SEG_RW);
		growreg(prp, SSIZE, DBD_DFILL);
		regrele(rp);
#endif NOTDEF
		return(USRDATA);
	}
	if (newproc(0, 0)) {
		u.u_procp->p_flag |= SLOAD|SSYS;
		u.u_procp->p_tlbpid = min(1,SYS_TLBPID_UPB);
		u.u_cstime = u.u_stime = u.u_cutime = u.u_utime = 0;
		bcopy("vhand", u.u_psargs, 6);
		bcopy("vhand", u.u_comm, 5);
		vhand(0);
	}

	if (newproc(0, 0)) {
		u.u_procp->p_flag |= SLOAD|SSYS;
		u.u_procp->p_tlbpid = min(2,SYS_TLBPID_UPB);
		u.u_cstime = u.u_stime = u.u_cutime = u.u_utime = 0;
		bcopy("bdflush", u.u_psargs, 8);
		bcopy("bdflush", u.u_comm, 7);
		bdflush();
	}

	u.u_procp->p_tlbpid = min(3,SYS_TLBPID_UPB);
	bcopy("sched", u.u_psargs, 6);
	bcopy("sched", u.u_comm, 5);
	prt_where = PRW_BUF; /* by default, only send printfs to putbuf */
	sched();

	/*
	 *  Note:  p_tlbpid for system procs not to exceed SYS_TLBPID_UPB
	 */
}
