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
#ident	"$Header: graf_r3030.c,v 1.3.1.6.1.3.1.3 90/11/02 17:55:22 beacker Exp $"

/*
 * Graphics board interface
 */

#include "sys/sysmacros.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/ipc.h"		/* needed for stream.h and shm.h */
#include "sys/msg.h"		/* needed for stream.h */
#include "sys/stream.h"		/* defines queue_t in grafreg.h */
#include "sys/grafreg.h"
#include "sys/immu.h"		/* needed for region.h */
#include "sys/region.h"
#include "sys/shm.h"
#include "sys/cmn_err.h"
#include "sys/errno.h"
#include "sys/debug.h"
#include "sys/sbd.h"
#include "sys/iop.h"
#include "sys/cpu_board.h"
#include "sys/rambo.h"

extern struct shmid_ds shmem[];
extern struct shminfo shminfo;
extern time_t time;

struct shmid_ds *ipcget();
int	colour_check_ret;

extern int mono, do_mono, do_colour;
extern caddr_t frm_addr;
extern int cpu_config;
extern int	color;
extern int showconfig;
extern struct shmid_ds *display_shm1, *display_shm2, *display_shm3;
extern struct shmid_ds *display_shm4;

#define	KREG		(R3030_GRAPHICS_REG_ADDR + R3030_KERNEL_OFFSET)
#define	RAMBO_COUNT	(RAMBO_BASE + RAMBO_TCOUNT)

grafinit ()
{
	/*
	 * Don't make the shared memory available unless a graphics board
	 * is available.
	 */

	if ( mono || do_mono ) {
		mono_init();
		if (do_mono)
			teReset(0);
	} else {
		if (showconfig) {
			cmn_err(CE_WARN, "monochrome not being used\n");
		}
	}
	color_probe();
}

color_probe ()
{
	register char *p;
    
	if (IS_SABLE)
		color = 0;
	else {
		if ( !color_check())
			color = 0;
		else {
			color = 1;
			color_init();
		}
	}
	return color;
}

#define	V_BLANK_BIT	0x20
#define	H_BLANK_BIT	0x10
#define	COLOR_RSV	0xCE

#define	CLOCK_DUR	1562500
#define	CLOCK_SLACK	1000

#define	COLOR_VBLNK_MIN	15
#define	COLOR_VBLNK_MAX	50

/*
 *  This routine relies on the Kernel color register to follow the
 *  Pizazz spec of 12/89
 *
 *  The algorithm is as follows:
 *	Read the kreg
 *	loop for a certain time of rambo info
 *		if the reserved bit change assume that we have been reading
 *			random information and not a color board
 *		count number of transision of the h_blank signal
 *	if the number of transitions of the hblank is reasonable then
 *		we have a color board
 */
color_check() {
	register volatile unsigned long *time;
	register volatile unsigned long *kreg;
	register	  unsigned long stime, etime, ntime;
	register	  unsigned long	skreg, s1kreg;
	register          unsigned long	v_count, color_ret;
	register	  unsigned long wrap_iminent;

	time = (volatile unsigned long *)PHYS_TO_K1(RAMBO_COUNT);
	kreg = (volatile unsigned long *)PHYS_TO_K1(KREG);
	stime = *time;
	if ((stime + CLOCK_DUR + CLOCK_SLACK) < stime) {
		wrap_iminent=1; 
		etime = stime + CLOCK_DUR + CLOCK_SLACK;
	} else {
		wrap_iminent=0; 
		etime = stime + CLOCK_DUR;
	}
	skreg = *kreg;
	v_count = 0;
	color_ret = 1;
	while (color_ret) {
		ntime = *time;
		if ( ((wrap_iminent && (ntime < stime)) | !wrap_iminent) &&
				(ntime > etime) ) {
			break;
		}
		if ((skreg ^ (s1kreg = *kreg)) & COLOR_RSV) {
			color_ret = 0;
			break;
		}
		if ((skreg ^ s1kreg) & V_BLANK_BIT) {
			v_count++;
			skreg = s1kreg;
		}
	}
	if (color_ret) {
		if ((v_count > COLOR_VBLNK_MIN) && (v_count < COLOR_VBLNK_MAX))
			return colour_check_ret = 1;
	}
	return colour_check_ret = 0;
}

color_init()
{
	register struct shmid_ds *sp, *spc;
	register struct region *rp, *rpc;
	preg_t prp;
	int status, end, i, gp, size;
	register pde_t *pt, *ptc;
	char *cp;

	/*
	 * Don't make the shared memory available unless a graphics board
	 * is available.
	 */

	if ( ! color )
		return;

	cpu_config |= P_COLOUR;
	/*
	 * Get a shared memory pointer
	 */
	sp = ipcget (GBUFKEY, IPC_PERMANENT|IPC_CREAT|0666, shmem, 
		shminfo.shmmni, sizeof (*sp), &status);
	display_shm1 = sp;
	spc = ipcget (GBFCKEY, IPC_PERMANENT|IPC_CREAT|0666, shmem, 
		shminfo.shmmni, sizeof (*sp), &status);
	display_shm2 = spc;
	ASSERT(sp);
	ASSERT(spc);
	ASSERT(status);
	ASSERT(rp = allocreg (NULL, RT_SHMEM, 0));
	ASSERT(rpc = allocreg (NULL, RT_SHMEM, 0));

	sp->shm_segsz = R3030_GRAPHICS_FRAME_SIZE;
	sp->shm_reg = rp;
	sp->shm_atime = sp->shm_dtime = 0;
	sp->shm_ctime = time;
	sp->shm_lpid = 0;
	sp->shm_cpid = 0;		/* root */
	rp->r_flags |= RG_NOFREE;

	spc->shm_segsz = R3030_GRAPHICS_FRAME_SIZE;
	spc->shm_reg = rpc;
	spc->shm_atime = spc->shm_dtime = 0;
	spc->shm_ctime = time;
	spc->shm_lpid = 0;
	spc->shm_cpid = 0;		/* root */
	rpc->r_flags |= RG_NOFREE;

	/*
	 * Expand the region low to high
	 */
	prp.p_reg = rp;
	ptexpand (&prp, btoc(R3030_GRAPHICS_FRAME_SIZE), 1);
	prp.p_reg = rpc;
	ptexpand (&prp, btoc(R3030_GRAPHICS_FRAME_SIZE), 1);

	end = btoc(R3030_GRAPHICS_FRAME_SIZE);
	gp = pnum(R3030_GRAPHICS_FRAME_ADDR);

	for (i = 0; i < ctos(end); i++) {
	    pt = rp->r_list[i];
	    ptc = rpc->r_list[i];
	    size = end - stoc(i);
	    if (size > NPGPT)
		size = NPGPT;

	    while (--size >= 0) {
		pt->pgi.pg_pde = mkpde(PG_N|PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, gp);
		pdetodbd(pt)->dbd_type = DBD_NONE;
		pt++;
		ptc->pgi.pg_pde = mkpde(PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, gp);
		pdetodbd(ptc)->dbd_type = DBD_NONE;
		ptc++;
		gp += btoc(R3030_GRAPHICS_LINE_SIZE);
	    }
	}

	rp->r_pgsz += end;
	regrele (rp);
	rpc->r_pgsz += end;
	regrele (rpc);

	/* set up the vector version */
	sp = ipcget (GBFVKEY, IPC_PERMANENT|IPC_CREAT|0666, shmem, 
		shminfo.shmmni, sizeof (*sp), &status);
	display_shm3 = sp;
	ASSERT(sp);
	ASSERT(status);
	ASSERT(rp = allocreg (NULL, RT_SHMEM, 0));

	sp->shm_segsz = R3030_GRAPHICS_VECTOR_FRAME_SIZE;
	sp->shm_reg = rp;
	sp->shm_atime = sp->shm_dtime = 0;
	sp->shm_ctime = time;
	sp->shm_lpid = 0;
	sp->shm_cpid = 0;		/* root */
	rp->r_flags |= RG_NOFREE;

	/*
	 * Expand the region low to high
	 */
	prp.p_reg = rp;
	ptexpand (&prp, btoc(R3030_GRAPHICS_VECTOR_FRAME_SIZE), 1);

	end = btoc(R3030_GRAPHICS_VECTOR_FRAME_SIZE);
	gp = pnum(R3030_GRAPHICS_FRAME_ADDR);
	
	for (i = 0; i < ctos(end); i++) {
		pt = rp->r_list[i];
		size = end - stoc(i);
		if (size > NPGPT)
			size = NPGPT;
	    
		while (--size >= 0) {
			pt->pgi.pg_pde = mkpde(PG_N|PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, gp++);
			pdetodbd(pt)->dbd_type = DBD_NONE;
			pt++;
		}
	}
	
	rp->r_pgsz += end;
	regrele (rp);

	if (!IS_SABLE) {
		/*
		 * Get a shared memory pointer
		 */
		sp = ipcget (GREGKEY, IPC_PERMANENT|IPC_CREAT|0666, shmem, 
				shminfo.shmmni, sizeof (*sp), &status);
		display_shm4 = sp;
		ASSERT(sp);
		ASSERT(status);
		ASSERT(rp = allocreg (NULL, RT_SHMEM, 0));

		sp->shm_segsz = R3030_GRAPHICS_REG_SIZE;
		sp->shm_reg = rp;
		sp->shm_atime = sp->shm_dtime = 0;
		sp->shm_ctime = time;
		sp->shm_lpid = 0;
		sp->shm_cpid = 0;		/* root */
		rp->r_flags |= RG_NOFREE;
	
		/*
		 * Expand the region low to high
		 */
		prp.p_reg = rp;
		ptexpand (&prp, btoc(R3030_GRAPHICS_REG_SIZE), 1);
	
		end = btoc(R3030_GRAPHICS_REG_SIZE);
		pt = rp->r_list[0];
	
		pt->pgi.pg_pde = mkpde(PG_N|PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, pnum(R3030_GRAPHICS_REG_ADDR + R3030_RAMDAC_ADLO_OFFSET));
		pdetodbd(pt)->dbd_type = DBD_NONE;
		pt++;
		pt->pgi.pg_pde = mkpde(PG_N|PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, pnum(R3030_GRAPHICS_REG_ADDR + R3030_RAMDAC_ADHI_OFFSET));
		pdetodbd(pt)->dbd_type = DBD_NONE;
		pt++;
		pt->pgi.pg_pde = mkpde(PG_N|PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, pnum(R3030_GRAPHICS_REG_ADDR + R3030_COL_REG_OFFSET));
		pdetodbd(pt)->dbd_type = DBD_NONE;
		pt++;
		pt->pgi.pg_pde = mkpde(PG_N|PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, pnum(R3030_GRAPHICS_REG_ADDR + R3030_COL_DATA_OFFSET));
		pdetodbd(pt)->dbd_type = DBD_NONE;
		pt++;
		pt->pgi.pg_pde = mkpde(PG_N|PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, pnum(R3030_GRAPHICS_REG_ADDR + R3030_XSERVER_OFFSET));
		pdetodbd(pt)->dbd_type = DBD_NONE;
		pt++;
		pt->pgi.pg_pde = mkpde(PG_N|PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, pnum(R3030_GRAPHICS_REG_ADDR + R3030_KERNEL_OFFSET));
		pdetodbd(pt)->dbd_type = DBD_NONE;
		pt++;
		pt->pgi.pg_pde = mkpde(PG_N|PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, pnum(R3030_GRAPHICS_REG_ADDR + R3030_WRITE_OFFSET));
		pdetodbd(pt)->dbd_type = DBD_NONE;
	
		rp->r_pgsz += end;
		regrele (rp);

		/* clear the vertical retrace interrupt */
		cp = (char *)PHYS_TO_K1(R3030_GRAPHICS_REG_ADDR + R3030_KERNEL_OFFSET);
		*cp = 0;
	}
}
