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
#ident	"$Header: mono.c,v 1.2.1.8.1.3.1.3 90/11/02 17:55:42 beacker Exp $"

/*
 * Monochrome graphics using rambo
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
#include "sys/bitmap.h"
#include "sys/utsname.h"

#define	RAMBO_ALIGN	(512*1024)
#define	RAMBO_CHECK(x)	(((unsigned long)(x))&~(RAMBO_ALIGN-1))

extern struct shmid_ds shmem[];
extern struct shminfo shminfo;
extern time_t time;
extern	Bitmap	monoscreen;

struct shmid_ds *ipcget();

caddr_t getcpages();
extern int conspty;
extern int vrtcalled;
caddr_t frm_addr = 0;
extern int do_mono;
extern int mono;
extern int showconfig;
extern int cpu_config;
extern struct shmid_ds *display_shm1, *display_shm2;

mono_init ()
{
	register struct shmid_ds *sp, *spc;
	register struct region *rp, *rpc;
	preg_t prp;
	int status, end, i, gp, size;
	caddr_t begin;
	register pde_t *pt, *ptc;
	register volatile struct rambo_chan *ram_ch_p = 
		&(((struct rambo *)PHYS_TO_K1(RAMBO_BASE))->rambo_ch[1]);

#if 0
	if ( !mono ) {
		if (showconfig) {
			cmn_err(CE_WARN, "monochrome not being used\n");
		}
		return;
	}
#endif
	
	if ( frm_addr != NULL) {
		cmn_err(CE_WARN, "monochrome frame buffer already allocated\n");
		return;
	}
	/*
	 * Don't make the shared memory available unless frame buffer
	 * is available.
	 */
	size = btoc(MONO_FRAME_SIZE);
	if ( (frm_addr = (caddr_t)getcpages(2*size, 1)) == NULL) {
		cmn_err(CE_WARN, "can't get monochrome frame buffer\n");
		return;
	}
	/*
	 * This code is here to get around a rambo problem
	 */
	if ( RAMBO_CHECK(frm_addr) != RAMBO_CHECK(frm_addr + MONO_FRAME_SIZE) ) {
		/* free the first half */
		begin = frm_addr;
		frm_addr += ctob(size);
	} else {
		/* free the second half */
		begin = frm_addr + ctob(size);
	}
	while (size > 0) {
		freepage(pnum(svirtophys(begin)));
		begin += ctob(1);
		size--;
	}
	if (do_mono) {
		bcopy(PHYS_TO_K1(PROM_RAMBO_BUFFER), frm_addr, MONO_FRAME_SIZE);
		init_r3030_mono(K0_TO_K1(frm_addr), 0);
		mono_reset_rambo_addr(svirtophys(frm_addr));
	} else {
		/* setup Rambo to do output */

		mono_r3030_clear(K0_TO_K1(frm_addr));
		ram_ch_p->dma_mode = FLUSH_RAMBO_FIFO | CLR_DMA_ERR;
		ram_ch_p->dma_laddr = svirtophys(frm_addr);
		ram_ch_p->dma_block_s = (MONO_FRAME_SIZE >>BLOCK_SHIFT);
		ram_ch_p->dma_mode = CHANNEL_EN | AUTO_RELOAD;
	}
	cpu_config |= P_MONO;
	strcpy(utsname.m_type, MT_R3030_S);	/* we are now an RS3230 */

	/*
	 * Get a shared memory pointer
	 */
	sp = ipcget (GBMNUNC, IPC_PERMANENT|IPC_CREAT|0666, shmem, 
		shminfo.shmmni, sizeof (*sp), &status);
	display_shm1 = sp;
	spc = ipcget (GBMONCH, IPC_PERMANENT|IPC_CREAT|0666, shmem, 
		shminfo.shmmni, sizeof (*sp), &status);
	display_shm2 = spc;
	ASSERT(sp);
	ASSERT(spc);
	ASSERT(status);
	ASSERT(rp = allocreg (NULL, RT_SHMEM, 0));
	ASSERT(rpc = allocreg (NULL, RT_SHMEM, 0));

	sp->shm_segsz = MONO_FRAME_SIZE;
	sp->shm_reg = rp;
	sp->shm_atime = sp->shm_dtime = 0;
	sp->shm_ctime = time;
	sp->shm_lpid = 0;
	sp->shm_cpid = 0;		/* root */
	rp->r_flags |= RG_NOFREE;
	
	spc->shm_segsz = MONO_FRAME_SIZE;
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
	ptexpand (&prp, btoc(MONO_FRAME_SIZE), 1);
	prp.p_reg = rpc;
	ptexpand (&prp, btoc(MONO_FRAME_SIZE), 1);

	end = btoc(MONO_FRAME_SIZE);
	gp = pnum(svirtophys(frm_addr));

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
			ptc->pgi.pg_pde = mkpde(PG_M|PG_VR|PG_LOCK|PG_SV|PG_NDREF, gp++);
			pdetodbd(ptc)->dbd_type = DBD_NONE;
			ptc++;
		}
	}

	rp->r_pgsz += end;
	regrele (rp);
	rpc->r_pgsz += end;
	regrele (rpc);
}
