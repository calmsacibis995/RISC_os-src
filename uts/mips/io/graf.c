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
#ident	"$Header: graf.c,v 1.8.1.4.1.3.1.2 90/10/16 12:13:12 beacker Exp $"

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

extern struct shmid_ds shmem[];
extern struct shminfo shminfo;
extern time_t time;

struct shmid_ds *ipcget();

int conspty = -1;
int vrtcalled = 0;
JupiterPutc JupPutCharData;
extern unsigned ipl_special_mask;
extern unsigned int irq5to;

video_intr()
{
    register int *intrclr = (int *)PHYS_TO_K1(IOP_VRTCLEAR);
    extern write_color();
    extern kbd_irq5on();
    extern kbd_irq5missing();
		
    vrtcalled++;
    untimeout_func(kbd_irq5on,0);
    untimeout_func(kbd_irq5missing,0);
    write_color();
    irq5to = 0;
    irq5off();
    *intrclr = 0;
}

grafprobe ()
{
    register char *p;
    
    if (IS_SABLE)
      JupPutCharData.g_type = JP_NOBOARD;
    else {
	if (cpu_status_reg() & Video_Not_Present)
	  JupPutCharData.g_type = JP_NOBOARD;
	else {
	    g_ginit();
	    g_pinit(0);
	    JupPutCharData.g_type = JP_BOARD;
	    ipl_special_mask = SR_2030MASK;
	    init_2030_colour();
	}
    }

    return JupPutCharData.g_type;
}

grafinit ()
{
    register struct shmid_ds *sp, *spc;
    register struct region *rp, *rpc;
    preg_t prp;
    int status, end, i, gp, size;
    register pde_t *pt, *ptc;

    /*
     * Don't make the shared memory available unless a graphics board
     * is available.
     */
    if (JupPutCharData.g_type & JP_NOBOARD)
      return;

    /*
     * Get a shared memory pointer
     */
    sp = ipcget (GBUFKEY, IPC_CREAT|0666|IPC_PERMANENT, shmem, shminfo.shmmni,
		 sizeof (*sp), &status);
    spc = ipcget (GBFCKEY, IPC_CREAT|0666|IPC_PERMANENT, shmem, shminfo.shmmni,
		 sizeof (*sp), &status);
    ASSERT(sp);
    ASSERT(spc);
    ASSERT(status);
    ASSERT(rp = allocreg (NULL, RT_SHMEM, 0));
    ASSERT(rpc = allocreg (NULL, RT_SHMEM, 0));

    sp->shm_segsz = GRAPHICS_FRAME_SIZE;
    sp->shm_reg = rp;
    sp->shm_atime = sp->shm_dtime = 0;
    sp->shm_ctime = time;
    sp->shm_lpid = 0;
    sp->shm_cpid = 0;		/* root */
    rp->r_flags |= RG_NOFREE;
    
    spc->shm_segsz = GRAPHICS_FRAME_SIZE;
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
    ptexpand (&prp, btoc(GRAPHICS_FRAME_SIZE), 1);
    prp.p_reg = rpc;
    ptexpand (&prp, btoc(GRAPHICS_FRAME_SIZE), 1);

    end = btoc(GRAPHICS_FRAME_SIZE);
    gp = pnum(GRAPHICS_FRAME_ADDR);

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

    if (!IS_SABLE) {
	/*
	 * Get a shared memory pointer
	 */
	sp = ipcget (GREGKEY, IPC_CREAT|0666, shmem, shminfo.shmmni,
		     sizeof (*sp), &status);
	ASSERT(sp);
	ASSERT(status);
	ASSERT(rp = allocreg (NULL, RT_SHMEM, 0));
	
	sp->shm_segsz = GRAPHICS_REG_SIZE;
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
	ptexpand (&prp, btoc(GRAPHICS_REG_SIZE), 1);
	
	end = btoc(GRAPHICS_REG_SIZE);
	gp = pnum(GRAPHICS_REG_ADDR);
	
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
    }
    
}

/*
 * This routine does a little more than the name implies.  This routine
 * dispatches the character to the appropriate place.
 */
extern char *console;
grafputc (c, rflag)
    char c;
    int rflag;			/* true == do \r with \n */
{
    extern int panic_level;
    mblk_t *bp;

    if (JupPutCharData.g_type & JP_NOBOARD ||
	(*console == '0') || (*console == '1') || (*console == 't') ||
	(*console == 'T')) {
    	uartputc (c);
    } else if (panic_level || !(JupPutCharData.g_type & JP_STREAM)) {
	if (IS_SABLE)
	  uartputc (c);
	else {
	    if ((c == '\n') && rflag)
	      g_putchar ('\r');
	    g_putchar (c);
	}
    }
    else {
	if ((c == '\n') && rflag)
	  grafputc ('\r',0);
	if ((bp = allocb (1, BPRI_HI)) == NULL)
	  return;
	*bp->b_wptr++ = c;
	pts_wput (JupPutCharData.g_wq, bp);
    }
}

grafgetc ()
{
    register int c;

    if (JupPutCharData.g_type & JP_NOBOARD ||
	(*console == '0') || (*console == '1') || (*console == 't') ||
	(*console == 'T')) {
      c = uartgetc();
    } else if (panic_level || !(JupPutCharData.g_type & JP_STREAM))
      while ((c = g_getchar (0)) == 0);
    else
      cmn_err (CE_PANIC, "grafgetc called with stream active\n");
    
    return c & 0x7f;
}
	
/*
 * Make sure that kernel output is redirected to frame buffer
 * instead of non-existant stream.
 */
graphics_shutdown()
{
    if ((JupPutCharData.g_type & JP_NOBOARD) == 0) {
        initcmap();
        g_pinit(0);
        graphics_remove_queue(conspty);
    }
}

/*
 * Return 1 if a graphics board is available else 0.  This routine
 * is provided so that other routines don't need to know about JupPutCharData
 */
JupiterGraphics()
{
    return (JupPutCharData.g_type & JP_NOBOARD) ? 0 : 1;
}

/*
 * When minor 0 of the pseudo tty is opened this becomes the output channel
 * for kernel printfs.  This routine prevents spty.c from knowing about
 * JupPutCharData.
 */
graphics_set_queue(unit,wq)
    register int unit;
    register queue_t *wq;
{
    if ( conspty == -1 ) {
    	conspty = unit;
    	JupPutCharData.g_type |= JP_STREAM;
    	JupPutCharData.g_wq = wq;
	return(0);
    } else {
	return(EBUSY);
    }
}

/*
 * If the control side of the console is closed the kernel should resort
 * to printing directly on the frame buffer.
 */
graphics_remove_queue(unit)
    register int unit;
{
    if ( conspty != unit ) {
	return(EINVAL);
    } else {
    	conspty = -1;
    	JupPutCharData.g_type &= ~JP_STREAM;
    	JupPutCharData.g_wq = 0;
    	return(0);
    }
}


/*
  Local Variables:
  c-indent-level: 4
  c-argdecl-indent: 4
  End:
  */
