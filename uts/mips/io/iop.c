/*
 * $Header: iop.c,v 1.6.1.2 90/01/09 18:22:32 wje Exp $
 */
#include "sys/param.h"

#ifndef STANDALONE
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/sbd.h"
#include "sys/cmn_err.h"
#include "sys/iop.h"
#include "sys/immu.h"
#include "sys/edt.h"
#include "sys/systm.h"
#include "sys/cpu_board.h"
#include "sys/boot.h"
#include "bsd43/sys/syslog.h"
#else
#include "machine/cpu.h"
#include "machine/iop.h"

#define	cmn_err(pri,fmt)	_io_abort(fmt)
#define	log(msg,fmt,arg)	printf(fmt,arg)
#define	IS_SABLE	0
#define	ASSERT(args)
#define	splall()	(0)
#define	splhi()		(0)
#define	splx(args)

extern int stdio_init;
#endif	/* STANDALONE */

volatile struct iopb *iopb;

#ifdef IOP_STATS
struct iop_stats IopStats[] = {
    { "IRQ busy", 0 },
    { "IRQ number of loops", 0 },
    { "IOPB free offset", 0 },
    { "Lost Uart xmit", 0 },
    { 0, 0 },
};
#endif

#define	IOP_BSIZE	256
char iop_buffer[IOP_BSIZE];

iop_initialize ()
{
	register volatile IopControl *ic;
	short *sizp = (short *)iop_buffer;
	int status;

	iopb = (struct iopb *)PHYS_TO_K1(IOPBBASE);

	if ((ic = (IopControl *)iop_alloc (IOPIOCB, sizeof (*ic))) == NULL) {
		cmn_err (CE_CONT,
			 "iop_init: iop_alloc failed\n");
	} else {
		*sizp = IOP_BSIZE;
		ic->iop_page[0] = (u_long)K0_TO_PHYS(iop_buffer);
		ic->iop_status = 0;
		ic->iop_cmd = GIVE_IOP_BUFFER;
		iop_poke (IOPIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(ic), 0);
		iop_wait (IOPIOCB, IOPB_SPIN, &status, 0);
		iop_clear (IOPIOCB);
#ifndef STANDALONE
		iop_debug(1);		/* turn off the ^x for iop */
#endif
	}
}

/*
 * iop_alloc()
 * allocates nbytes from a communication area.  The value
 * returned will be in K1 address space (non cached).  Most
 * devices will have command and status registers located in
 * this area.  
 *
 * Arguments:
 * id = index into iocb structure.
 * nbytes = number of bytes to allocate from general pool.
 *
 * Return codes:
 * 0 = failure.  No memory left
 * non-zero = K1 address of allocated block.
 */
iop_alloc (id, nbytes)
    register int id, nbytes;
{
    register volatile struct iocb *p;
    register int rtn;
    register int s;
    
#ifndef	STANDALONE
    ASSERT(iopb);
#else
    if (iopb == NULL) {
	iop_initialize();
    }
#endif	/* STANDALONE */

    if (iopb->free_off == 0)	/* first time */
      iopb->free_off = ((sizeof (*iopb) + sizeof (long) -1 )/sizeof (long)) * sizeof (long);
    
    if ((id < 0) || (id >= MAXIOCB))
      cmn_err (CE_PANIC, "iop_alloc: invalid id\n");
    p = &iopb->iop_iocb[id];
    
    s = splall();

    /*
     * If previously allocated return it instead of allocating a
     * new one.
     */
    if (p->ioc_buf != 0)
      rtn = p->ioc_buf;
    else {
	/*
	 * What we return will be used as structure pointer in most
	 * cases.  We must therefore align these pointers to four byte
	 * boundaries which the R2000 requires.
	 */
	nbytes = ((nbytes + sizeof (long) - 1)/sizeof (long)) * sizeof (long);
	if ((iopb->free_off + nbytes) > IOPBSIZE)
	  rtn = 0;
	else {
	    rtn = PHYS_TO_K1(iopb->free_off + IOPBBASE);
	    p->ioc_buf = rtn;
	    bzero (rtn, nbytes);
	    iopb->free_off += nbytes;
#ifdef IOP_STATS
	    IopStats[IOPSTAT_FREE].s_stat = iopb->free_off;
#endif
	}
    }

    splx (s);
    return rtn;
}

iop_setbuf (id, val)
register int id;
u_long val;
{
    register volatile struct iocb *p;
    
    if ((id < 0) || (id >= MAXIOCB))
      cmn_err (CE_PANIC, "iop_setbuf: invalid id\n");
    p = &iopb->iop_iocb[id];
    p->ioc_buf = val;
}

/*
 * iop_poke()
 * Tell the IOP to start working on this command.  If val is
 * a pointer to a command block it must be converted into an
 * offset from the beginning of the communication area.  There
 * is a macro for this called K1_TO_IOPB_OFFSET.
 *
 * Arguments:
 * id = index into iocb structure.
 * flag = IOPB_SLEEP = wait until the command semaphore is clear and then
 *                   initiate new command. Calls sleep.
 *      = IOPB_SPIN = same as IOPB_SCAN except doesn't call sleep.
 *      = IOPB_NOWAIT = return -1 if command semaphore is busy.
 * val = device specific.  Any 32 bit quantity.
 * sleep_addr = If non-zero this address will be used when sleep is called.
 *              else iop_poke will use the address of the iocb block.
 *
 * Return values:
 * 0 = command was posted successfully.
 * -1 = needed to wait for command semaphore to clear and flag was
 *      set to IOPB_NOWAIT.
 * -2 = sleep was interrupted
 */
iop_poke (id, flag, val, sleep_addr)
    register int id, flag, val;
    register caddr_t sleep_addr;
{
    register volatile char *iop_addr = (char *)PHYS_TO_K1(IOP_CMDREG);
    register volatile int *iop_addr_sable = 
			(int *)PHYS_TO_K1(IOP_CMDREG_SABLE);
    register volatile struct iocb *p;
    register int s;
    
    if ((id < 0) || (id >= MAXIOCB))
      cmn_err (CE_PANIC, "iop_poke: invalid id");
    p = &iopb->iop_iocb[id];
    
    if (p->ioc_cmdsem != SEMAPHORE_CLEAR) {
	if (flag == IOPB_NOWAIT)
	  return -1;
	else if (flag == IOPB_SPIN)
	  while (p->ioc_cmdsem != SEMAPHORE_CLEAR)
	    ;
	else
	  while (p->ioc_cmdsem != SEMAPHORE_CLEAR)
#ifndef STANDALONE
	    if (sleep (sleep_addr == NO_SLEEP_ADDR ?
		       (caddr_t)p : sleep_addr,
		       PRIBIO|PCATCH))
	      return -2;
#else
	    _scandevs();
#endif  /* STANDALONE */
      }

    s = splhi();

    p->ioc_cmdparm = val;
    p->ioc_cmdsem = SEMAPHORE_SET;
    wbflush();

    /*
     * IopIntrReady will be set if the IOP has ack'd my interrupt.
     * We must also wait until the V50 has finished writting to the
     * pal or else the processors will collide and someone's write
     * will fail.
     */
    if (IS_SABLE) {
	while ((iopb->intr_sem == 0) ||
	       ((*iop_addr_sable & IopIntrReady) == 0))
	  ;
	*iop_addr_sable = SetIOPIRQ;
    }
    else {
	while ((iopb->intr_sem == 0) ||
	       ((*iop_addr & IopIntrReady) == 0))
	  ;
	*iop_addr = SetIOPIRQ;
    }
    splx(s);
    wbflush();
    
    return 0;
}

/*
 * iop_wait()
 * After the command has been issued by iop_poke sometimes a driver needs
 * to poll until completion or wishes to test an see if the last command
 * has completed.
 *
 * Arguments:
 * id = index into iocb structure.
 * flag = IOPB_SCAN = wait until the command semaphore is clear and then
 *                   initiate new command. Calls _scandev()
 *      = IOPB_SPIN = same as IOPB_SCAN except doesn't call _scandevs().
 *      = IOPB_NOWAIT = return -1 if command semaphore is busy.
 * status = returns the 32 bit quantity found in the parameter field for
 *          the semaphore.  This is always returned.
 *
 * Return codes:
 * 0 = successful completion.  If op == IOPB_COMMAND the semaphore is clear
 *     and you would be able to issue a new command without waiting.
 *     If op == IOPB_STATUS the semaphore is set indicating that the IOP has
 *     completed its last command.
 * -1 = needed to wait and flag is set to IOPB_NOWAIT.
 * -2 = sleep was interrupted (Only valid for the KERNEL).
 */
iop_wait (id, flag, status, sleep_addr)
    register int id, flag, *status;
    register caddr_t sleep_addr;
{
    register volatile struct iocb *p;
    register int rtn;
    
    if ((id < 0) || (id >= MAXIOCB))
      cmn_err (CE_PANIC, "iop_wait: invalid id");
    p = &iopb->iop_iocb[id];
    
    if (flag == IOPB_NOWAIT)
      rtn = (p->ioc_statsem == SEMAPHORE_CLEAR) ? -1 : 0;
    else {
	rtn = 0;
	if (flag == IOPB_SPIN)
	  while (p->ioc_statsem == SEMAPHORE_CLEAR)
	    ;
	else
	  while (p->ioc_statsem == SEMAPHORE_CLEAR)
#ifndef STANDALONE
	    if (sleep (sleep_addr == NO_SLEEP_ADDR ?
		       (caddr_t)p : sleep_addr,
		       PRIBIO|PCATCH)) {
		clear_cpu_led();
		return -2;
	    }
#else
	    _scandevs();
	clear_cpu_led();
#endif /* STANDALONE */
    }

    *status = p->ioc_statparm;
    return rtn;
}

/*
 * iop_cmdsem()
 * Will return the status of the command semaphore.  Some async drivers
 * need to send out commands like acknowledging a receiver interrupt while
 * a transmit command is in progress.
 *
 * Arguments:
 * id = index into iocb structure.
 *
 * Return codes:
 * SEMAPHORE_CLEAR = The iop has at least read the command into its
 *                   internal memory.  You could call iop_poke without
 *                   waiting right now.
 * SEMAPHORE_SET = The iop hasn't acknowledged your last command yet.
 */
iop_cmdsem (id)
    register int id;
{
    register volatile struct iocb *p;
    
    if ((id < 0) || (id >= MAXIOCB))
      cmn_err (CE_PANIC, "iop_cmdsem: invalid id");
    p = &iopb->iop_iocb[id];
    
    return p->ioc_cmdsem;
}

/*
 * iop_clear()
 * You're responsible for clearing the status semaphore after the IOP
 * has set it.  If you don't the IOP will never give you status again.
 *
 * Arguments:
 * id = index into iocb structure
 *
 * Return codes:
 * 0 = successful
 * -1 = already cleared
 */
iop_clear (id)
    register int id;
{
    register volatile struct iocb *p;
    register int rtn;
    
    if ((id < 0) || (id >= MAXIOCB))
      cmn_err (CE_PANIC, "iop_clear: invalid id");
    p = &iopb->iop_iocb[id];
    
    if (p->ioc_statsem == SEMAPHORE_CLEAR)
      rtn = -1;
    else
      rtn = 0;
    p->ioc_statsem = SEMAPHORE_CLEAR;

    /*
     * TODO: poke the IOP at this point.  Bob needs to know when the status
     * has changed.
     */
    return rtn;
}

/*
 * iop_wakeup()
 * If your has call iop_poke or iop_wait with IOPB_SLEEP use this routine
 * to wake the guy up.
 *
 * Arguments:
 * id = index into iocb structure
 * val = If zero it's assumed that you let the iop choose the address
 *       to sleep on in iop_poke/iop_wait.  iop_wakeup will use this address.
 *       If non-zero this'll be the argument passed to wakeup.  In this case
 *       you can just call wakeup if you want.
 * Return codes:
 * none.
 */
iop_wakeup (id, wakeup_addr)
    register int id, wakeup_addr;
{
    register volatile struct iocb *p;
    
    if (wakeup_addr) {
#ifndef	STANDALONE
	wakeup (wakeup_addr);
#endif
	return;
    }

    if ((id < 0) || (id >= MAXIOCB))
      cmn_err (CE_PANIC, "iop_clear: invalid id");
    p = &iopb->iop_iocb[id];
#ifndef STANDALONE
    wakeup ((caddr_t)p);
#endif
    return;
}

#ifndef STANDALONE

/*
 * copy the physical pages of a kernel buffer into 
 * an array ptp.  The array is to be passed to IOP
 * such that it can  set up the mapper to perform 
 * DMA operations.
 */
iop_ptes(ptp, addr, len)
u_long *ptp, addr;
int len;
{
	register i;

	i = ((addr & (NBPC -1)) + len + (NBPC -1)) / NBPC;
	if (iskvir(addr)) {
		while (i--) {
			*ptp++ = (u_long)(kvtokptbl(addr)->pgm.pg_pfn);
			addr += NBPC;
		}
	} else {
		addr = K1_TO_PHYS(addr) >> BPCSHIFT;
		while (i--)
			*ptp++ = addr++;
	}
}

iop_ptes_short(ptp, addr, len)
u_short *ptp;
u_long addr;
int len;
{
	register i;

	i = ((addr & (NBPC -1)) + len + (NBPC -1)) / NBPC;
	if (iskvir(addr)) {
		while (i--) {
			*ptp++ = (u_short )(kvtokptbl(addr)->pgm.pg_pfn);
			addr += NBPC;
		}
	} else {
		addr = K1_TO_PHYS(addr) >> BPCSHIFT;
		while (i--)
			*ptp++ = (u_short )addr++;
	}
}

#endif STANDALONE

/*
 * TODO:
 * See if we can get Jonathon to change is pal
 */
int irq_map[] = {
    ClrIRQ0,
    ClrIRQ1,
    ClrIRQ2,
    -1,
    ClrIRQ4,
};

extern unsigned kbd_buzzerout;

#ifndef STANDALONE
iop_intr (ep, level)
    int *ep, level;
{
    register volatile char *iop_addr = (char *)PHYS_TO_K1(IOP_CMDREG);
    register volatile int *iop_addr_sable = (int *)PHYS_TO_K1(IOP_CMDREG_SABLE);
    register volatile struct iocb *p;
    register struct edt *edt_ptr;
    register struct vme_intrs *vp;
    register int seen_intr = 0, s;
    register volatile int addr;
    int stat;

    s = splhi();
    while (iopb->intr_sem == 0)
      ;
    if (IS_SABLE)
      *iop_addr_sable = irq_map[level];
    else
      *iop_addr = irq_map[level];

    splx(s);
    wbflush();

    for (edt_ptr = &edt[0]; edt_ptr < &edt[nedt]; edt_ptr++) {
	vp = edt_ptr->e_intr_info;
	p = &iopb->iop_iocb[vp->v_vec];
	ASSERT((int)p & K1BASE);
	
	/*
	 * All IOP tasks mush set the status semaphore when they
	 * interrupt.  The check to see if brl is greater than level
	 * is for debugging purposes.  This could happen if a device
	 * forgot to call iop_clear after processing it's interrupt.
	 */
	if ((p->ioc_statsem != SEMAPHORE_CLEAR) && vp->v_vintr) {
	    if (vp->v_brl == level) {
		(*vp->v_vintr)(edt_ptr->e_base, ep, p);
		seen_intr++;
	    }
	}
    }
    /*
     * check to see if buzzer is out and complete.
     */
    if ( kbd_buzzerout ) {
	if ( iop_wait (BUZZERIOCB, IOPB_NOWAIT, &stat, 0) == 0 ) {
		iop_clear (BUZZERIOCB);
		kbd_buzzerout = 0;
	}
    }

	iop_v50_print();		/* check to see if we need to print */
}
#endif	/* !STANDALONE */

iop_v50_print() {
	register volatile struct iocb *p;
    
	p = &iopb->iop_iocb[IOPIOCB];
	if (p->ioc_statsem) {
		register volatile IopControl *ic;
		int status;

		if ((ic = (IopControl *)iop_alloc (IOPIOCB, sizeof (*ic)))
				!= NULL && p->ioc_statparm == GIVE_IOP_BUFFER) {
			short *sizp = (short *)iop_buffer;

			iop_clear(IOPIOCB);

			log(BSD43_LOG_ERR, "iop: %s", iop_buffer);

			p->ioc_statparm = 0;

			*sizp = IOP_BSIZE;
			ic->iop_page[0] = (u_long)K0_TO_PHYS(iop_buffer);
			ic->iop_status = 0;
			ic->iop_cmd = GIVE_IOP_BUFFER;

			iop_poke (IOPIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(ic),
				 0);
			iop_wait (IOPIOCB, IOPB_SPIN, &status, 0);
		}
		iop_clear (IOPIOCB);
	}
}

#ifndef	STANDALONE
iop_intr0 (ep)
    int *ep;
{
    iop_intr (ep, 0);
}

iop_intr1 (ep)
    int *ep;
{
    iop_intr (ep, 1);
}

iop_intr2 (ep)
    int *ep;
{
    iop_intr (ep, 2);
}

iop_intr4 (ep)
    int *ep;
{
    iop_intr (ep, 4);
}

#endif	/* STANDALONE */

/*
 * prom_restart()
 *
 * This will cause the IOP to restart and reload the R2000 just like power
 * on except that memory will not be touch (The semaphore area is cleared).
 * Before jumping to IPL prom_restart() will dump IopStats
 */
_prom_restart (restart_val)
    int restart_val;
{
    register IopControl *ic;
    register struct iocb *i;
    register struct iop_stats *is;
    int *prom_watch = (int *)(PROMCOMM + JUMP_LOC);
    int status;

    /*
     * When the R2000 is restarted by the V50 the R2000 prom code will look 
     * here to find an offset that it should as an entry point and jump to it.
     * restart_val is a prom address in the range of 0xbfc00000 to 
     * 0xbfc0003c.  The 0xbfc00000 is not relevant and we must mask it off.
     * firmware.h has the defines that are used by locore.s when this routine
     * is called.
     */
    *prom_watch = restart_val & 0xff;

#ifdef IOP_STATS
    cmn_err (CE_CONT, "================ IOP statistics ================\n\n");
    for (is = &IopStats[0]; is->s_name; is++)
      cmn_err (CE_CONT, "%s: 0x%x\n", is->s_name, is->s_stat);
    cmn_err (CE_CONT, "\n================================================\n");
#endif

	iop_take_buf();
   
    if ((ic = (IopControl *)iop_alloc (IOPIOCB, sizeof (*ic))) == NULL) {
	cmn_err (CE_CONT, "restart_block: iop_alloc failed, can't restart\n");
	while (1);
    }
    
	/* tell him to go restart */
	ic->iop_cmd = RELOAD_IPL;
	iop_poke (IOPIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(ic), 0);
	iop_wait (IOPIOCB, IOPB_SPIN, &status, 0);
	while (1);		/* shouldn't get here. */
}

iop_take_buf() {	
	register IopControl *ic;
	int status;

	if ((ic = (IopControl *)iop_alloc (IOPIOCB, sizeof (*ic))) == NULL) {
		cmn_err (CE_CONT, "iop_take_buf: iop_alloc failed\n");
		while (1);
	}
    
	/* take away the buffer that we gave the v50 */
	ic->iop_page[0] = (u_long) 0;
	ic->iop_status = 0;
	ic->iop_cmd = GIVE_IOP_BUFFER;
	iop_poke (IOPIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(ic), 0);
	iop_wait (IOPIOCB, IOPB_SPIN, &status, 0);
	iop_clear (IOPIOCB);
}

#ifndef STANDALONE
set_leds(on_off)
{
	on_off ? set_cpu_led() : clear_cpu_led();
}

set_cpu_led ()
{
    _cpu_led (LEDOn);
}
#endif	/* !STANDALONE */

clear_cpu_led ()
{
    _cpu_led (LEDOff);
}

#ifndef PROM
_cpu_led (val)
    register int val;
{
    register volatile char *iop_addr = (char *)PHYS_TO_K1(IOP_CMDREG);
    register volatile int *iop_addr_sable = (int *)PHYS_TO_K1(IOP_CMDREG_SABLE);

    while (iopb->intr_sem == 0)
      ;
    if (IS_SABLE)
      *iop_addr_sable = val;
    else
      *iop_addr = val;

    wbflush();
}
#endif	/* !PROM */

#ifdef STANDALONE

iop_status_reg()
{
    register volatile char *iop_addr = (char *)PHYS_TO_K1(IOP_CMDREG);

    return *iop_addr;
}
#endif	/* STANDALONE */

cpu_status_reg()
{
    register volatile char *iop_addr = (char *)PHYS_TO_K1(IOP_CMDREG);
    register volatile int *iop_addr_sable = (int *)PHYS_TO_K1(IOP_CMDREG_SABLE);

    if (IS_SABLE)
      return *iop_addr_sable;
    else
      return *iop_addr;
}
      

iop_debug (arg)
    int arg;
{
    register IopControl *ic;
    int status;	

    if ((ic = (IopControl *)iop_alloc (IOPIOCB, sizeof (*ic))) == NULL) {
	printf ("iop_debug: Couldn't alloc iopb\n");
	return;
    }
    
    ic->iop_cmd = arg ? MON_OFF : MON_ON;
    iop_poke (IOPIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(ic), 0);
    iop_wait (IOPIOCB, IOPB_SPIN, &status, 0);
    iop_clear (IOPIOCB);
}

iop_level0()
{
}
