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
#ident	"$Header: r3200mem.c,v 1.6.2.2.1.1.1.2 90/10/26 16:11:53 beacker Exp $"

/*
 * Memory Error Correcting Code handler.
 * R3200 CPU.
 * This handler works for the
 *  R3250 (32 MByte) and
 *  R3251 (16 MByte) (basically a half-populated R3250)
 * memory boards.
 * To note in particular,
 * it will not work automatically for register-compatible 128 MByte boards.
 *
 * To run the system without printing lots of ecc error messages to the console,
 * use the boot-time switch ecc_noprint.
 * This is intended to let you run with a dead bit if that is important to you.
 * ECC is still enabled and causes traps, but error messages
 * are not printed for single-bit errors.
 *
 * To test operation of the ecc handling code, define DEBUG and then
 * modify locations r3200_single_err and r3200_double_err while the
 * system is running (with dbx or kvar).
 * The value written to the variables is an address to modify 
 * to create bad ecc (single or double err as the names suggest).
 * A periodic monitor routine examines the variables, writes the bad data,
 * and schedules another debug func via timeout to read the word in error
 * at some time in the future (it could get used in normal operation
 * prior to that).  Single-bit errors created this way should cause
 * no problems in a running system.  Double-bit errors panic the system.
 */

#ident "$Header: r3200mem.c,v 1.6.2.2.1.1.1.2 90/10/26 16:11:53 beacker Exp $"

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/dir.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/buf.h"
#include "sys/vmevar.h"
#include "sys/vmereg.h"
#include "sys/elog.h"
#include "sys/ioctl.h"
#include "sys/edt.h"
#include "sys/dump.h"
#include "sys/cpu_board.h"
#include "sys/r3250reg.h"
#include "sys/r3200mem.h"

#define DEBUG 1

extern	int timeout(/*int (*func)(), caddr_t, int */);

/* items declared in the master file */
extern	struct lmem_cntrl lmem_cntrl[];	/* local mem control		*/
extern int	r3200memvec;	/* interrupt vector for all memory boards */
extern int	lmem_errcount;	/* system-total of ecc errors		*/
extern int	lmem_spl;	/* interrupt priority for boards	*/


extern int	ecc_noprint;	/* if nonzero, don't print single-bit errs */


/* the memory boards have CSR and prom IO in supervisor A16 space */

#define	BOARD_ADDR(x) (PHYS_TO_K1(VMESA16_TO_PHYS((x) << SLOT_SHIFT)))

#define	A_MEG	(1024*1024)

#ifndef TRUE
#define	TRUE	1
#define	FALSE	0
#endif


/* forward references */
void		r3200memedtinit();
void		r3200memintr();

static void	lmem_reenable();

#ifdef DEBUG
static void	lmem_test();
static void	lmem_readerr();
static void	repeat_err();
static void	force_err();
#endif /* DEBUG */


/*
 * Figure out the max size of local memory.
 * This routine finds the maximum size of memory that should
 * be probed for valid addresses in the startup routine.
 * It is called early, before device init, so we can't enable
 * ecc reporting here.  We do the other configuration here, however.
 *
 * Figure out what slots have memory boards
 * and what addresses are assigned to them.
 * Fill in our control structure information.
 * Light the board LEDs to contain the logical board number and a
 * "configured" bit.
 * If we are asked to show the configuration, say something about each board.
 *
 * This driver doesn't make any configuration decisions, it just
 * records the decisions made by the PROMs.
 * The one critical configuration rule is that there can be only
 * one partially-populated board (16 Meg on a 32-Meg board)
 * per system and it MUST be the highest addressed (i.e. last) board.
 * The reason for this is unfortunate.
 * The boards, fully populated or not, can only be configured on
 * 32 Mbyte address boundries.
 * The half populated board responds to all memory addresses,
 * including the unpopulated 16 Meg.  We have to arrange not to use
 * this portion of the board.  If it weren't the highest addressed
 * board, it would leave a hole in the address space.
 */

void
lmem_setmax()
{
	int		slot;		/* slot number during scan	*/
	unsigned	board;		/* board logical #		*/
	unsigned	board_type;	/* board type of memory		*/
	ushort		cntrl;		/* mem board control reg	*/
	ushort		a_conf;		/* address config reg		*/
	int		i;
	int 		actual_size;      /* in MB 			*/
	int		logical_board;   /* log. number of the board    */
	int		lmem_max=0;	/* size of local memory		*/
	extern char	*mem_test_max;	/* max to test in startup	*/

	struct lmem_cntrl *mc;	/* per logical board control info	*/
	volatile struct r3250_regs *m_reg; /* mem cntrl/status space	*/

	/* scan through the local-bus slots looking for memory boards */

	logical_board = 0;
	for (slot=0; slot < R3200_PBUS_MAX; slot++) {
		
		m_reg = (struct r3250_regs *) BOARD_ADDR(slot);
		if (badaddr(&(m_reg->mem_idprom.board_type),2)) {
			continue;
		}

		/* At this point, we believe there is a memory board */
		board_type = m_reg->mem_idprom.board_type;

		a_conf = (unsigned) m_reg->mem_addr;

		if(board_type == TYPE_R3250)
		{
			/* clear unused bits */
			a_conf &= UNUSED_BITS_MASK;
		}
		board = (a_conf >> 8);

		if(logical_board >= R3200_MEM_MAX) {
			printf("Memory in slot %d has invalid base addr 0x%x\n",
			slot, (a_conf<<24));
			printf("       the addr config register is 0x%x\n",a_conf);
			continue;
		}
		mc = &(lmem_cntrl[logical_board]);
		mc->configured = 1;
		mc->slot = slot;
		mc->errtime = 0;
		mc->base = (char *)(board<<24);
		mc->vmebase = (char *)((a_conf)<<24);
		mc->leds = 0x8 | logical_board;	/* bd configured led+logical# */
		mc->board_type = m_reg->mem_idprom.board_type & 0xff;

		mc->board_size = m_reg->mem_idprom.board_size & 0xff;


		for (i=0; i<5; i++) {
			mc->board_sn[i] = m_reg->mem_idprom.board_sn[i] & 0x7f;
			/* if there is no PROM, set S/N to 0 */
			if (mc->board_sn[i] == 0x7f) { /* ascii chars */
				mc->board_sn[i] = ' ';
			}
		}
		mc->board_sn[6] = '\0';	/* terminate string */

		switch (mc->board_type) {
		case 0xff:
			/* no ID prom, use reasonable default values */
			/* ??? the following won't work for 128M boards!!! */
			/* Even if 128 MB boards were present. This is */
			/* still a good guess.				   */
			mc->board_type = TYPE_R3250;
			mc->board_size = SIZE_32MEG;
			if (showconfig) {
				printf("Mem: board in slot %d has no IDPROM - assume R3250 32 Megabyte board\n",slot);
			}
			break;
		case TYPE_R3250:
			break;
		case TYPE_R3264:
			break;
		default:
			printf("Mem: board in slot %d has unknown type of 0x%x\n",slot,mc->board_type);
		}

		switch (mc->board_size & SIZE_MASK) {
		case SIZE_32MEG:
			break;
		case SIZE_16MEG:
			break;
		case SIZE_128MEG:
			break;
		case SIZE_64MEG:
			break;
		default:
			printf("Mem: board in slot %d has unknown size of 0x%x\n",slot,mc->board_size & SIZE_MASK);
			break;
		}

		if (mc->base >= (char *)lmem_max) {
			switch (mc->board_size & SIZE_MASK) {
			case SIZE_32MEG:
				lmem_max = (int)mc->base + (32 * A_MEG);
				actual_size = 32 ;
				break;
			case SIZE_16MEG:
				lmem_max = (int)mc->base + (16 * A_MEG);
				actual_size = 16 ;
				break;
			case SIZE_64MEG:
				lmem_max = (int)mc->base + (64 * A_MEG);
				actual_size = 64 ;
				break;
			case SIZE_128MEG:
				lmem_max = (int)mc->base + (128 * A_MEG);
				actual_size = 128;
				break;
			default:
				break;
			}
		}

		/* set board control.
		 * without disturbing any other bits, clear any existing errs.
		 */

		cntrl = m_reg->mem_cntrl;
		cntrl &= ~EN_SYN_ERR;	/* to clear any pending error */
		m_reg->mem_cntrl = cntrl;   wbflush();

		set_mem_leds(m_reg,mc->leds);


		if (showconfig) {
			printf("Mem: slot %d:  %dM @ %dM base, vme-base %dM, vec 0x%x, pri %d, SN <%s>\n",
				slot,
				actual_size,
				((int)mc->base >> 20),
				((int)mc->vmebase >> 20),
				r3200memvec,
				(lmem_spl==SPL_7) ? 7 :
				    ((lmem_spl==SPL_3) ? 3 : 1),
				&mc->board_sn[0]);
		}
	logical_board++;
	}

	if (lmem_max > 0) {
		mem_test_max = (char *)lmem_max;
	}

}
/*
 * "Device" initialization.
 * Tell the memory boards an interrupt vector for ecc error interrupts.
 * Enable ecc reporting.
 */

void
r3200memedtinit(e)
struct edt *e;
{
	int		slot;		/* slot number during scan	*/
	unsigned	board;		/* board logical #		*/
	ushort		cntrl;		/* mem board control reg	*/
	ushort		a_conf;		/* address config reg		*/
	int		i;

	struct lmem_cntrl *mc;	/* per logical board control info	*/
	volatile struct r3250_regs *m_reg; /* mem cntrl/status space	*/

	/* scan through the local-bus slots looking for memory boards */

	for (board=0; board < R3200_MEM_MAX; board++) {

		mc = &(lmem_cntrl[board]);
		if (mc->configured != 1) {
			/* no board in this slot, just skip it */
			continue;
		}

		m_reg = (struct r3250_regs *) BOARD_ADDR(mc->slot);

		m_reg->mem_intr_vec = r3200memvec;

		/* set board control.
		 * without disturbing any other bits, clear any existing
		 * error, enable interrupts, set the priority to 7,
		 * and turn off any of the test features (inhibits).
		 */

		cntrl = m_reg->mem_cntrl;
		cntrl &= ~EN_SYN_ERR;	/* to clear any pending error */
		m_reg->mem_cntrl = cntrl;   wbflush();
		cntrl |= (EN_SYN_ERR | EN_INT_SERR | EN_INT_DERR);
		cntrl &= ~SPL_MASK;
		cntrl |= lmem_spl;
		cntrl &= ~(INH_ECC | INH_WRT_DATA | INH_WRT_CHK);
		m_reg->mem_cntrl = cntrl;   wbflush();
	}

#ifdef DEBUG
	/* start ECC test monitor routine */
	timeout(lmem_test,0,HZ);	
#endif /* DEBUG */
}

/* Look for, and handle, memory errors.
 * In this case, that means to scan the existing memory boards
 * and see if any have error conditions.
 * You have to do this in general because the things that signal
 * double-bit memory errors are non-maskable exceptions and
 * you don't have any idea whether there is any memory error or
 * where it might be.
 *
 * Single bit errors are logged, and the error condition cleared.
 * A count is kept of errors per board and errors per system.
 * If single-bit errors happen too frequently (too close proximity)
 * disable single-bit error interupts for the board and schedule
 * a timeout to re-enable the board after a bit.
 * The objective is to make the system and console useable
 * (e.g. to shut the system down) in the face of a persistent
 * single-bit error.
 *	
 * For the time being, log double bit errors and panic.
 * This scan is called from panic since a bus error or write address
 * error could be caused by a double-bit error.
 * Be careful not to recurse panic().
 */

void
lmem_err_scan()
{
	int	board;		/* logical board number			*/
	ushort	status;		/* copy of mem status register		*/
	ushort	cntrl;		/* copy of mem control register		*/
	struct lmem_cntrl *mc;	/* per logical board control info	*/
	volatile struct r3250_regs *m_reg; /* mem cntrl/status space	*/

	static short	lmem_double_err=0; /* set if double error ever seen */

	if (lmem_double_err != 0) {
		/* recursive error scan after fatal error, just return */
		return;
	}
	for (board=0; board < R3200_MEM_MAX; board++) {
		mc = &lmem_cntrl[board];
		if (mc->configured == 0) {
			continue;
		}
		m_reg = (struct r3250_regs *) BOARD_ADDR(mc->slot);
		status =  m_reg->mem_status;
		if ((status & (S_ERR | D_ERR)) == 0) {
			continue;
		}
		/* we have an error at this point */

		/* Light the LED first in case we get more errors
		 * trying to handle the error.
		 */
		mc->leds |= ECC_ERR_LED;
		set_mem_leds(m_reg,mc->leds);
		mc->errcount += 1;
		lmem_errcount +=1;
		if ((status & D_ERR) || ecc_noprint==0) {
			printf("Mem:  ");
			if (status & D_ERR) {
				printf("ECC DOUBLE-BIT error:  ");
				lmem_double_err = 1;
			} else if (status & S_ERR) {
				printf("ECC SINGLE-BIT error:  ");
			}
			printf("board %d, slot %d (backplane ID %d)\n",
				board, mc->slot+1, mc->slot);
			switch(mc->board_size) {
			case  SIZE_16MEG:
				printf("      %dMeg board,",16);
				break;
			case  SIZE_32MEG:
				printf("      %dMeg board,",32);
				break;
			case  SIZE_64MEG:
				printf("      %dMeg board,",64);
				break;
			case  SIZE_128MEG:
				printf("      %dMeg board,",128);
				break;
			}
			printf(" base addr 0x%x, serial # <%s>\n",
				mc->base, &mc->board_sn[0]);
			printf("      status reg 0x%x, Bank 0x%x %s, Syndrome 0x%x, err on %s bus access\n",
				status,
				(~status & BANK_MASK) >> BANK_SHIFT,
				(status  & MSR_BANK_DEC_0) ? "ODD" : "EVEN",
				(status & SYNDROME_MASK) >> SYNDROME_SHIFT,
				(status & BUS_ERR_ID) ? "PRIVATE" : "VME");
			printf("      Error %d for this board, %d for the system\n",
				mc->errcount, lmem_errcount);
		}
		/* ??? how about really logging the error ??? */
		cntrl = m_reg->mem_cntrl;
		cntrl &= ~EN_SYN_ERR;	/* to clear any pending error */
		m_reg->mem_cntrl = cntrl;   wbflush();
		/* check to see if error is second within a short interval */
		if (mc->errtime > (lbolt-(4*HZ))) {
			timeout(lmem_reenable,board,(10*HZ));
			/* don't reenable single-bit error interrupting */
			cntrl &= ~(EN_INT_SERR);
			cntrl |= (EN_SYN_ERR);
		} else {
			cntrl |= (EN_SYN_ERR | EN_INT_SERR | EN_INT_DERR);
		}
		m_reg->mem_cntrl = cntrl;   wbflush();
		mc->errtime = lbolt;
	}
	/* After we've looked at all the boards */
	if (lmem_double_err) {
		panic("double-bit error");
	}
}


/*
 * ECC error interrupt handler
 *
 * All memory cards share a single interrupt vector for simplicity
 * so look at all the cards and see where the error is.
 */
void
r3200memintr()
{
	lmem_err_scan();
}

/*
 * Re-enable single-bit error interrupts on a memory board.
 * Boards may have single-bit error reporting temporarily disabled
 * if errors happen "too quickly" (for instance they flood the console).
 * Re-enable reporting single bit errors at the expiration of a timeout.
 */

static void
lmem_reenable(board)
int board;
{
	ushort	status;
	ushort	cntrl;	
	struct lmem_cntrl *mc;	/* per logical board control info	*/
	volatile struct r3250_regs *m_reg; /* mem cntrl/status space	*/

	mc = &lmem_cntrl[board];
	if (mc->configured == 0) {
		printf("lmem_reenable: Board number %d is not a configured board\n",board);
		return;
	}
	m_reg = (struct r3250_regs *) BOARD_ADDR(mc->slot);

	lmem_err_scan(); 	/* this will take care of any pending error */

	cntrl = m_reg->mem_cntrl;
	cntrl |= (EN_SYN_ERR | EN_INT_SERR | EN_INT_DERR);
	m_reg->mem_cntrl = cntrl;   wbflush();
}

/* Set the memory board leds.
 * the board LEDs are 1=dark, 0=lit, so invert the bit pattern
 * that we are given.
 */

set_mem_leds(m_reg,led_val)
volatile struct r3250_regs *m_reg;	 /* mem cntrl/status space	*/
int	led_val;			/* led bit values: 1==ON, 0==OFF */
{
	m_reg->mem_leds = ~(led_val); wbflush();
}


#ifdef DEBUG
/* test aid
 * every now and then (like 10 seconds) wake up and look at
 * a couple global variables to see if we should create an
 * ECC error in memory.
 * If the single-bit address is nonzero,
 * force a single-bit error into that address
 * and clear the variable.
 * Similarly for the double-bit error.
 * Comment, prior to creating the error that we are going to do so.
 * At the end, reschedule execution of this routine.
 */

char	*r3200_single_err=0;	/* if nonzero, force single-bit err at addr */
char	*r3200_double_err=0;	/* if nonzero, force dbl-bit err at addr */
int	r3200_repeat_err=0;	/* if nonzero, repeat err at loc + 4 */


static void
lmem_test()
{
	int	s;

	s = splall();
	if (r3200_single_err) {
		printf("SET: 1-bit err at 0x%x\n",
			r3200_single_err);
		force_err(r3200_single_err,1);
		r3200_single_err = 0;
	}
	if (r3200_double_err) {
		printf("SET: 2-bit err at 0x%x\n",
			r3200_double_err);
		force_err(r3200_double_err,2);
		r3200_double_err = 0;
	}

	splx(s);
	timeout(lmem_test,0,HZ);	
}

static void
repeat_err(addr)
{
	force_err(addr,1);
}

/* read a location that should generate an error
 * called as the result of a timeout sometime after an
 * error has been written into the location.
 */
static void
lmem_readerr(addr)
int	addr;
{
	int	word;
	int	s;

	s = splall();
	word = * (int *)(PHYS_TO_K1(addr));
	printf("READ: loc 0x%x = 0x%x\n",
		addr,word);
	if (r3200_repeat_err > 0) {
		r3200_repeat_err -= 1;
		timeout(repeat_err,addr+4,15);
	}
	splx(s);
}


static void
force_err(addr,size)
int	addr;		/* physical address to force error */
int	size;		/* number of bits to change -- 1 or 2 */
{
	int	board;
	ushort	status;
	ushort	cntrl;	
	ushort	badcntrl;	
	struct lmem_cntrl *mc;	/* per logical board control info	*/
	volatile struct r3250_regs *m_reg; /* mem cntrl/status space	*/
	char	*limit;
	int	s;
	int	word;
	int	badword;

	for (board=0; board < R3200_MEM_MAX; board++) {
		mc = &lmem_cntrl[board];
		if (mc->configured == 0) {
			printf("r3300mem.c: force_errcan't locate addr 0x%x to force error\n",addr);
			return;
		}
		limit = mc->base + ((mc->board_size & SIZE_MASK) *1024*1024);
		if (addr >= (int)mc->base && addr < (int)limit) {
			m_reg = (struct r3250_regs *) BOARD_ADDR(mc->slot);


			cntrl = m_reg->mem_cntrl;
			
			badcntrl = cntrl & ~EN_SYN_ERR; /* clear err if one */
			badcntrl |= INH_WRT_CHK;	/* don't wrt new chk bits */
printf("brd %d (@ 0x%x) - get/set cntrl 0x%x 0x%x\n",
	board, m_reg, cntrl, badcntrl);
			
			s = splall();
			word = * (int *)(PHYS_TO_K1(addr));
			if (size == 2) {
				badword = word ^ 3;
			} else {
				badword = word ^ 1;
			}
			m_reg->mem_cntrl = badcntrl; wbflush();
			* (int *)(PHYS_TO_K1(addr)) = badword; wbflush();
			m_reg->mem_cntrl = cntrl; wbflush();
			splx(s);
			printf("mod word at 0x%x from 0x%x to 0x%x\n",
				addr,word,badword);
			timeout(lmem_readerr,addr,HZ*2);
			return;
		}
	}
	printf("r3200mem.c: force_err: can't locate addr 0x%x to force error\n",addr);
}
#endif /* DEBUG */
