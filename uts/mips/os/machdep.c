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
#ident	"$Header: machdep.c,v 1.65.1.17.1.5.1.4 91/01/04 16:59:48 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/sysmacros.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/cpu_board.h"
#include "sys/iop.h"
#include "sys/fpu.h"
#include "sys/systm.h"
#include "sys/reg.h"
#include "sys/buf.h"
#include "sys/map.h"
#include "sys/conf.h"
#include "sys/dump.h"
#include "sys/pfdat.h"
#include "sys/sysmips.h"
#include "bsd43/mips/hwconf.h"
#include "sys/cpu_board.h"
#include "posix/sys/signal.h"
#include "sys/ctlspace.h"
#include "sys/ucred.h"
#if defined(R6000) && defined(DEBUG)
#include "sys/bc.h"
#endif

extern int userstack[];

extern int LED_REG[];			/* Lboot'able addresses */

extern int showconfig;

/*	The following flags are used to indicate the cause
**	of a level 8 interrupt.
*/

char	iswtchflag;

/*	The following flags are used to indicate the
**	cause of a level 9 (PIR) interrupt.
*/

char	timeflag;
char	uartflag;
char	pwrflag;

/*	The follow access various locations in the firmware
**	vector table or edt.
**
**	The number of entries in the edt.
*/

#define	VNUM_EDT	(*(((struct vectors *)VBASE)->p_num_edt))

/*	A pointer to the start of the edt.
*/

#define	VP_EDT		(((struct vectors *)VBASE)->p_edt)

/*	A pointer to an entry in the edt.
*/

#define	V_EDTP(X)	(VP_EDT + (X))


/*
 * coprocessor revision identifiers
 */
unsigned cputype_word;
unsigned fptype_word;

#ifdef R6000_BUG_FPINTR
unsigned r6000_bug_fpintr;		/* nonzero means needs workaround */
#endif R6000_BUG_FPINTR

union rev_id {
	unsigned int	ri_uint;
	struct {
#ifdef MIPSEB
		unsigned int	Ri_fill:16,
				Ri_imp:8,		/* implementation id */
				Ri_majrev:4,		/* major revision */
				Ri_minrev:4;		/* minor revision */
#endif MIPSEB
#ifdef MIPSEL
		unsigned int	Ri_minrev:4,		/* minor revision */
				Ri_majrev:4,		/* major revision */
				Ri_imp:8,		/* implementation id */
				Ri_fill:16;
#endif MIPSEL
	} Ri;
};
#define	ri_imp		Ri.Ri_imp
#define	ri_majrev	Ri.Ri_majrev
#define	ri_minrev	Ri.Ri_minrev

/*
 * For the entries that have the same implementation id, the chips are 
 * differentiated by the maj rev field.
 *
 * So currently in these tables, The R2000A and the R3000 have the same
 * implementation number, but the R2000A  has a majrev of 1, R3000 has a 
 * majrev of 2 and R3000A has a majrev of 3.
 */

struct rimp_tbl {
	char *it_name;
	unsigned it_imp;
	unsigned it_majrev;
};

struct rimp_tbl cpu_imp_tbl[] = {
	{ "MIPS R2000 Processor Chip",			0, 0 },
	{ "MIPS R2000 Processor Chip",			1, 0 },
	{ "MIPS R2000A Processor Chip",			2, 1 },
	{ "MIPS R3000 Processor Chip",			2, 2 },
	{ "MIPS R3000A Processor Chip",			2, 3 },
	{ "MIPS R6000 Processor Chip",			3, 0 },
	{ 0,						0, 0 }
};

struct rimp_tbl fp_imp_tbl[] = {
	{ "MIPS R2360 Floating Point Board",		1, 0 },
	{ "MIPS R2010 VLSI Floating Point Chip",	2, 0 },
	{ "MIPS R2010A VLSI Floating Point Chip",	3, 1 },
	{ "MIPS R3010 VLSI Floating Point Chip",	3, 2 },
	{ "MIPS R3010A VLSI Floating Point Chip",	3, 3 },
	{ "MIPS R3010A VLSI Floating Point Chip",	3, 4 },
	{ "MIPS R6010 Floating Point Chip",		4, 0 },
	{ 0,						0, 0 }
};

int cpu_config = 0;

coproc_find()
{
	char *rimp_name();
	union rev_id ri;
	extern char mfc0_start[], mfc0_end[];

	ri.ri_uint = cputype_word = get_cpu_irr();
	if (ri.ri_imp == 0) {
		ri.ri_majrev = 1;
		ri.ri_minrev = 5;
	}
	printf ("CPU: %s Revision: %d.%d\n",
	    rimp_name(ri, cpu_imp_tbl),
	    ri.ri_majrev, ri.ri_minrev);

#ifdef R6000_BUG_FPINTR
	r6000_bug_fpintr = IS_R6300 && (ri.ri_majrev == 2);
#endif R6000_BUG_FPINTR

#ifdef NO_LONGER_NEEDED
/*
 * Make sure the PROBE_BUG option and the mfc0 assembler option
 * are turned on for the old 1.5 chips.
 */
	if ((IS_R2300 || IS_R2400) && (ri.ri_majrev < 2)) {
#ifndef PROBE_BUG
		cmn_err(CE_PANIC,"Kernel must be compiled with -DPROBE_BUG for 1.5 revision cpu chip");
#endif !PROBE_BUG
		if ((mfc0_end - mfc0_start) <= 4)
			cmn_err(CE_PANIC,"Kernel must be assembled with -Wb,-mfc0 for 1.5 revision cpu chip");
	}
#endif NO_LONGER_NEEDED
	
/*
 * TODO:
 *	check cpu_config register
 *	print a message about vme or local memory
 */
	if (IS_R2300)
		cpu_config = *(char *)PHYS_TO_K1(CPU_CONFIG);
	else if (IS_R2400)
		cpu_config = *(short *)PHYS_TO_K1(SCR);
	else if (IS_M2000_ARCH)
		cpu_config = *(int *)PHYS_TO_K1(CPU_CR_M2000);
	else if (IS_M6000)
		cpu_config = CONFIG_POWERUP
			   | ((get_fpc_irr()) ? 0 : CONFIG_NOCP1);
	else if (IS_R3030) {
		extern	int	simms_type;	/* defined in mem_r3030.c */
		/*
		 * things set up in configuration are
		 *	BIT	MEANING
		 *	0	R3010 present
		 *	1	MONO installed
		 *	2	COLOUR installed
		 *	3	Floppy disk installed
		 *	4,5	Digiboard (0=none, 1=8, 2=16, 3=??)
		 *	6	4Mb rams (0 = 1Mb Rams)
		 */

		cpu_config |= simms_type;	/* size of simms installed */
#if 0
		if (!pkbd_3010())
			cpu_config |= P_NO_FPU;
#endif
#if 0
		if (floppy_present())
			cpu_config |= P_FLOPPY;
#endif
		switch (is_digi()) {
		case 8:
			cpu_config |= P_DIG8;
			break;
		case 16:
			cpu_config |= P_DIG16;
			break;
		}
		cpu_config |= ((((physmem * NBPC) / 0x800000) - 1) << P_MEM_SHIFT);
	} else if (IS_I2000) {
#  ifdef notdef
		extern int enable_fp;
		
		cpu_config = CONFIG_NOCP2|CONFIG_POWERUP;
		/*
		 * 22-Aug-1988 On the pass1 boards there is a
		 * layout problem that prevents the use of
		 * the fp chip.  Some boards have been
		 * modified and enable_fp can be set when
		 * booting the kernel.  The pass 1 boards
		 * have *fp_cond and *fp_present lines tied
		 * together.  If a chip is present the
		 * *fp_preset line interferes with the
		 * *fp_cond line causing anomalous behavior.
		 * enable_fp can be removed when the second
		 * pass boards are ready.
		 */
		if (enable_fp) {
			if (cpu_status_reg() & FPU_Not_Present)
			  cpu_config |= CONFIG_NOCP1;
		}
	else
		  cpu_config |= CONFIG_NOCP1;
#  else
		cpu_config = *(char *)PHYS_TO_K1(IOP_CMDREG);

#  endif /* notdef */
	} else {
		cmn_err(CE_PANIC,"Bad machine type\n");
	}

	if ( (IS_R2300 && (cpu_config & CONFIG_NOCP1)) ||
	     (IS_R2400 && (cpu_config & SCR_NOFPP)) ||
	     (IS_M2000_ARCH && (cpu_config & CR_FPPRES)) ||
	     (IS_M6000 && (cpu_config & CONFIG_NOCP1)) ||
	    (IS_I2000 && (cpu_config & FPU_Not_Present)) ||
	     (IS_R3030 && (cpu_config & P_NO_FPU)))
		fptype_word = 0;
	else {
		ri.ri_uint = fptype_word = get_fpc_irr();
		fptype_word &= IRR_IMP_MASK;
		printf("FPU: %s Revision: %d.%d\n",
		    rimp_name(ri, fp_imp_tbl),
		    ri.ri_majrev, ri.ri_minrev);
		fp_init();
	}
}

char *
rimp_name(ri, itp)
struct rev_id ri;
struct rimp_tbl *itp;
{

	for (; itp->it_name; itp++) {
		if (itp->it_imp == ri.ri_imp) {
		   switch (itp->it_majrev) {
		   case 0: 
			   break;
		   case 1:
		   case 2:
		   case 3:
			   itp += (ri.ri_majrev - 1);
			   break;
		   default:
			   return("Unknown implementation");
		   }

		   return(itp->it_name);
		}
         }
	 return("Unknown implementation");
}

fp_init()
{
	int led;

	if (IS_R2300 || IS_M2000_ARCH) {
		*(char *)PHYS_TO_K1(MACHDEP(LED_REG)) = -1 &~ LED_FPBD_RUN;
		wbflush();
		DELAY(10);
		*(char *)PHYS_TO_K1(MACHDEP(LED_REG)) = -1;
		wbflush();
		DELAY(10);
		set_fpc_csr(0);
		if (fptype_word == IRR_IMP_R2360)
		    set_fpc_led(-1);
	} else if (IS_R2400 || IS_M6000 || IS_I2000 || IS_R3030) {
		set_fpc_csr(0);
	}
}

/*
 * Start clock
 * This routine must run at IPL15
 */

clkstart()
{
	startrtclock();		/* Start Real time clock. */
#ifdef KGCLOCK
	startkgclock();		/* Start kernel profiling clock. */
#endif
}

/*
 * Adjust the time to UNIX based time
 * called when the root file system is mounted with the time
 * obtained from the root. Call inittodr to try to get the actual 
 * time from the battery backed-up real-time clock.
 */

clkset(oldtime)
register time_t	oldtime;
{
	time.tv_sec = (unsigned)oldtime;
	inittodr(time.tv_sec);
}

/*
 * Stop the clock.
 * This routine must run at IPL15
 */

clkreld()
{

	/*	Stop the interval timer.
	*/

	stopclocks();
}

/*
 */

timepoke()
{
	setsoftclock();
}

#ifdef SABLE

#define ASYNCDISK 1
#ifdef ASYNCDISK
extern struct buf sdtab;
extern int sdserv;
#endif

#endif SABLE

volatile int idleflag;

unsigned int pzerocnt;		/* number of times pages have been cleared */
extern int _riscos_idle_clear_pages;
extern int page_allocation_count;

idle()
{
	register long		*longaddr, *pgend;
	register struct pfdat	*pfd;
	register int		old_page_allocation_count;
	register int		s;
	register int		i;

	idleflag = 1;
#ifdef ASYNCDISK
	if(sdserv)
		sdservice();
#endif ASYNCDISK
	spl0();
	while(idleflag) {
		bump_leds();
#ifdef MIPS_LOCAL
		if (freemem == 0) {
			static int last_time = 0;
			/* Don't print message too often. */
			if (time.tv_sec >= (last_time+5)) {
				printf("idle: Yow! freemem=0\n");
				last_time = time.tv_sec;
			}
		}
#endif MIPS_LOCAL
		if (_riscos_idle_clear_pages == 0 /* enabled */) {
			/* stop if all free pages have already been cleared
			 * 		or
			 * stop if the rest of the free pages are hashed
			 */
			s = splhi();
			old_page_allocation_count = page_allocation_count;
			pfd = pclearp->pf_next;
			if ((pfd == &phead) || (pfd->pf_flags & P_HASH))
				continue;
			/* bzero(phystokv(pfdattopfn(pfd)<<PNUMSHFT), NBPP); */
			longaddr= (long *)phystokv(pfdattopfn(pfd)<<PNUMSHFT);
			pgend = longaddr + (NBPP/sizeof(long));
	
			i = 0;
			do {
				*longaddr = 0;
				if (i > 16) {
					splx(s);
					if (idleflag == 0 ||
					    page_allocation_count !=
						old_page_allocation_count)
						break;
					s = splhi();
					i = 0;
				};
			} while (++longaddr != pgend);
	
			if (longaddr == pgend) {
				pfd->pf_flags |= P_CLEARED;
				pclearp = pfd;
				pclearstat.p_cnt++;
			}
			splx(s);
		};
	}
}

sendsig (p, sig, is_sigset, sig_type)
	int (*p), sig, is_sigset, sig_type;
{
	register struct sigcontext *scp;
	register struct pregion *prp;
	register int *srp, *frp;
	register uint *urp;
	register regbit, regmask;
	struct sigcontext sc;

	/*
	 * See if region at SP can hold the sigcontext structure.
	 * If not, try to grow the stack.
	 */
	scp = ((struct sigcontext *)(USER_REG(EF_SP)) - 1);
	scp = (struct sigcontext *)((int)scp & ~15);	/* Quad word aligned. */
	prp = findreg (u.u_procp, scp);
	if (prp)  {
		regrele(prp->p_reg);
	}
	else if (!grow(scp)) {
		return(0);
	}

	/* Check for write access */
	if (!useracc((caddr_t)scp, sizeof (*scp), B_READ)) {
		/* Process has trashed its stack; can't send a signal. */
		return(0);
	}
	/*
	 * The amount of state to save depends on the context in which
	 * we are handling the signal.  From a trap(), the user is in an
	 * indeterminant state, so we must save everything.  From a syscall,
	 * only callee-save must be protected.  A SIGRET syscall is special 
	 * because it looks like a syscall but returns to the middle of
	 * ordinary user code so we must save all user registers.
	 */
	switch (sig_type) {
	case SIG_TRAP:
		regmask = scp->sc_regmask = 0xffffffff;	
		break;
	case SIG_SYSCALL:
		regmask = scp->sc_regmask = 0xfcff00fe;	
		break;
	case SIG_SIGRET:
		regmask = scp->sc_regmask = 0xffffffff;	
		break;
	default:
		cmn_err(CE_PANIC, "psig: Bad sig_type value.");
		break;
	}

	scp->is_sigset = is_sigset;
	scp->sc_pc = USER_REG(EF_EPC);
	scp->sc_regs[0] = 0;
	for (urp = &USER_REG(EF_AT), srp = &scp->sc_regs[1], regbit = 1<<1;
	    urp <= &USER_REG(EF_RA);
	    urp++, srp++, regbit <<= 1)
		if (regmask & regbit)
			*srp = *urp;
	scp->sc_mdhi = USER_REG(EF_MDHI);
	scp->sc_mdlo = USER_REG(EF_MDLO);
	scp->sc_ownedfp = u.u_pcb.pcb_ownedfp;
	if ((regmask & 1) && u.u_pcb.pcb_ownedfp) {
		checkfp (u.u_procp, 0);	/* dump fp to pcb */
		for (srp = &scp->sc_fpregs[0], frp = u.u_pcb.pcb_fpregs;
		    frp < &u.u_pcb.pcb_fpregs[32]; srp++, frp++)
			*srp = *frp;
		scp->sc_fpc_csr = u.u_pcb.pcb_fpc_csr;
		scp->sc_fpc_eir = u.u_pcb.pcb_fpc_eir;
		u.u_pcb.pcb_fpc_csr &= ~FPCSR_EXCEPTIONS;
	}
	scp->sc_cause = USER_REG(EF_CAUSE);
	scp->sc_badvaddr = USER_REG(EF_BADVADDR);

	/*
	 * setup register to enter signal handler
	 * when resuming user process
	 */
	USER_REG(EF_A0) = sig;
	if (sig == SIGFPE || sig == SIGSEGV || sig == SIGILL ||
	    sig == SIGBUS || sig == SIGTRAP) {
		USER_REG(EF_A1) = u.u_code;
		u.u_code = 0;
	} else
		USER_REG(EF_A1) = 0;
	USER_REG(EF_A2) = (unsigned)scp;
	USER_REG(EF_SP) = (unsigned)scp;
	USER_REG(EF_A3) = (unsigned)p;
	USER_REG(EF_EPC) = (unsigned)u.u_sigtramp;
	return(1);
}


#define	R_ZERO	0
#define	R_AT	1
#define	R_V0	2
#define	R_A0	4
#define	R_A1	5
#define	R_A2	6
#define	R_A3	7
#define	R_SP	29

#define	STACK_ALIGN(x)	((unsigned)(x) &~ ((4*sizeof(int))-1))

bsd_sendsig(p, sig, signalmask)
int (*p)(), sig, signalmask;
{
	register struct bsd43_sigcontext *scp;
	register struct pregion *prp;
	register int *srp;
	register u_int *urp;
	register int *frp;
	int oonstack;

	ASSERT((u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) == \
	       		BSD43_U_SF_BSD43_SIGCONTEXT);


	XPRINTF(XPR_SIGNAL,"enter sendsig %d, 0x%x",sig,signalmask,0,0);
	oonstack = u.u_onstack;

	/*
	 * decide which stack signal is to be taken on, and make
	 * sure its aligned and accessable.
	 */
	if (!u.u_onstack && (u.u_sigonstack & bsd43_sigmask(sig))) {
		scp = (struct bsd43_sigcontext *)STACK_ALIGN(u.u_sigsp) - 1;
		u.u_onstack = 1;
	} else
		scp = (struct bsd43_sigcontext *)STACK_ALIGN(USER_REG(EF_SP)) - 1;

	prp = findreg (u.u_procp, scp);
	if (prp)  {
		regrele(prp->p_reg);
	}
	else if (!grow(scp)) {
		return(0);
	}

	/* Check for write access */
	if (!useracc((caddr_t)scp, sizeof (*scp), B_READ)) {
		/* Process has trashed its stack; can't send a signal. */
		return(0);
	}
	/*
	 * Save into the sigcontext the signal state and that portion
	 * of process state that we trash in order to transfer control
	 * to the signal trampoline code.
	 * The remainder of the process state is preserved by
	 * signal trampoline code that runs in user mode.
	 */
	scp->sc_onstack = oonstack;
	scp->sc_mask = signalmask;
	scp->sc_pc = USER_REG(EF_EPC);
	scp->sc_regs[R_V0] = USER_REG(EF_V0);
	scp->sc_regs[R_A0] = USER_REG(EF_A0);
	scp->sc_regs[R_A1] = USER_REG(EF_A1);
	scp->sc_regs[R_A2] = USER_REG(EF_A2);
	scp->sc_regs[R_A3] = USER_REG(EF_A3);
	scp->sc_regs[R_SP] = USER_REG(EF_SP);

	/*
	 * if this process has ever used the fp coprocessor, save
	 * its state into the sigcontext
	 */
	scp->sc_ownedfp = u.u_pcb.pcb_ownedfp;
	if (u.u_pcb.pcb_ownedfp) {
		checkfp(u.u_procp, 0);	/* dump fp to pcb */
		for (srp = &scp->sc_fpregs[0], frp = u.u_pcb.pcb_fpregs;
		    frp < &u.u_pcb.pcb_fpregs[32]; srp++, frp++)
			*srp = *frp;
		scp->sc_fpc_csr = u.u_pcb.pcb_fpc_csr;
		scp->sc_fpc_eir = u.u_pcb.pcb_fpc_eir;
		u.u_pcb.pcb_fpc_csr &= ~FPCSR_EXCEPTIONS;
	}
	scp->sc_cause = USER_REG(EF_CAUSE);
	scp->sc_badvaddr = USER_REG(EF_BADVADDR);

	/*
	 * setup registers to enter signal handler
	 * when resuming user process
	 */
	USER_REG(EF_A0) = sig;
	if (sig == SIGFPE || sig == SIGSEGV || sig == SIGILL ||
	    sig == SIGBUS || sig == SIGTRAP) {
		USER_REG(EF_A1) = u.u_code;
		u.u_code = 0;
	} else
		USER_REG(EF_A1) = 0;
	USER_REG(EF_A2) = (unsigned)scp;
	USER_REG(EF_A3) = (unsigned)p;
	USER_REG(EF_SP) = STACK_ALIGN(scp);
	USER_REG(EF_EPC) = (unsigned)u.u_sigtramp;
	XPRINTF(XPR_SIGNAL,"exit sendsig %d, 0x%x",sig,signalmask,0,0);
	return(1);

#if later
bad:
	uprintf("sendsig: can't grow stack, pid %d, proc %s\n", 
		u.u_procp->p_pid, u.u_comm);
	/*
	 * Process has trashed its stack; give it an illegal
	 * instruction to halt it in its tracks.
	 */
	u.u_signal[SIGILL] = SIG_DFL;
	sig = bsd43_sigmask(SIGILL);
	u.u_procp->p_sigignore &= ~sig;
	u.u_procp->p_sigcatch &= ~sig;
	u.u_procp->p_hold &= ~sig;
	psignal(u.u_procp, SIGILL);
#endif
}

sigreturn ()
{
	register struct sigcontext *scp;
	register int *srp, *frp;
	register uint *urp;
	register regbit, regmask;

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_BSD43_SIGCONTEXT:
	  	return(bsd_sigreturn());
	case BSD43_U_SF_POSIX_SIGCONTEXT:
		return(posix_sigreturn());
	default:
		break;
	};
	scp = (struct sigcontext *)u.u_ap[0];
	/* Check for read access */
	if (!useracc ((caddr_t)scp, sizeof (*scp), B_WRITE))
		return;
	u.u_eosys = FULLRESTORE;
	regmask = scp->sc_regmask;
	USER_REG(EF_EPC) = scp->sc_pc;
	for (urp = &USER_REG(EF_AT), srp = &scp->sc_regs[1], regbit = 1<<1;
	    urp <= &USER_REG(EF_RA);
	    urp++, srp++, regbit <<= 1)
		if (regmask & regbit)
			*urp = *srp;
	
	if (IS_M6000) {
		/* always put a good SBC IVectMask value on the stack */
		USER_REG(EF_IVECTMASK) = CSR_IVECTMASK_NONE;
	}
	USER_REG(EF_MDHI) = scp->sc_mdhi;
	USER_REG(EF_MDLO) = scp->sc_mdlo;
	/*
	 * unless bit one of regmask is set we don't restore coprocessor
	 * state.
	 */
	if ((regmask & 1) && u.u_pcb.pcb_ownedfp) {
		checkfp (u.u_procp, 1);	/* toss current fp contents */
		for (frp = u.u_pcb.pcb_fpregs, srp = &scp->sc_fpregs[0];
		    frp < &u.u_pcb.pcb_fpregs[32]; frp++, srp++)
			*frp = *srp;
		u.u_pcb.pcb_fpc_csr = scp->sc_fpc_csr & ~FPCSR_EXCEPTIONS;
	}

	/*
	 * If we came from a sigset() then we must call sigrelse()
         * to release any impending signals.
	 */
	if (scp->is_sigset) {
		/*
		 * The flag is_sigset contains the arg to be passed
                 * to sigrelse() if we came from a sigset() signal
		 */
		u.u_arg[0] = scp->is_sigset;
		ssig();
	}
}


bsd_sigreturn()
{
	register int *srp;
	register u_int *urp;
	register int *frp;
	struct a {
		struct bsd43_sigcontext *sigcontextp;
	} *uap = (struct a *)u.u_ap;
	register struct bsd43_sigcontext *scp;

	scp = uap->sigcontextp;
	/* Check for read access */
	if (!useracc((caddr_t)scp, sizeof (*scp), B_WRITE))
		return;
	u.u_eosys = FULLRESTORE;
	u.u_onstack = scp->sc_onstack & 01;
	u.u_procp->p_hold =
	    scp->sc_mask &~ (bsd43_sigmask(SIGKILL)|
			     bsd43_sigmask(BSD43_SIGCONT)|
			     bsd43_sigmask(BSD43_SIGSTOP));
	/*
	 * copy entire user process state from sigcontext into
	 * exception frame, a special exit from syscall insures
	 * that the entire exception frame gets restored
	 */
	USER_REG(EF_EPC) = scp->sc_pc;
	USER_REG(EF_MDLO) = scp->sc_mdlo;
	USER_REG(EF_MDHI) = scp->sc_mdhi;
	for (urp = &USER_REG(EF_AT), srp = &scp->sc_regs[R_AT];
	    urp <= &USER_REG(EF_RA); urp++, srp++)
			*urp = *srp;
	if (IS_M6000) {
		/* always put a good SBC IVectMask value on the stack */
		USER_REG(EF_IVECTMASK) = CSR_IVECTMASK_NONE;
	}
	if (u.u_pcb.pcb_ownedfp) {
		checkfp(u.u_procp, 1);	/* toss current fp contents */
		for (frp = u.u_pcb.pcb_fpregs, srp = &scp->sc_fpregs[0];
		    frp < &u.u_pcb.pcb_fpregs[32]; frp++, srp++)
			*frp = *srp;
		u.u_pcb.pcb_fpc_csr = scp->sc_fpc_csr & ~FPCSR_EXCEPTIONS;
	}
}

/*
 * Clear registers on exec
 */

setregs()
{
	register int	i, *p_int;
	void (* *rp)();
	register int nc;

	u.u_procp->p_chold = 0;	/* clear p_chold */

	/* Any pending signal remain held, so don't clear p_hold and
	p_sig */	

	/* If the action was to catch the signal, then the action
	must be reset to SIG_DFL */

	for (nc = 1; nc <= MAXSIG; nc++)
		if (u.u_procp->p_sigcatch & bsd43_sigmask(nc)) {
			u.u_procp->p_sigcatch &= ~bsd43_sigmask(nc);
			u.u_signal[nc - 1] = SIG_DFL;
		};

	/*
	 * Reset stack state to the user stack.
	 * Clear set of signals caught on the signal stack.
	 */
	u.u_onstack = 0;
	u.u_sigsp = 0;
	u.u_sigonstack = 0;
	u.u_sigintr = 0;
	u.u_sig_flag &= ~(BSD43_U_SF_SIGCONTEXT_SELECT);

	for (p_int = &u.u_ar0[EF_AT]; p_int < &u.u_ar0[EF_IVECTMASK];)
		*p_int++ = 0;
	u.u_ar0[EF_FP] = 0;
	u.u_ar0[EF_RA] = 0;
	u.u_ar0[EF_MDLO] = 0;
	u.u_ar0[EF_MDHI] = 0;
	u.u_ar0[EF_EPC] = u.u_exdata.ux_entloc;
	for (p_int = u.u_pcb.pcb_fpregs; p_int < &u.u_pcb.pcb_fpregs[32];)
		*p_int++ = 0;
	u.u_pcb.pcb_fpc_csr = 0;
	u.u_pcb.pcb_fpc_eir = 0;
	u.u_pcb.pcb_ownedfp = 0;
	u.u_pcb.pcb_sstep = 0;
	u.u_pcb.pcb_ssi.ssi_cnt = 0;

	for (i=0; i<v.v_nofiles; i++) {
		if ((u.u_pofile[i] & EXCLOSE) && u.u_ofile[i] != NULL) {
			/* Release all System-V style record locks, if any */
			(void) vno_lockrelease(u.u_ofile[i]);
			closef(u.u_ofile[i]);
			u.u_ofile[i] = NULL;
			u.u_pofile[i] = 0;
		}
		u.u_pofile[i] &= ~UF_MAPPED;
	};

}

/* Allocate 'size' units from the given map,
 * returning the base of an area which starts on a "mask" boundary.
 * That is, which has all of the bits in "mask" off in the starting
 * address.  Mask is assummed to be (2**N - 1).
 * Algorithm is first-fit.
 */

ptmalloc(mp, size, mask)
struct map *mp;
{
	register int a, b;
	register int gap, tail;
	register struct map *bp;

	ASSERT(size >= 0);
	for (bp = mapstart(mp); bp->m_size; bp++) {
		if (bp->m_size >= size) {
			a = bp->m_addr;
			b = (a+mask) & ~mask;
			gap = b - a;
			if (bp->m_size < (gap + size))
				continue;
			if (gap != 0) {
				tail = bp->m_size - size - gap;
				bp->m_size = gap;
				if (tail) 
					mfree(mp, tail, bp->m_addr+gap+size);
			} else {
				bp->m_addr += size;
				if ((bp->m_size -= size) == 0) {
					do {
						bp++;
						(bp-1)->m_addr = bp->m_addr;
					} while ((bp-1)->m_size = bp->m_size);
					mapsize(mp)++;
				}
			}
			ASSERT(bp->m_size < 0x80000000);
			return(b);
		}
	}
	return(0);
}

/*
 * Returns address of pde to access vaddr in proc procno
 *	Finds region in that proc and returns address of pde's
 *	or 0 if bad.
 */

#define EBADDR	0

extern struct proc *curproc;

pde_t *
vtop(vaddr,procno)
struct proc *procno;
register long vaddr;
{
	register paddr_t	retval;
	register pde_t		*pd;
	register reg_t	*rp;
	register preg_t	*prp;
	register unsigned tmp;

	ASSERT((unsigned)vaddr < (unsigned)K0SEG);

	/*	Lock the region containing the page that faulted.
	 */

	if(( prp = findreg(procno, vaddr))==NULL)
		return(0);

	rp = prp->p_reg;

	/*
	 *	Find the physical address of the pde.
	 * 	The physical pde's are found through the region's r_list.
	 *	Could optimize here for current process by
	 *	allowing access to kpteseg from outside utlbmiss.
	 */

	tmp = btotp(vaddr - (int)prp->p_regva);
	pd = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);
	regrele(rp);
	return(pd);
}


#ifdef NOTDEF /* XXX - later */
devcheck(devtype, dev_addr)
int devtype;
paddr_t dev_addr[];
{
	int dev_cnt;
	int i;
 
	dev_cnt = 0;
	for (i=0; i < VNUM_EDT; i++)
	{
		if (V_EDTP(i)->opt_code == devtype)
			dev_addr[dev_cnt++] =
			   (paddr_t)((17 * V_EDTP(i)->opt_slot - 14) * NBPS);
	}

	return(dev_cnt);
}

unsigned char
getvec(baddr)
register long baddr;
{
	/*	Simulate the system routine that will supply the
	**	interrupt vector given a peripheral board address.
	*/

	return(((((baddr / 131072) - 3) / 17) + 1) << 4);
}
#endif

/*
 * XXX 
 * Some stubs that I don't want to put in cf (to make them more visable
 * that they have to be added.  Most should be put in ml 
 */

bzeroba(dst, count)
int dst, count;
{
	bzero(dst, count);
}

/*
** this file used to implement the upath() and spath() routines
** these have been replaced by the copyinstr() and copystr() routines
** that were in assembly code in NFS4.0
** NOTE: these should be recoded for speed
*/

/*
** copystr( from, to, maxlen, lencopied )
** caddr_t	from
** caddr_t	to
** int		maxlen
** int		*lencopied
**
**	Copy a null terminated string from one point to another in
**	the kernel address space
**
**	INPUT:
**		maxlen - the maximum length of the string to by copied,
**			 including the null character
**	OUTPUT:	
**		lencopied - actual number of characters copied, including
**				the null terminator; the one exception 
**				is the null string, for which 0 is returned
**	RETURNS:
**		EFAULT - if supplied address was not valid
**		ENAMETOOLONG - if pathname is > maxlen - 1
**		0  - otherwise
*/
copystr( from, to, maxlen, lencopied )
caddr_t	from;
caddr_t	to;
int	maxlen;
int	*lencopied;
{
	register int		ch;
	register caddr_t	st = from;

	if ( maxlen <= 0 ) {
		*lencopied = 0;
		return ( ENAMETOOLONG );
	}

	if ( *st == 0 ) {
		*to = 0;
		*lencopied = 0;
		return ( 0 );
	}

	for ( ; *st; st++ )
		;

	if ( st - from + 1 > maxlen ) {
		*lencopied = 0;
		return ( ENAMETOOLONG );
	}
	for ( st = from ; *to = *from; to++, from++ )
		;
	*lencopied = from - st + 1;
	return ( 0 );
}

/*
** copyinstr( from, to, maxlen, lencopied )
** caddr_t	from
** caddr_t	to
** int		maxlen
** int		*lencopied
**
**	Read in a pathname from user space.
**	INPUT:
**		maxlen - the maximum length of the string to by copied,
**			 including the null character
**	OUTPUT:	
**		lencopied - actual number of characters copied, including
**				the null terminator; the one exception 
**				is the null string, for which 0 is returned
**	RETURNS:
**		EFAULT - if supplied address was not valid
**		ENAMETOOLONG - if pathname is > maxlen 
**		0  - if null pathname was specified
*/
copyinstr( from, to, maxlen, lencopied )
caddr_t	from;
caddr_t	to;
int	maxlen;
int	*lencopied;
{
	register int		ch,
				cnt;

	if ( maxlen <= 0 ) {
		*lencopied = 0;
		return ( ENAMETOOLONG );
	}

	if ( ( ch = fubyte( from ) ) == 0 ) {
		*to = 0;
		*lencopied = 0;
		return ( 0 );
	}

	if ( ch < 0 ) {
		*lencopied = 0;
		return ( EFAULT );
	}

	for ( cnt = 1; ( ch = fubyte( from ) ) > 0 ; cnt++, to++, from++ )
	{
		if ( cnt > maxlen - 1 ) {
			*lencopied = cnt - 1;
			return ( ENAMETOOLONG );
		}
		*to = (unchar)ch;
	}
	if ( ch == 0 )
	{
		if ( cnt > maxlen ) {
			*lencopied = cnt - 1;
			return ( ENAMETOOLONG );
		}
		*to = (unchar)ch;
		*lencopied = cnt;
		return ( 0 );
	}
	*lencopied = cnt - 1;
	return ( EFAULT );
}

min(a,b)
{
	return( (a<b) ? (a):(b) );
}

max(a,b)
{
	return( (a>b) ? (a):(b) );
}

/* Instead of porting this whole routine now, I'll just flush the
   entire cache on each call to this routine */
#ifdef TODO
vflush(addr, bcnt)
register unsigned addr, bcnt;
{
	register struct pte *pte;
	register unsigned rem;
	register unsigned k0addr;
	extern struct pte eSysmap[];

	for (; bcnt; bcnt -= rem) {
		;
		/*
		 * calculate bytes to flush in this page
		 */
		rem = NBPG - (addr & PGOFSET);
		if (rem > bcnt)
			rem = bcnt;

		/*
		 * calculate appropriate physical address
		 */
		if (IS_KSEG0(addr))
			k0addr = addr;
		else if (IS_KSEG1(addr))
			k0addr = K1_TO_K0(addr);
		else if (IS_KUSEG(addr)) {
			pte = vchktopte(u.u_procp, btop(addr));
			if (!pte || !pte->pg_v) {
			/*
				 * can't resolve this to a physical address now
				 * so just flush entire cache and give up
				 */
				flush_cache();
				return;
			}
			/*
			 * interpretation of pg_v and pg_fod:
			 *	v && !fod	-> mapped to "real" memory
			 *	v && fod	-> mapped to bitmaps, etc
			 *			   (don't put in free page
			 *			    pool on memfree's)
			 *	!v && !fod	-> reclaimable
			 *	!v && fod	-> requires io or bzero
			 *			   (which will do a flush
			 *			    so skip it here)
			 */
			if (pte->pg_n)
				continue;
			k0addr = PHYS_TO_K0(ptob(pte->pg_pfnum))
			    | (addr & PGOFSET);
		} else if (IS_KSEG2(addr)) {
			pte = &Sysmap[btop(addr - K2BASE)];
			if (pte >= eSysmap)
				cmn_err(CE_PANIC, "vflush");
			if (!pte->pg_v)
				cmn_err(CE_PANIC, "vflush3");
			if (pte->pg_n)
				continue;
			k0addr = PHYS_TO_K0(ptob(pte->pg_pfnum))
			    | (addr & PGOFSET);
		} else {
			flush_cache();
			return;
		}
		clean_cache(k0addr, rem);
	}
}
#else
vflush(addr, bcnt)
register unsigned addr, bcnt;
{
	flush_cache();
}
#endif TODO

newptes(p, vpn, size)
struct proc *p;
unsigned vpn, size;
{
	if(p->p_tlbpid == -1)
		return;
#ifdef R6000
	/*
	 * For the R6000 we would need to invalidate vpn in the primary
	 * and find its physical page to invalidate the secondary cache.
	 * Its faster, then, to invalidate all of this process's vtags
	 * (and tlb entries) by bumping its pid. Since newptes can be
	 * called repeatedly by the page daemon, we keep from spinning
	 * cur_tlbpid too rapidly by only calling new_tlbpid if we are
	 * u.u_procp.
	 */
	if (p != u.u_procp)
		p->p_tlbpid = -1;
	else
		new_tlbpid(p);
#else
	if (size > 7)
		new_tlbpid(p);
	else  {
		int s = splall();

		while(size > 0) {
			unmaptlb(p->p_tlbpid, vpn++);
			size--;
		}

		splx(s);
	}
#endif
}

/*
 * Invalidate a piece of the kernel's address space.
 * This includes both obliterating any virtual cache tags and
 * annihilating the tlb entry.
 * (user addresses are invalidated mainly by changing the pid)
 */
kvirt_inval(vpn, size)
	unsigned vpn, size;
{
	caddr_t vaddr;
	int i;
	pde_t *pde;

	vaddr = (caddr_t)ctob(vpn);
	invalidate_virt_dcache(vaddr, ctob(size));
	pde = kvtokptbl(vaddr);
	for (i = 0; i < size; i++, pde++) {
		if (pde->pgm.pg_pfn && !pg_isnoncache(pde))
			invalidate_virt_scache(ptosv(pde->pgm.pg_pfn), NBPC);
		unmaptlb(0, vpn++);
	}
}


char	*version = "who cares";	/* just for savecore */
int	dumpmag = 0x8fca0101;	/* magic number for savecore */
int	dumpsize = 0;		/* also for savecore */

/* dump 16k per i/o */
#define DBSIZE		((NBPP * 4) >> SCTRSHFT)

void
dumperror(err)
	int err;
{
	printf("\nError during dump: ");
	switch (err) {
	  case ENXIO:
		printf("device bad\n");
		break;

	  case EFAULT:
		printf("device not ready\n");
		break;

	  case EINVAL:
		printf("area improper\n");
		break;

	  case EIO:
		printf("i/o error\n");
		break;

	  default:
		printf("unknown error (%d)\n", err);
		break;
	}
}

/*
 * doadump() comes here after turning off memory management and
 * getting on the dump stack, either when called above, or by
 * the auto-restart code.
 */
int no_diskless_dumps = 1;

void
dumpsys()
{
	register int dump_debug = 0;
	register int err;
	register int ndump = 0;	/* # of blocks available on dumpdev */
	register int (*dump)();
	extern int nodev();
	extern char *dump_path;
	extern char dump_hostname[];
	extern int nfs_dumpstub();

	if (dumpsize == 0)
		return;
	if ((dumpdev != -1 ) && ((major(dumpdev) >= bdevcnt) ||
	    ((dump = bdevsw[major(dumpdev)].d_dump) == 0))) {
		printf("\nno dump procedure for dev 0x%x\n", dumpdev);
		return;
	} if ( dumpdev == -1 ) {
		if (no_diskless_dumps) {
			if (showconfig)
				printf("\nno diskless dumps at this time.\n");
			return;
		}
		dump = nfs_dumpstub;
	}

	/*
	 * Amount to dump already determined in main.
	 */
	ndump = ptod(dumpsize);
	if ( dumpdev != -1 ) {
		printf("\ndumping to dev 0x%x at block %d, numpages %d\n",
		  dumpdev, dumplo, dumpsize);
	} else {
		printf("\ndumping to NFS (%s:%s) at block %d, numpages %d\n",
		  dump_hostname, dump_path, dumplo, dumpsize);
	}

	/* tell driver to initialize itself */
	if ((dumpdev != -1) && (err = (*dump)(dumpdev, DUMP_OPEN, 0, 0, 0))) {
		dumperror(err);
		return;
	}

	/* 
	 * dumpsize is set in main() to the total number pages which will 
	 * fit on dumpdev.  If the number of disk blocks
	 * needed to write all of memory out is greater than
	 * the number of disk blocks available on the dump device
	 * then dump as many pages as possible, in an order
	 * which saves the most important pages first.
	 */
	if ( ptod(physmem) > ndump || dump_debug ) { 
		printf("\ndoing ordered dump of only %d pages\n", 
			dtop(ndump));
		ordered_dump(ndump, dump);
	} else {
		printf("\ndoing complete dump\n");
		complete_dump(ptod(dumpsize), dump);
	}
	if (err = (*dump)(dumpdev, DUMP_CLOSE, 0, 0, 0))
		dumperror(err);
	else
		printf("\ndump completed\n");
}

ordered_dump(count, dump)
int count;
int (*dump)();
{
	register int index;
	register int err;
	register long physaddr, physaddr_upb;
	register pde_t *pde, *pde_top;
	register struct pfdat *pfd, *pfd1, *pfd_top;
	register struct dumphdr *cp;
	register preg_t	*prp;
	register caddr_t scp;
	register struct user *up = &u;
	extern 	 proc_t *lastproc;
	register unsigned pfn;
	daddr_t bn;

	if ((cp = (struct dumphdr *) getcpages(1, 1)) == NULL) {
		/* 
		 * steal a page from pfdat
		 */
		pfd = pfntopfdat(btoc(kpbase));
		pfd_top = pfntopfdat(maxclick);
		/*
		 * (it is not really necessary to search for a page
		 *  with a valid page number, since they should
		 *  all be in use -- otherwise we wouldn't be here --
		 *  but, just in case...)
		 */
		for ( ; pfd <= pfd_top, !pfd->pf_use ; pfd++);
		cp = (struct dumphdr *) ptosv(pfdattopfn(pfd));
		cmn_err(CE_WARN,"ordered_dump: no free pages, using 0x%x\n",cp);
	}
	strcpy(cp->magic, "ordered");
	cp->timestamp = time.tv_sec;
	bn = dumplo;

	/*
	 * Dump kernel text and data -- physical addresses 0 to (kpbase-1),
	 * plus the kernel page table (kptbl)
	 */
	physaddr = 0;
	physaddr_upb = kpbase + (syssegsz * sizeof(pde_t));
	while ( physaddr < physaddr_upb ) {
		cp->numpages = min ( btoc(physaddr_upb-physaddr), MAX_PPHDR );
		for ( index = 0; index < cp->numpages; index++ ) {
			cp->page[index] = physaddr;
			physaddr += NBPP;
		}
		if (cp->numpages == MAX_PPHDR)  {
			if (writedumphdr(dump, &bn, &count, cp) <= 0)
				return;
		}
	}

	/*
	 * Walk through the pfdats turning off the P_DUMPED bit, 
	 * just in case it was corrupted.
	 */
	pfd1 = pfntopfdat(btoc(physaddr_upb));
	pfd_top = pfntopfdat(maxclick);
	pfd = pfd1;
	for( ; pfd <= pfd_top; pfd++ ) {
		pfd->pf_flags &= ~P_DUMPED;  
	}

	/*
	 * Dump u-structure and kernel stack.
	 */
	if (dumpublock(up->u_procp, dump, &bn, &count, cp, 0) < 0)
		return;

	scp = (caddr_t)((USER_REG(EF_SP)) - 1);
	scp = (caddr_t)((int)scp & ~15);		/* Quad word aligned. */
	prp = findreg (u.u_procp, scp);
	if ( prp ) {
		if (dumpregion(prp->p_reg, dump, &bn, &count, cp) < 0)
			return;
	}

	if (dumpublock(lastproc, dump, &bn, &count, cp, 0) < 0)
		return;

	if (dumpregion(&sysreg, dump, &bn, &count, cp) < 0)
		return;

	/*
	 * Walk through process table and dump all the Uareas of
	 * live processes.
	 */
	for (index = 0; index < v.v_proc; index++) {
		if (proc[index].p_stat) {
			if (dumpublock( &proc[index],dump,&bn,&count,cp,1 ) < 0)
				return;
		}
	}

	/* 
	 * Dump kernel virtual memory (i.e., entries in kptbl).
	 */
	pde = kptbl;
	pde_top = kptbl + syssegsz;
	while ( pde < pde_top ) {
		while ( (cp->numpages < MAX_PPHDR) && (pde < pde_top)) {
			pfn = pde->pgm.pg_pfn;
			if (pg_isvalid(pde) && (pfn < physmem)) {
				cp->page[cp->numpages++] = pfn << BPCSHIFT;
			}
			pde++;
			if (cp->numpages == MAX_PPHDR) {
				if (writedumphdr(dump, &bn, &count, cp) <= 0)
					return;
			}
		}
	}
	/* 
	 * Walk through pfdat and dump any pages which haven't
	 * already been dumped and aren't on the free queue.
	 */
	pfd = pfd1;
	while ( pfd <= pfd_top ) {
		while ( (cp->numpages < MAX_PPHDR) && (pfd <= pfd_top)) {
			pfn = pfdattopfn(pfd);
			if ( ( pfn && pfn < physmem ) &&
			     ( ! (pfd->pf_flags & (P_QUEUE|P_DUMPED)) ) ) {
				cp->page[cp->numpages++] = pfn << BPCSHIFT;
				pfd->pf_flags |= P_DUMPED;  
			}
			pfd++;
			if (cp->numpages == MAX_PPHDR) {
				if (writedumphdr(dump, &bn, &count, cp) <= 0)
					return;
			}
		}
	}
	/*
	 * Dump whatever else there is room for by walking
	 * through pfdat dumping any pages which haven't
	 * already been dumped.
	 */
	pfd = pfd1;
	while ( pfd <= pfd_top ) {
		while ( (cp->numpages < MAX_PPHDR) && (pfd <= pfd_top)) {
			pfn = pfdattopfn(pfd);
			if ( ( pfn && pfn < physmem ) &&
			     ( ! (pfd->pf_flags & P_DUMPED ) )) {
				cp->page[cp->numpages++] = pfn << BPCSHIFT;
				pfd->pf_flags |= P_DUMPED;  
			}
			pfd++;
			if (cp->numpages == MAX_PPHDR) {
				if (writedumphdr(dump, &bn, &count, cp) <= 0)
					return;
			}
		}
	}
	if (cp->numpages > 0) {
		if (writedumphdr(dump, &bn, &count, cp) <= 0)
			return;
	}
	
}

writedumphdr(dump, bnp, countp, cp)
int (*dump)();
daddr_t *bnp;
int *countp;
struct dumphdr *cp;
{
	register int i;
	register int err;

	/*
	 * Adjust number of pages to be written if 
	 * we do not have enough disk space to write
	 * all of the pages in this header.
	 */
	if ( ((cp->numpages + 1) << DPPSHFT) > *countp ) {
		/* 
		 * If we don't have enough room for at least 2 pages
		 * (header page plus 1 page of data) then don't
		 * bother to write any more, else adjust numpages
	 	 * to fit space left.
		 */
		if (*countp < (2 << DPPSHFT)) {
			return(0);
		} else {
			/* adjust count to a intergal # of pages */
			*countp -= ((*countp%NDPP) + NDPP); 
			cp->numpages = (*countp>>DPPSHFT) - 1;
		}
	}
	
	flush_cache();		/* writeback caches to physical memory */
	if (err=(*dump)(dumpdev,DUMP_WRITE,*bnp,svirtophys(cp),NDPP)) {
		dumperror(err);
		return(-1);
	}
	*countp -= NDPP;
	*bnp += NDPP;
	for ( i = 0 ; i < cp->numpages; i++ ) {
		if (err=(*dump)(dumpdev,DUMP_WRITE,*bnp,cp->page[i],NDPP)) {
			dumperror(err);
			return(-1);
		}
		*bnp += NDPP;
		*countp -= NDPP;
		if ( (i&0xff)==0 ) {
			printf(".");
		}
	}
	cp->numpages = 0;
	return( *countp );
}

dumpregion(rp, dump, bnp, countp, cp)
register reg_t *rp;
int (*dump)();
daddr_t *bnp;
int *countp;
struct dumphdr *cp;
{
	int seg;			/* Current segment in region */
	int page;			/* Current page in seg. */
	int last_seg;			/* Last segment in region. */
	int last_page;			/* Last page in seg. */
	int pfn;
	struct pfdat *pfd;
	pde_t *pde;

#define LAST_PAGE(rp, seg) min(NPGPT, (rp)->r_pgoff + (rp)->r_pgsz - stoc(seg))

	if ( !rp ) return;

	seg  = 0;
	page = rp->r_pgoff;

	last_seg = ctos(rp->r_pgoff + rp->r_pgsz);

	for ( ; seg < last_seg; ++seg ) {
		last_page = LAST_PAGE( rp, seg );
		for ( ; page < last_page; ++page ) {
			pde = (pde_t *)(&(rp->r_list[seg][page]));
			if ( pde && pg_isvalid(pde)) {
				pfn = pde->pgm.pg_pfn;
			} else {
				continue;
			}
			pfd = pfntopfdat(pfn);
			if (pfn && (pfn < physmem) &&
			    (! (pfd->pf_flags & (P_QUEUE|P_DUMPED)))) {
				cp->page[cp->numpages++] = pfn << BPCSHIFT;
				pfd->pf_flags |= P_DUMPED;  
			}
			if (cp->numpages == MAX_PPHDR) {
			    if (writedumphdr(dump, bnp, countp, cp) <= 0)
					return(-1);
			}
		}
	}
	return(0);
}

dumpublock(p, dump, bnp, countp, cp, only_uarea)
struct proc *p;
int (*dump)();
daddr_t	*bnp;
int *countp;
struct dumphdr *cp;
int only_uarea;
{
	register int index, index_upb;
	register int err;
	register struct pfdat *pfd;
	register unsigned pfn;
	struct user *upt;			/* u-area pointer */
	register pde_t *ucred_pdptr;		/* user credentials pte */

	/*
	 *  Dump the pages which are mapped by the wired tlb entries
	 */
	index_upb = (only_uarea) ? USIZE : p->p_nexttlb;
	for ( index = 0; index < index_upb; index++ ) {
		if (pg_isvalid(&(p->p_ubptbl[index]))){
			pfn = p->p_ubptbl[index].pgm.pg_pfn;
		} else {
			continue;
		}
		pfd = pfntopfdat(pfn);
		if ( ! (pfd->pf_flags & (P_QUEUE|P_DUMPED))) {
			cp->page[cp->numpages++] = pfn << BPCSHIFT;
			pfd->pf_flags |= P_DUMPED;  
		}
		if (cp->numpages == MAX_PPHDR) {
			if (writedumphdr(dump, bnp, countp, cp) <= 0)
				return(-1);
		}
		if ((u_int)p->p_tlbhi_tbl[index] == UADDR) {
			/*
			 *  For the first U-area page, locate the
			 *  credentials structure and dump that, too.	
			 */
			upt = (struct user *)ptosv(pfn);
			if (IS_KSEG2(upt->u_cred)) {
			    ucred_pdptr = kvtokptbl(upt->u_cred);
			    pfd = pfntopfdat(ucred_pdptr->pgm.pg_pfn);
			    if ( ! (pfd->pf_flags & (P_QUEUE|P_DUMPED))) {
				cp->page[cp->numpages++]
				  = (u_int)ucred_pdptr->pgm.pg_pfn << BPCSHIFT;
				pfd->pf_flags |= P_DUMPED;  
			    }
			    if (cp->numpages == MAX_PPHDR) {
				if (writedumphdr(dump, bnp, countp, cp) <= 0)
					return(-1);
			    }
			}
		}
	}
	return(0);
}

complete_dump(count, dump)
register int count;
int (*dump)();
{
	register int amount;
	register int err;
	register daddr_t bn;
	register long physaddr;

	flush_cache();		/* writeback caches to physical memory */
	physaddr = 0;
	bn = dumplo;
	while (count) {
		amount = count;
		if (amount > DBSIZE)
			amount = DBSIZE;
		if (err = (*dump)(dumpdev, DUMP_WRITE, bn, physaddr, amount)) {
			dumperror(err);
			return;
		}
		if ((physaddr&0xFFFFF) == 0)	/* every 1MB */
			printf(".");
		bn += amount;
		physaddr += amount << SCTRSHFT;
		count -= amount;
	}
}


install_fast_utlbhndlr()
{
	extern struct bsd43_hw_config bsd43_hwconf;

	/* Because of 3.0 chip bug in first implementation, don't
	 * install a fast version of utlbmiss handler on a system
	 * using first implementation processor with a revision
	 * 3.x or less.
	 */
	if (!(bsd43_hwconf.cpu_processor.bsd43_ri_imp <= 1 &&
			bsd43_hwconf.cpu_processor.bsd43_ri_majrev < 4))
		install_utlbhndlr(UTLBHNDLR_FAST);
}

install_utlbhndlr(type)
int	type;
{
#ifndef	R6000
	int 	s;
	extern int	utlbhndlr;

	extern char utlb_3bug[], eutlb_3bug[];
	extern char utlb_3bugp[], eutlb_3bugp[];
	extern char utlb_fast[], eutlb_fast[];
	extern char utlb_fastp[], eutlb_fastp[];

	/* The following code relies upon the fact that you
	 * can't take any utlbmiss exception in the middle
	 * of copying this exception handler.
	 */
	if (utlbhndlr == type)
		return(0);

	s = splall();
	switch(type) {
	  case UTLBHNDLR_3BUG:
		bcopy(utlb_3bug, UT_VEC, eutlb_3bug - utlb_3bug);
		break;
	  case UTLBHNDLR_3BUGP:
		bcopy(utlb_3bugp, UT_VEC, eutlb_3bugp - utlb_3bugp);
		break;
	  case UTLBHNDLR_FAST:
		bcopy(utlb_fast, UT_VEC, 
				eutlb_fast - utlb_fast);
		break;
	  case UTLBHNDLR_FASTP:
		bcopy(utlb_fastp, UT_VEC, 
			    eutlb_fastp - utlb_fastp);
		break;
	  default:
		splx(s);
		return(EINVAL);
	}
	flush_cache();
	splx(s);
	utlbhndlr = type;
	return(0);
#else /* R6000 */
	return(EINVAL);
#endif /* R6000 */
}

posix_sendsig(p, sig, signalmask, sig_type)
void (*p)(); 
int sig, signalmask, sig_type;
{
	register struct posix_sigcontext *scp;
	register struct pregion *prp;
	register int *srp;
	register u_int *urp;
	register int *frp;

	ASSERT((u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) == \
			BSD43_U_SF_POSIX_SIGCONTEXT);

	XPRINTF(XPR_SIGNAL,"enter sendsig %d, 0x%x",sig,signalmask,0,0);

	scp = (struct posix_sigcontext *)STACK_ALIGN(USER_REG(EF_SP)) - 1;
	prp = findreg (u.u_procp, scp);
	if (prp)  {
		regrele(prp->p_reg);
	} else if (!grow(scp)) {
		return(0);
	}

	/* check write access */
	if (!useracc((caddr_t)scp, sizeof (*scp), B_READ)) {
		/* Process has trashed its stack; can't send a signal. */
		return(0);
	}
	/*
	 * Save into the sigcontext the signal state and that portion
	 * of process state that we trash in order to transfer control
	 * to the signal trampoline code.
	 * The remainder of the process state is preserved by
	 * signal trampoline code that runs in user mode.
	 */
	ktousigset(&signalmask, &scp->sc_sigmask);
	scp->sc_restoremask = 1;

	scp->sc_pc = USER_REG(EF_EPC);
	scp->sc_regs[R_V0] = USER_REG(EF_V0);
	scp->sc_regs[R_A0] = USER_REG(EF_A0);
	scp->sc_regs[R_A1] = USER_REG(EF_A1);
	scp->sc_regs[R_A2] = USER_REG(EF_A2);
	scp->sc_regs[R_A3] = USER_REG(EF_A3);
	scp->sc_regs[R_SP] = USER_REG(EF_SP);

	scp->sc_ownedfp = u.u_pcb.pcb_ownedfp;
	if (u.u_pcb.pcb_ownedfp) {
		checkfp (u.u_procp, 0);	/* dump fp to pcb */
		for (srp = &scp->sc_fpregs[0], frp = u.u_pcb.pcb_fpregs;
		    frp < &u.u_pcb.pcb_fpregs[32]; srp++, frp++)
			*srp = *frp;
		scp->sc_fpc_csr = u.u_pcb.pcb_fpc_csr;
		scp->sc_fpc_eir = u.u_pcb.pcb_fpc_eir;
		u.u_pcb.pcb_fpc_csr &= ~FPCSR_EXCEPTIONS;
	}
	scp->sc_cause = USER_REG(EF_CAUSE);
	scp->sc_badvaddr = USER_REG(EF_BADVADDR);

	/*
	 * setup register to enter signal handler
	 * when resuming user process
	 */
	USER_REG(EF_A0) = sig;
	if (sig == SIGFPE || sig == SIGSEGV || sig == SIGILL ||
	    sig == SIGBUS || sig == SIGTRAP) {
		USER_REG(EF_A1) = u.u_code;
		u.u_code = 0;
	} else
		USER_REG(EF_A1) = 0;
	USER_REG(EF_A2) = (unsigned)scp;
	USER_REG(EF_A3) = (unsigned)p;
	USER_REG(EF_SP) = STACK_ALIGN(scp);
	USER_REG(EF_EPC) = (unsigned)u.u_sigtramp;
	return(1);
}

posix_sigreturn()
{
	register int *srp;
	register u_int *urp;
	register int *frp;
	struct a {
		struct posix_sigcontext *sigcontextp;
	} *uap = (struct a *)u.u_ap;
	register struct posix_sigcontext *scp;

	scp = uap->sigcontextp;
	/* check for read access */
	if (!useracc((caddr_t)scp, sizeof (*scp), B_WRITE))
		return;
	u.u_eosys = FULLRESTORE;
	/* if we are called from sigsetjmp, we may or may not be called
	 * on to restore the signal mask... This will always be set if
	 * called from signal() or sigaction()...
	 */
	if (scp->sc_restoremask)
		u.u_procp->p_hold =
		    scp->sc_sigmask.sig_bits[0] &~ (bsd43_sigmask(SIGKILL)|
						    bsd43_sigmask(SIGSTOP));
	/*
	 * copy entire user process state from sigcontext into
	 * exception frame, a special exit from syscall insures
	 * that the entire exception frame gets restored
	 */
	USER_REG(EF_EPC) = scp->sc_pc;
	USER_REG(EF_MDLO) = scp->sc_mdlo;
	USER_REG(EF_MDHI) = scp->sc_mdhi;
	for (urp = &USER_REG(EF_AT), srp = &scp->sc_regs[R_AT];
	    urp <= &USER_REG(EF_RA); urp++, srp++)
			*urp = *srp;
	if (IS_M6000) {
		/* always put a good SBC IVectMask value on the stack */
		USER_REG(EF_IVECTMASK) = CSR_IVECTMASK_NONE;
	}
	if (u.u_pcb.pcb_ownedfp) {
		checkfp(u.u_procp, 1);	/* toss current fp contents */
		for (frp = u.u_pcb.pcb_fpregs, srp = &scp->sc_fpregs[0];
		    frp < &u.u_pcb.pcb_fpregs[32]; frp++, srp++)
			*frp = *srp;
		u.u_pcb.pcb_fpc_csr = scp->sc_fpc_csr & ~FPCSR_EXCEPTIONS;
	}
}
