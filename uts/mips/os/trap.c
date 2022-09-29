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
#ident	"$Header: trap.c,v 1.52.1.17.1.5.1.3 91/01/04 17:54:28 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/sysmacros.h"
#include "sys/param.h"
#include "sys/numips.h"
#include "sys/types.h"
#include "sys/psw.h"
#include "sys/pcb.h"
#include "sys/systm.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/signal.h"
#include "sys/immu.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/reg.h"
#include "sys/sysinfo.h"
#include "sys/edt.h"
#include "sys/limits.h"
#include "sys/utsname.h"
#include "sys/firmware.h"
#include "sys/cmn_err.h"
#include "sys/var.h"
#include "sys/debug.h"
#include "sys/ptrace.h"
#include "sys/inst.h"
#include "sys/fpu.h"
#include "bsd43/sys/vmmeter.h"
#include "sys/rambo.h"
#include "sys/bc.h"
#define	R_STIME	u_rambo_ticks[2]
#define	R_LEVEL	u_rambo_ticks[4]

extern int uarea_tlb_initialized;

int nofault;
int nofault_cause;
int nofault_badvaddr;
unsigned int saved_far;
unsigned short saved_fid;

extern struct reg_desc cp0_error_desc[];
extern struct reg_desc fpc_parity_desc[];
extern uint fptype_word;
uint cp0_error_imask = 0;		/* ignore no Parity errors	   */
uint cp0_error_check;			/* C0_ERROR at time of MachineCheck */
uint cp0_error_reg;			/*	    at last entry to kernel */

/*
 * This must be declared as an int array to keep the assembler from
 * making it gp relative.
 */
extern int sstepbp[];

/*
 * Used for decoding break instructions.  There is an old standing bug in the
 * assembler which encoded the break code right justified to bit 16 not to
 * bit 6 (that's why the BRK_SHIFT is 16 not 6 as would be obvious).
 */
#define BRK_MASK	0xfc00003f
#define BRK_SHIFT	16
#define BRK_SUBCODE(x)	(((x) & ~BRK_MASK) >> BRK_SHIFT)

/*
 * Used for decoding trap instructions.
 */
#define TRAP_MASK	0x00003fc0
#define TRAP_SHIFT	6
#define TRAP_SUBCODE(x)	(((x) & TRAP_MASK) >> TRAP_SHIFT)

addupc_clk()
{
	ASSERT(0);
#ifdef TODO
	register proc_t	*pp;

	addupc(u.u_pcb.pc, &u.u_prof, 1);
	pp = u.u_procp;
	curpri = pp->p_pri = calcppri(pp);
	if ( runrun != 0 ) 
		qswtch();

#endif TODO
}


static uint cp0_error_delay_report = 0;

report_soft_parity()
{
	uint	error_bits;
	uint	fpc_parity;

	/*
	 *  If we're in a mode where some parity errors are non-fatal
	 *  and are being ignored, then we still want to notice when
	 *  a particular parity error has occurred and to report it.
	 *  Report an error the first time it appears, then only report
	 *  reappearances every now and then.
	 *  This routine will check for new errors.  It's up to someone
	 *  to occasionally call it.
	 */

	if (cp0_error_delay_report) {
		cp0_error_delay_report--;	/* wait a bit longer */
		return;
	}

	error_bits = cp0_error_reg & C0_ERROR_MASK;

	if (error_bits) {
		cmn_err(CE_WARN,"CPU error: %R", error_bits, cp0_error_desc);
#ifndef SABLE
		cp0_error_delay_report = 300*HZ;	/* delay next report */
#endif
	}
	/*
	 *  Reset Cp0 Error reg in every case, in the event that
	 *  cp0_error_imask has been manually changed.
	 */
	reset_cp0_error( cp0_error_imask | error_bits );
	if (cp0_error_reg & C0_ERROR_EXT) {
		fpc_parity = (fptype_word) ? get_fpc_parity() : 0;
		if (fpc_parity & (FPPARITY_RF | FPPARITY_IB)) {
			cmn_err(CE_CONT,"  due to FPC error: %R",
				fpc_parity, fpc_parity_desc);
			set_fpc_parity( fpc_parity &
				        ~(FPPARITY_RF | FPPARITY_IB) );
		}
	}
	if (error_bits)
		cmn_err(CE_CONT,"\n");
}


k_trap(ep, code, sr, cause)
register uint *ep, code;
uint sr, cause;
{
	register int		i;
	int	saved_nofault;
	int	saved_nofault_cause;
	int	saved_nofault_badvaddr;
	uint	fpc_parity;

	switch (code) {

	case EXC_CHECK:
		flush_cache();
		flush_tlb();
		dumpregs(ep, code, sr, cause, cp0_error_check);
		if (cp0_error_check & (C0_ERROR_RF | C0_ERROR_EXT)) {
		    fpc_parity = (fptype_word) ? get_fpc_parity() : 0;
		    if (fpc_parity & (FPPARITY_RF | FPPARITY_IB)) {
			/*
			 *  FPC reports parity error -- fatal to kernel
			 */
			set_fpc_parity( fpc_parity &
				        ~(FPPARITY_RF | FPPARITY_IB) );
		    }
		    /*
		     *  This is a Reg File or an External Parity error.
		     *  The simplest thing to do is to panic.
		     */
		    cmn_err(CE_PANIC, "k_trap, proc = %x",
			    (uarea_tlb_initialized) ? u.u_procp : 0); 
		}
		reset_cp0_error(cp0_error_imask);	/* reenable Checks */
		/*
		 *  I- or D-bus parity error is retryable
		 */
		return;

	case EXC_IBE:
	case EXC_DBE:
	case EXC_RADE:
	case EXC_WADE:
	case SEXC_SEGV:
	case SEXC_KILL:
	case SEXC_BUS:
		/*
		 *	save nofault across possible wait
		 */
		saved_nofault = nofault;
		saved_nofault_cause = nofault_cause;
		saved_nofault_badvaddr = nofault_badvaddr;
		nofault = 0;

		/*	Try to correct the fault.
		*/

		i = usrxmemflt(ep,code,sr,cause);

		/*
		 *	restore nofault
		 */
		nofault = saved_nofault;
		nofault_cause = saved_nofault_cause;
		nofault_badvaddr = saved_nofault_badvaddr;

		/*
		 * If usrxmemflt returned 0, the fault was fixed.
		 * Return to faulting code.
		 */

		if(i == 0)
			return;

		/*	If usrxmemflt returned a non-zero value,
		**	then the fault couldn't be corrected.
		**	If there is a nofault routine, return to the 
		**	error routine indicated by nofault.
		*/

		if (i != 0 && nofault) {
			extern (*nofault_pc[])();
			nofault_cause = cause;
			nofault_badvaddr = ep[EF_BADVADDR];
			i = nofault;
			nofault = 0;
			if (i < 1 || i >= NF_NENTRIES)
				cmn_err(CE_PANIC,"bad nofault: %d",i);
			ep[EF_EPC] = (uint)nofault_pc[i];
			return;
		}

		/*
		 * If usrxmemflt couldn't fix the problem and there
		 * was no nofault routine indicated, fall through
		 * to panic.
		 */

	default:
		dumpregs(ep, code, sr, cause);
		cmn_err(CE_PANIC, "K_TRAP, proc = %x",
			(uarea_tlb_initialized) ? u.u_procp : 0); 

	}
}

trap(ep, code, sr, cause)
register uint *ep, code;
uint sr, cause;
{
	extern int		syscall();
	register int		sig;
	register struct user	*uptr = &u;
	register time_t		syst;
	register struct proc	*pp;
	uint sp;
	int vaddr;
	uint inst;
	union mips_instruction	instruction;
	uint fpc_parity;

	ASSERT(ep[EF_SP] != (uint)ep); /* XXX */

	if(!USERMODE(sr)) {
		k_trap(ep, code, sr, cause);
		return;
	}

	ASSERT(nofault == 0);
	syst = uptr->u_stime;		/* access uarea after k_trap check */
	cnt.v_trap++;

	sig = 0;

	pp = uptr->u_procp;

	switch(code) {

	case SEXC_RESCHED:
		/*
		 * softfp has called psignal and needs to have
		 * curproc process the signal
		 */
		if (u.u_pcb.pcb_resched) {
			u.u_pcb.pcb_resched = 0;
			if ((pp->p_flag&SOWEUPC)) {
				if (u.u_prof.pr_scale)
					addupc(ep[EF_EPC], &u.u_prof, 1);
				pp->p_flag &= ~SOWEUPC;
			}
		} else
			qswtch();
		break;

	case SEXC_KILL:		/* due to bad page read in vfault */
		sig = SIGKILL;
		break;

	case SEXC_BUS:
		sig = SIGBUS;
		break;

	case EXC_RMISS:		/* from softfp nofault code */
	case EXC_WMISS:
		code = SEXC_SEGV;
		/* fall through */

	case SEXC_SEGV:
		if((ep[EF_SP] & 0x80000000) == 0 &&
			grow(ep[EF_SP]))
			break;
		u.u_code = code;
		sig = SIGSEGV;
		break;

	case EXC_II:
	case SEXC_CPU:
		u.u_code = code;
		sig = SIGILL;
		break;

	case EXC_OV:
		u.u_code = code;
		sig = SIGFPE;
		break;

	case EXC_DBE:
		/*
		 * interpret instruction to calculate faulting address
		 */
		ep[EF_BADVADDR] = ldst_addr(ep);
		/* fall through */
	case EXC_IBE:
		u.u_code = code;
		sig = SIGBUS;
#ifdef MIPS_LOCAL
	printf("%sBE: %s (%d) pc=0x%x vaddr=0x%x\n", 
		(code == EXC_DBE) ? "D" : "I",
		u.u_comm, u.u_procp->p_pid, 
		(ep[EF_CAUSE] & CAUSE_BD) ? ep[EF_EPC]+4 : ep[EF_EPC],
		ep[EF_BADVADDR]);
#ifdef R6000
	printf("  SBC ErrReg=0x%x\n", *(uint *)CSR_ERRREG);
#endif R6000
#endif MIPS_LOCAL
		break;

	case EXC_RADE:
	case EXC_WADE:
		if((u.u_procp->p_flag & SFIXADE) && fixade(ep, cause))
			break;
		u.u_code = code;
		sig = SIGBUS;
		break;

	case EXC_DBL_NC:
		/*
		 *  XXX  should do something similar to EXC_*ADE:  if
		 *	 p_flag allows, do singleword uncached loads or stores.
		 *	 For now, just kill the process.
		 */
		u.u_code = code;
		sig = SIGBUS;
		break;

	case EXC_TRAP:
		sig = SIGTRAP;
		vaddr = ep[EF_EPC];
		if (ep[EF_CAUSE] & CAUSE_BD)
			vaddr += 4;
		instruction.word = fuiword((caddr_t)vaddr);
		if (instruction.i_format.opcode == spec_op) {
			/*
			 *  Register form of the instruction.
			 *  Subcode comes from "unused" bits in the instruction.
			 */
			u.u_code = TRAP_SUBCODE(instruction.word);
		} else {
			u.u_code = BRK_RANGE;
		}
		break;

	case EXC_BREAK:
		sig = SIGTRAP;
		vaddr = ep[EF_EPC];
		if (ep[EF_CAUSE] & CAUSE_BD)
			vaddr += 4;
		inst = fuiword((caddr_t)vaddr);
		u.u_trapcause = (fuiword((caddr_t)vaddr) == *sstepbp)
		    ? CAUSESINGLE : CAUSEBREAK;
		u.u_code = BRK_SUBCODE(inst);
		break;

	case EXC_CHECK:
		flush_tlb();
		flush_cache();
		dumpregs(ep, code, sr, cause, cp0_error_check);
		if (cp0_error_check & C0_ERROR_EXT) {
		    fpc_parity = (fptype_word) ? get_fpc_parity() : 0;
		    if (fpc_parity & (FPPARITY_RF | FPPARITY_IB)) {
			/*
			 *  FPC reports parity error -- fatal to process
			 */
			set_fpc_parity( fpc_parity &
				        ~(FPPARITY_RF | FPPARITY_IB) );
			u.u_code = code;
			sig = SIGBUS;			/* kill the process */
		    } else {
			/*
			 *  This is a non-FPC error, probably an S-cache tag
			 *  parity error, and we don't know where the problem
			 *  lies.  The simplest thing to do is to panic.
			 */
			cmn_err(CE_PANIC,"trap");
		    }
		}
		reset_cp0_error(cp0_error_imask);	/* reenable Checks */
		if (cp0_error_check & C0_ERROR_RF) {
		    /*
		     *  Reg File parity error is fatal to the executing process
		     */
		    u.u_code = code;
		    sig = SIGBUS;			/* kill the process */
		}
		/*
		 *  I- or D-bus parity error is retryable
		 */
		break;

	default:
		dumpregs(ep, code, sr, cause);
		cmn_err(CE_PANIC,"trap");

	}

	curpri = pp->p_pri = calcppri(pp);

	if ( sig != 0 ) {
		psignal( uptr->u_procp, sig );
		if (u.u_pcb.pcb_bd_ra) {
			ep[EF_EPC] = u.u_pcb.pcb_bd_epc;
			ep[EF_CAUSE] = u.u_pcb.pcb_bd_cause;
			u.u_pcb.pcb_bd_ra = 0;
		}
	}

	if ( runrun != 0 ) {
		u.u_ru.ru_nivcsw++;
		qswtch();
	};
	if ( uptr->u_procp->p_cursig ||
	    (uptr->u_procp->p_flag & SPRSTOP) ||	/* for /proc */
	    (uptr->u_procp->p_sig && issig()) ) {
		psig(SIG_TRAP);
		curpri = pp->p_pri = calcppri(pp);
	}
	/*
	 * if single stepping this process, install breakpoints before
	 * returning to user mode.  Do this here rather than in procxmt
	 * so single stepping will work when signals are delivered.
	 */
	if (u.u_pcb.pcb_sstep)
		install_bp();
	if ( uptr->u_prof.pr_scale ){
		addupc((caddr_t)ep[EF_EPC],
			&uptr->u_prof,
			(int)(uptr->u_stime - syst));
	}
}

/*
 * install_bp -- install breakpoints to implement single stepping
 */
static
install_bp()
{
	unsigned inst;
	unsigned target_pc;

	if (u.u_pcb.pcb_ssi.ssi_cnt)
		cmn_err(CE_PANIC, "install_bp2");
	/*
	 * If user can't access where his pc points, we give up.
	 * He'll be getting a SIGSEGV shortly anyway!
	 */
	if ((inst = fuiword(USER_REG(EF_EPC))) == -1)
		return;
	if (is_branch(inst)) {
		target_pc = branch_target(inst, USER_REG(EF_EPC));
		/*
		 * Can't single step self-branches, so just wait
		 * until they fall through
		 */
		if (target_pc != USER_REG(EF_EPC))
			set_bp(target_pc);
		set_bp(USER_REG(EF_EPC)+8);
	} else
		set_bp(USER_REG(EF_EPC)+4);
	/*
	 * only install breakpoints once!
	 */
	u.u_pcb.pcb_sstep = 0;
}

static
set_bp(addr)
unsigned *addr;
{
	register struct ssi_bp *ssibp;

	ssibp = &u.u_pcb.pcb_ssi.ssi_bp[u.u_pcb.pcb_ssi.ssi_cnt];
	ssibp->bp_addr = addr;
	/*
	 * Assume that if the fuiword fails, the write_utext will also
	 */
	ssibp->bp_inst = fuiword(addr);
	if (write_utext(addr, *sstepbp)) {
		u.u_pcb.pcb_ssi.ssi_cnt++;
		if (IS_R6300) {
		    invalidate_virt_icache( addr,sizeof(int) );
		} else {
		    vflush(addr, sizeof(int));
		}
	}
}

/*
 * remove_bp -- remove single step breakpoints from current process
 */
remove_bp()
{
	register struct ssi_bp *ssibp;

	while (u.u_pcb.pcb_ssi.ssi_cnt > 0) {
		u.u_pcb.pcb_ssi.ssi_cnt--;
		ssibp = &u.u_pcb.pcb_ssi.ssi_bp[u.u_pcb.pcb_ssi.ssi_cnt];
		if (!write_utext(ssibp->bp_addr, ssibp->bp_inst)) {
			printf("couldn't remove breakpoint\n");
			continue;
		}
		if (IS_R6300) {
		    invalidate_virt_icache( ssibp->bp_addr, sizeof(int) );
		} else {
		    vflush(ssibp->bp_addr, sizeof(int));
		}
	}
}

static
is_branch(inst)
{
	union mips_instruction i;

	i.word = inst;
	switch (i.j_format.opcode) {
	case spec_op:
		switch (i.r_format.func) {
		case jr_op:	case jalr_op:
			return(1);
		}
		return(0);

	case bcond_op:
		switch (i.i_format.rt) {
		case bltz_op:	case bgez_op:	case bltzl_op:	case bgezl_op:
		case bltzal_op:	case bgezal_op:	case bltzall_op:case bgezall_op:
			return(1);
		}
		return(0);

	case j_op:	case jal_op:
	case beq_op:	case bne_op:	case blez_op:	case bgtz_op:
	case beql_op:	case bnel_op:	case blezl_op:	case bgtzl_op:
		return(1);

	case cop0_op:	case cop1_op:	case cop2_op:	case cop3_op:
		switch (i.r_format.rs) {
		case bc_op:
			return(1);
		}
		return(0);
	}
	return(0);
}

#define	REGVAL(x)	((x)?USER_REG((x)+EF_AT-1):0)
static
branch_target(inst, pc)
{
	union mips_instruction i;
	register short simmediate;

	i.word = inst;
	switch (i.j_format.opcode) {
	case spec_op:
		switch (i.r_format.func) {
		case jr_op:	case jalr_op:
			return(REGVAL(i.r_format.rs));
		}
		break;

	case bcond_op:
		switch (i.i_format.rt) {
		case bltz_op:	case bgez_op:	case bltzl_op:	case bgezl_op:
		case bltzal_op:	case bgezal_op:	case bltzall_op:case bgezall_op:
			/*
			 * assign to temp since compiler currently
			 * doesn't handle signed bit fields
			 */
			simmediate = i.i_format.simmediate;
			return(pc+4+(simmediate<<2));
		}
		break;

	case j_op:	case jal_op:
		return( ((pc+4)&~((1<<28)-1)) | (i.j_format.target<<2) );

	case beq_op:	case bne_op:	case blez_op:	case bgtz_op:
	case beql_op:	case bnel_op:	case blezl_op:	case bgtzl_op:
		/*
		 * assign to temp since compiler currently
		 * doesn't handle signed bit fields
		 */
		simmediate = i.i_format.simmediate;
		return(pc+4+(simmediate<<2));

	case cop0_op:	case cop1_op:	case cop2_op:	case cop3_op:
		switch (i.r_format.rs) {
		case bc_op:
			/*
			 * kludge around compiler deficiency
			 */
			simmediate = i.i_format.simmediate;
			return(pc+4+(simmediate<<2));
		}
		break;
	}
	cmn_err(CE_PANIC, "branch_target");
}

ldst_addr(ep)
register u_int *ep;
{
	register u_int *pc;
	union mips_instruction i;
	int base;

	pc = (u_int *)ep[EF_EPC];
	if (ep[EF_CAUSE] & CAUSE_BD)
		pc++;
	i.word = *pc;	/* theoretically can't fault */
	if (i.i_format.opcode < lb_op) {
		dumpregs(ep);
		panic("DBE not on load or store");
	}
	base = (i.i_format.rs == 0) ? 0 : ep[EF_AT + i.i_format.rs - 1];
	return (base + i.i_format.simmediate);
}

trap_nofault(ep, code, sr, cause)
register uint *ep;
register uint code;
uint sr, cause;
{
	register i;

	switch(code) {

	case EXC_DBE:
		/* nofault handler */
		if (nofault) {
			extern (*nofault_pc[])();
			i = nofault;
			nofault = 0;
			if (i < 1 || i >= NF_NENTRIES)
				cmn_err(CE_PANIC, "bad nofault");
			ep[EF_EPC] = (uint)nofault_pc[i];
			return;
		}
		/* fall through to default */

	default:
		dumpregs(ep, code, sr, cause);
		cmn_err(CE_PANIC,"trap_nofault");

	}
}

dumpregs(ep, code, sr, cause, machine_check_cause)
uint *ep;
{
	extern struct reg_desc fid_desc[];
	uint fpc_parity;

	/*
	 * Dumpregs is called just before panic.  The kernel should
	 * send this information directly to the frame buffer instead
	 * of a stream which will be going away in the very near future.
	 */
        graphics_shutdown();
	cmn_err(CE_CONT,"\nPC: 0x%x  RA: 0x%x\n", ep[EF_EPC], ep[EF_RA]);
	cmn_err(CE_CONT,"EXC code: %d, `%s'", code, code_names[code>>2]);
	if (code == EXC_CHECK) {
		cmn_err(CE_CONT," %R",
			machine_check_cause, cp0_error_desc);
		if (machine_check_cause & C0_ERROR_EXT) {
		    fpc_parity = (fptype_word) ? get_fpc_parity() : 0;
		    if (fpc_parity & (FPPARITY_RF | FPPARITY_IB)) {
			cmn_err(CE_CONT,"\n             due to FPC error %R",
				fpc_parity, fpc_parity_desc);
		    }
		}
	}
	cmn_err(CE_CONT,"\nBad addr: 0x%x, ", ep[EF_BADVADDR]);
	cmn_err(CE_CONT,"cause: 0x%x, sr: 0x%x\n", cause, sr);
	if (IS_R3030) {
		r3030_report();
	} else if (IS_R2400) {
		printf("FID: %R\n", saved_fid ,fid_desc);
		printf("FAR: 0x%x\n", saved_far);
	}
}


/*
 * nonexistent system call-- signal bad system call.
 */
nosys()
{
	if (((u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) ==
			BSD43_U_SF_BSD43_SIGCONTEXT) &&
	    (u.u_signal[SIGSYS - 1] == SIG_IGN ||
	     u.u_signal[SIGSYS - 1] == SIG_HOLD))
		u.u_error = EINVAL;
	psignal(u.u_procp, SIGSYS);
}

errsys()
{
	u.u_error = EINVAL;
}

/* 
 * An entry with "indir" is handled by the syscall specified in register A0.  
 * This code gets executed only if the A0 value points at an indir.
 */
indir() {
	nosys();
};

/*
 * Invalid sysent structure at which to point callp if necessary.
 */
struct sysent badsysent = { 0, nosys, 0 };

/* 
 * To support multiple systype's, syscall() has been generalized to search for system
 * calls in more than one array.  (See numips.h).
 *
 * This code initializes the array of sysent arrays that syscall() uses.
 */
extern int nsysent;
extern int bsd_nsysent;
extern int posix_nsysent;

struct sysent_tab sysent_tab[] = {
	0, 0,				/* 0-999: Reserved */
	sysent, 0,			/* 1000-1999: -systype sysv */
	bsd_sysent, 0,			/* 2000-2999: -systype bsd43 */
	posix_sysent, 0			/* 3000-3999: -systype posix */
};

int nsysent_tab = sizeof(sysent_tab)/sizeof(sysent_tab[0]);

syscall_init() {
	/*
	 * Do sysent_tab initialization that can't happen statically.
	 */
	sysent_tab[1].nents = nsysent;
	sysent_tab[2].nents = bsd_nsysent;
	sysent_tab[3].nents = posix_nsysent;
}

/* 
 * To support the various systypes allowed under RISC/os, syscall() has
 * been generalized to support multiple arrays of system call entries.
 * Each systype N is assigned the system call numbers on the range
 * [N*1000,N*1000+1000).  
 *
 * See <sys/numips.h> for definitions of the various systypes.  That
 * incude file also contains truth macros which can be used to determine
 * the systype of the current system call.
 *
 * Note:  VEC_syscall does all the work to save s0-s7 in case we return
 * through trap (who expects to be able to reload the user context from
 * EP), and then we save all the callee saved again here.  This is costing
 * us 8 sw and 8 lw, for a big 30 cycles or so.  Is this worth trying to
 * fix?  per djl
 */
unsigned long	rambo_clock_delay;	/* used for the highly accurate clock */

syscall(ep, sysnum, sr, cause)
uint *ep;
uint sysnum;
{
	register struct user	*uptr = &u;
	register struct sysent	*callp = 0;
	register struct proc	*pp;
	register struct proc	*pi;
	register 		regparams;
	register int		*params;
	register uint		nargs;
	time_t			syst;
	short			pid;
	int 			systype;
	int			opc;
	volatile struct rambo *rambo = ((struct rambo *)PHYS_TO_K1(RAMBO_BASE));
#define	rambo_at_eend	*(unsigned long *)0x800003ec	/* defined in locore.s */
	int			return_value;

	if (IS_R3030) {
		rambo_at_eend = rambo->rambo_tcount;
		u.R_LEVEL = 1;
	}
	syst = u.u_stime;
	pid = u.u_procp->p_pid;

	sysinfo.syscall++;
	u.u_error = 0;

	opc = ep[EF_EPC];
	ep[EF_EPC] += 4;	/* normal return executes next instruction */

	if(!USERMODE(sr))
		cmn_err(CE_PANIC,"syscall from kernel");

	regparams = EF_A0;

	/* 
	   Gross hack here, for efficiency.  Clean code with / and % is
	   very very slow.
	*/
	if (sysnum < sysent_tab[0].nents) 
		callp = &(sysent_tab[0].sysentp[sysnum]), systype = 0;
	else if (sysnum >= 1000 && sysnum < 1000 + sysent_tab[1].nents) 
		callp = &(sysent_tab[1].sysentp[sysnum - 1000]), systype = 1;
	else if (sysnum >= 2000 && sysnum < 2000 + sysent_tab[2].nents) 
		callp = &(sysent_tab[2].sysentp[sysnum - 2000]), systype = 2;
	else if (sysnum >= 3000 && sysnum < 3000 + sysent_tab[3].nents) 
		callp = &(sysent_tab[3].sysentp[sysnum - 3000]), systype = 3;
	else {
		u.u_error = EINVAL;
		callp = &badsysent;	/* call to nosys() -- illegal */
		ep[EF_EPC] = opc;	/* pc back to syscall inst */
	} 

	if (callp->sy_call == indir) {
		/*
		 * Indirect syscall.  Use syscall # from user register A0.
		 * I left this case as was, if we use indir's much, should
		 * be special cased like above.
		 */
		sysnum = ep[EF_A0];
		regparams++;
		systype = sysnum/1000;
		if ((systype >= nsysent_tab) 
		|| (sysnum%1000 >= sysent_tab[systype].nents)) {
			u.u_error = EINVAL;
			callp = &badsysent;	/* call to nosys() -- illegel */
			ep[EF_EPC] -= 4;    	/* pc back to syscall inst */
		} 
		else {
			callp = &(sysent_tab[systype].sysentp[sysnum%1000]);
		}
	}
	u.u_syscall_type = systype;
	/*
	 * u.u_ksegflg, if non-zero, causes copyin and
	 * copyout to allow references kernel space; this is
	 * used in allowing the kernel to execute system calls 
	 * internally
	 * We reset it here to ensure proper SysV semantics
	 */
	u.u_ksegflg = 0;

	u.u_ar0 = (int *)ep;	/* for exec */
	u.u_eosys = NORMALRETURN;
	params = (int *)u.u_arg;
	for(nargs = callp->sy_narg; nargs && regparams <= EF_A3; nargs--)
		*params++ = ep[regparams++];
	if(nargs) {
		if (copyin(ep[EF_SP] + 4*sizeof(int), params,
			nargs * sizeof(int))) {
			u.u_error = EFAULT;
			goto bad;
		}
	}

	u.u_rval1 = 0;
	u.u_rval2 = ep[EF_V1];
	u.u_ap = u.u_arg;

	MONITOR('s', sysnum, pid, ep[EF_EPC], 0 ); /* XXX */

	if ( callp->sy_flags & SY_SETJMP )
		if (setjmp(u.u_qsav)) {
			if (u.u_error == 0 && u.u_eosys != RESTARTSYS)
				u.u_error = EINTR;
		} else
			(*callp->sy_call)(u.u_ap);
	else
		(*callp->sy_call)(u.u_ap);

bad:
	/*
	 * a3 is returned to user 0 if indicate no errors on syscall,
	 * non-zero otherwise
	 */
	if (u.u_eosys == NORMALRETURN) {
		if (u.u_error) {
			ep[EF_V0] = u.u_error;
			ep[EF_A3] = 1;
			u.u_error = 0;
			if (++u.u_errcnt > 16) {
				u.u_errcnt = 0;
				runrun++;
			}
		} else {
			/*
			 * Some calls, like sigreturn, should not update
			 * the return value, because it would overwrite the
			 * return value of an interrupted system call.
			 */
			if ( !(callp->sy_flags & SY_SIGRET) )
			{
				ep[EF_V0] = u.u_rval1;
				ep[EF_V1] = u.u_rval2;
				ep[EF_A3] = 0;
			}
		}
	} else if (u.u_eosys == RESTARTSYS)
		ep[EF_EPC] = opc;
	/* else if (u.u_eosys == FULLRESTORE) */
		/* returning from sigreturn, force full state restore */

	pp = u.u_procp;
	INODE_LOCK_TEST(pp->p_pid);
	if (runrun != 0){
		curpri = pp->p_pri = CALCCPRI(pp);
		u.u_ru.ru_nivcsw++;
		/* do we need to swtch, or are we still top dog? */
		for (pi = runq  ;  pi != NULL  ;  pi = pi->p_link) {
			if (pi->p_pri > curpri)
				continue;
			if ((pi->p_flag & (SLOAD|SPROCIO)) != SLOAD)
				continue;
			/* if we get here, there is a proc that should run */
			qswtch();
			/* we are back, and should break out of the loop */
			break;
		}
	}

	/* Check for signals to deliver.  SY_SIGRET needs special handling. */
	if ( callp->sy_flags & SY_SIGRET )
		issig() && psig(SIG_SIGRET);
 	else if (issig() || (pp->p_flag & SPRSTOP))	/* for /proc */
 		psig(SIG_SYSCALL);

	curpri = pp->p_pri = CALCCPRI(pp);

	/*	If pid != pp->p_pid, then we are the child
	**	returning from a fork system call.  In this
	**	case, ignore syst since our time was reset
	**	in fork.
	*/

	if (u.u_prof.pr_scale)
		addupc((caddr_t)u.u_ar0[EF_EPC], &u.u_prof, 
			pid == pp->p_pid ? (int)(u.u_stime - syst)
					 : (int)u.u_stime);
	/*
	 * if single stepping this process, install breakpoints before
	 * returning to user mode.  Do this here rather than in procxmt
	 * so single stepping will work when signals are delivered.
	 */
	if (u.u_pcb.pcb_sstep)
		install_bp();
	/*
	 * If we are returning from a signal through sigreturn(), then
	 * we must restore the entire sigcontext, not just the normal
	 * system call context. The way to restore all context is to go
	 * back through the trap return interface. Returning non-zero
	 * from syscall() tells the return code to do an entire context
	 * restore instead of the normal syscall() context restore.
	 */
	ASSERT(nofault == 0);
	if ( (callp->sy_flags & SY_SIGRET) || (u.u_eosys == FULLRESTORE) )
		return_value = 1;
	else
		return_value = 0;
	if (IS_R3030) {
		int	s = splall();

		u.R_STIME += rambo->rambo_tcount - rambo_at_eend;
		while (u.R_STIME > rambo_clock_delay) {
			u.R_STIME -= rambo_clock_delay;
			u.u_stime++;
		}
		u.R_LEVEL = 0;
		splx(s);
	}
	return return_value;
}

/* 
 * package not installed -- return ENOPKG error  (STUBS support)
 */
nopkg()
{
	u.u_error = ENOPKG;
}


/*
 * internal function call for uninstalled package -- panic system.  If the
 * system ever gets here, it means that an internal routine was called for
 * an optional package, and the OS is in trouble.  (STUBS support)
 */
noreach()
{
	cmn_err(CE_PANIC,"Call to internal routine of uninstalled package");
}

/*
 * stray interrupts enter here
 */

intnull()
{
	/* TODO */
	int dummy = 1;
}

/*
 * intspwr()
 *
 * Handle a softpower shutdown. Simply sends SIGPWR to init, allowing
 * it to gracefully stop the machine.
 */
intspwr()
{
	psignal(&proc[1], SIGPWR);
}

/*
 * process exception
 */

intpx(pcbp)
struct pcb *pcbp;
{
	splall();
	cmn_err(CE_PANIC,
		"process exception, proc = 0x%x, pcbp = 0x%x.\n",
		u.u_procp, pcbp) ;
}

/*	This routine is called for kernel stack exceptions.
*/

intsxk()
{

	splall();
	cmn_err(CE_PANIC,"kernel process stack exception \n");
}

/*
 * Ignored system call
 */
nullsys()
{
	int dummy = 1;
}

/* 
 * Interrupt for asynchronous bus errors.
 * TODO: if user, kill process; if kernel, panic;
 */
buserror_intr(ep) 
int *ep;
{
	register struct user	*up = &u;

	/* these haven't been saved till now. do it here to avoid
	 * the overhead on any interrupt!
	 */
  	if (IS_R2400) {
		saved_fid = *(short *)(PHYS_TO_K1(FID));
		saved_far = *(int *)(PHYS_TO_K1(FAR));
	}

	cmn_err(CE_CONT,"Bus error interrupt PC: 0x%x\n", ep[EF_EPC]);
	/* error register handling is different for different models.
	 * The M 500/800/1000 register resets on read.
	 * The M 2000 register resets on write.
	 * The M 120 has fault cause and address regs reset by reads.
	 */
	if (IS_M2000_ARCH) {
		cmn_err(CE_CONT,"system bus error addr is 0x%x\n",
			*(volatile int *)(PHYS_TO_K1(BWE_ADDR_R3200)));
			*(volatile int *)(PHYS_TO_K1(BWE_ADDR_R3200))= 0;
			wbflush();
	} else if (IS_R2300) {
		cmn_err(CE_CONT,"system bus error addr is 0x%x\n",
			*(int *)(PHYS_TO_K1(SBE_ADDR)));
	} else if (IS_R2400) {
		printf("fault id: %R\n", saved_fid ,fid_desc);
		cmn_err(CE_CONT,"error addr is 0x%x\n", saved_far);
	} else if (IS_R3030) {
		r3030_report();
	} else {
		cmn_err(CE_CONT,"unknown machine type - no info available\n");
	}
	if(!USERMODE(ep[EF_SR])) {
		cmn_err(CE_PANIC,"kernel bus error");
	} else {
		/* For machines prior to the 6000, the bus error interrupt
		 * will be caused by the current process.  If long delays
		 * to memory are possible, a context switch, or a kernel
		 * interrupt, may have already occurred.  It may not be
		 * appropriate to kill the process on future machines.
		 */
		printf("buserror_intr: ep is 0x%x\n",ep);
		printf("epc 0x%x, status 0x%x, cause 0x%x\n",
				ep[EF_EPC], ep[EF_SR], ep[EF_CAUSE]);
		signal(up->u_procp->p_pid, SIGBUS);
	}

}


stray(ep)
int *ep;
{
	int addr = ep[EF_EPC];
	cmn_err(CE_NOTE,"stray interrupt at %x\n", addr);
	cmn_err(CE_CONT,"Cause: 0x%x\n", ep[EF_CAUSE]);
}

/*
 * tlbmiss for kuseg (present, not valid)
 * 	vfault() returns 0 or SEXC_SEGV or SEXC_BERR
 * 	(vfault does tlbdropin)
 * utlbmiss access to k2seg (double tlb miss)
 * 	tfault() returns 0 or SEXC_SEGV
 * 	(tfault does tlbdropin for both pages )
 * 	(tfault could check validity of pte and call vfault,
 * 	 but for now I'll let it fault again if valid bit off)
 * other kernel access to k2seg tlb miss
 *	if in sysseg (first 4 Meg of k2seg)
 *		get pde from kernel page table.
 * 	panic (ublock and utlbmiss context are only k2seg data)
 */

int kvmiss = 0;

tlbmiss(ep, code, vaddr, cause) 
uint *ep; 
uint code, vaddr, cause;
{
	int	result;
	int utlbmiss_pending;
	int	saved_nofault;
	int	saved_nofault_cause;
	int	saved_nofault_badvaddr;
#ifndef R6000
	extern char utlbmiss[], eutlbmiss[];

	utlbmiss_pending = (ep[EF_EPC] >= UT_VEC  &&
			    ep[EF_EPC] <= UT_VEC + (eutlbmiss - utlbmiss));
#endif !R6000

	ASSERT(ep[EF_SP] != (uint)ep); /* XXX */

	/* XXX looking for timing or race condition: rwn */
	ASSERT((ep[EF_SR] & SR_IEC) == 0); /* XXX */

	MONITOR('t', 0, vaddr, ep[EF_EPC], ep[EF_K0]); /* XXX */

	/*
	 *	save nofault across possible reuse
	 */
	saved_nofault = nofault;
	saved_nofault_cause = nofault_cause;
	saved_nofault_badvaddr = nofault_badvaddr;
	nofault = 0;

	if(IS_KUSEG(vaddr))
		result = vfault(vaddr);
	else if(IS_KPTESEG(vaddr))
		result = tfault(ep, code, vaddr, cause,
				((utlbmiss_pending == 0) ? 0 : 1));
	else if(iskvir(vaddr)) {
		register pde_t *pdptr;
		kvmiss++;
		pdptr = kvtokptbl(vaddr);
		if(pdptr->pgm.pg_vr) {
			tlbdropin(0, vaddr, pdptr->pgi.pg_pde);
			result = 0;
		} else {
			cmn_err(CE_PANIC,"invalid kv tlbmiss @0x%x at pc 0x%x",
				vaddr, ep[EF_EPC]);
		}
	} else cmn_err(CE_PANIC,"unknown tlbmiss, addr = %x", vaddr);

	/*
	 *	restore nofault
	 */
	nofault = saved_nofault;
	nofault_cause = saved_nofault_cause;
	nofault_badvaddr = saved_nofault_badvaddr;

	return(result);
}

tlbmod(ep, code, vaddr, cause) 
uint *ep; 
uint code, vaddr, cause;
{
	int	result;
	int	saved_nofault;
	int	saved_nofault_cause;
	int	saved_nofault_badvaddr;

	ASSERT(ep[EF_SP] != (uint)ep); /* XXX */

	if(IS_KUSEG(vaddr)) {
		/*
		 *	save nofault across possible reuse
		 */
		saved_nofault = nofault;
		saved_nofault_cause = nofault_cause;
		saved_nofault_badvaddr = nofault_badvaddr;
		nofault = 0;

		result = pfault(vaddr);

		/*
		 *	restore nofault
		 */
		nofault = saved_nofault;
		nofault_cause = saved_nofault_cause;
		nofault_badvaddr = saved_nofault_badvaddr;

		return(result);
	};
	cmn_err(CE_PANIC,"tlbmod not in kuseg");
}

/* return 0 if fault correctable */

usrxmemflt(ep,code,sr,cause)
int *ep, code, sr, cause;
{
	/* By the time usrxmemflt is called, tfault and/or vfault
	 * has already had a chance to validate the page. The
	 * only thing left is to grow the stack.
	 */
	if((ep[EF_SP] & 0x80000000) == 0 &&
		grow(ep[EF_SP]))
		return(0);
	return 1;
}

krnlflt(ep, code, vaddr, cause)
uint *ep;
{
	splall() ;
	dumpregs(ep, code, vaddr, cause);
	cmn_err(CE_PANIC, "KERNEL MODE FAULT") ;
}

int iplmask[8] = {
	SR_IMASK1|SR_IEC,
	SR_IMASK2|SR_IEC,
	SR_IMASK3|SR_IEC,
	SR_IMASK4|SR_IEC,
	SR_IMASK5|SR_IEC,
	SR_IMASK6|SR_IEC,
	SR_IMASK7|SR_IEC,
	SR_IMASK8|SR_IEC
};
/* for RS2030 (or any other machine) to disallow any interrupt */
/* used on pizzaz for iil and ail calls */
unsigned ipl_special_mask = 0xffffffff;

/*
 * TODO: pending interrupts now come for CAUSE reg which is passed as arg4
 */

extern volatile int idleflag;

int nested_interrupt = 0;

intr(ep, code, sr, cause)
uint *ep;
uint code, sr, cause;
{
	register req;
	register prevmask;
	int	saved_nofault;
	int	saved_nofault_cause;
	int	saved_nofault_badvaddr;
#ifdef	RAMBO_NOT_INSTALLED_ON_RX3030
	static int	rambo_changes = 0;
	int		rambo_time;
#endif

#ifdef	RAMBO_NOT_INSTALLED_ON_RX3030
	rambo_time = *(volatile int *)0xbc000c00;
#endif
	ASSERT(ep[EF_SP] != (uint)ep); /* XXX */

	nested_interrupt++;

	prevmask = (sr & SR_IMASK);
	cause &= prevmask;
	/*
	 *	save nofault across possible wait
	 */
	saved_nofault = nofault;
	saved_nofault_cause = nofault_cause;
	saved_nofault_badvaddr = nofault_badvaddr;
	nofault = 0;

#ifdef	RAMBO_NOT_INSTALLED_ON_RX3030
	if ((rambo_changes == 0) && (rambo_time != *(volatile int *)0xbc000c00)) {
		printf("Rambo installed\n");
		rambo_changes = 1;
	}
	if (!rambo_changes)
		while (get_cause() & 0x1000);	/* wait for rambo to go away */
#endif
	/*
	 * Should probably direct vector these
	 */
	while (req = ffintr(cause)) {
		cnt.v_intr++;
		req--;
		MONITOR('I', req, ep[EF_EPC], 0, 0); /* XXX */
		reset_sr(iplmask[req]&ipl_special_mask);
		(*c0vec_tbl[req])(ep);
#if 1
/*
 * we want some combination of the two following senarios;
 * we want to service ALL the current interrupts, but also allow
 * new ones through.
 * - We also want it to be fast!!! - Rex
 */
		cause &= ~(1 << (req + CAUSE_IPSHIFT));
#else
		cause = get_cause();
#endif
	}

	/* XXX looking for timing or race condition: rwn */
	ASSERT((ep[EF_SR] & SR_IEC) == 0); /* XXX */

	/*
	 *	restore nofault
	 */
	nofault = saved_nofault;
	nofault_cause = saved_nofault_cause;
	nofault_badvaddr = saved_nofault_badvaddr;

	nested_interrupt--;

	/* kick us out of idle */
	idleflag = 0;
}

/*
 * Masks and constants for the rs field of "coprocessor instructions" (25-21)
 * which are branch on coprocessor condition instructions.
 */
#define	COPz_BC_MASK	0x1a
#define COPz_BC		0x08

/*
 * Masks and constants for the rt field of "branch on coprocessor condition
 * instructions" (20-16).
 */
#define	COPz_BC_TF_MASK	0x01
#define	COPz_BC_TRUE	0x01
#define	COPz_BC_FALSE	0x00

#define	PC_JMP_MASK	0xf0000000

/*
 * emulate_branch is used by fp_intr() to calculate the resulting pc of a
 * branch instruction.  It is passed a pointer to the exception frame,
 * the branch instruction and the floating point control and status register.
 * The routine returns the resulting pc.  This routine will panic() if it
 * is called with a non-branch instruction or one it does not know how to
 * emulate.
 */
unsigned
emulate_branch(ep, instr, fpc_csr)
u_int *ep;
unsigned instr;
union fpc_csr fpc_csr;
{
    union mips_instruction cpu_instr;
    long condition;
    long rs, rt;

	cpu_instr.word = instr;

	/*
	 * The values for the rs and rt registers are taken from the exception
	 * frame and since there is space for the 4 argument save registers and
	 * doesn't save register zero this is accounted for (the +3).
	 */
	if(cpu_instr.r_format.rs == 0)
	    rs = 0;
	else
	    rs = ep[cpu_instr.r_format.rs + 3];
	if(cpu_instr.r_format.rt == 0)
	    rt = 0;
	else
	    rt = ep[cpu_instr.r_format.rt + 3];

	switch(cpu_instr.i_format.opcode){

	case spec_op:
	    switch(cpu_instr.r_format.func){
	    case jalr_op:
		/* r31 has already been updated by the hardware */
	    case jr_op:
		return(rs);
	    }
	    break;

	case jal_op:
	    /* r31 has already been updated by the hardware */
	case j_op:
	    return(((ep[EF_EPC] + 4) & PC_JMP_MASK) |
		   (cpu_instr.j_format.target << 2));

	case beq_op:	case beql_op:
	    condition = rs == rt;
	    goto conditional;

	case bne_op:	case bnel_op:
	    condition = rs != rt;
	    goto conditional;

	case blez_op:	case blezl_op:
	    condition = rs <= 0;
	    goto conditional;

	case bgtz_op:	case bgtzl_op:
	    condition = rs > 0;
	    goto conditional;

	case bcond_op:
	    switch(cpu_instr.r_format.rt){

	    case bltzal_op:	case bltzall_op:
		/* r31 has already been updated by the hardware */
	    case bltz_op:	case bltzl_op:
		condition = rs < 0;
		goto conditional;

	    case bgezal_op:	case bgezall_op:
		/* r31 has already been updated by the hardware */
	    case bgez_op:	case bgezl_op:
		condition = rs >= 0;
		goto conditional;
	    }
	    break;

	case cop1_op:
	    if((cpu_instr.r_format.rs & COPz_BC_MASK) == COPz_BC){
		if((cpu_instr.r_format.rt & COPz_BC_TF_MASK) == COPz_BC_TRUE)
		    condition = fpc_csr.fc_struct.condition;
		else
		    condition = !(fpc_csr.fc_struct.condition);
		goto conditional;
	    }

	}
	/*
	 * For all other instructions (including branch on co-processor 2 & 3)
	 * we return 1, which will eventually post a SIGILL.
	 */
	 return(1);

conditional:
	if(condition)
	    return(ep[EF_EPC] + 4 + (cpu_instr.i_format.simmediate << 2));
	else
	    return(ep[EF_EPC] + 8);
}

/*
 * Fixade() is called to fix unaligned loads and stores.  It returns a
 * zero value if can't fix it and non-zero if it can fix it.  It modifies
 * the destination register (general or floating-point) for loads or the
 * destination memory location for stores.  Also the epc is advanced past
 * the instruction (possibly to the target of a branch).
 */
fixade(ep, cause)
register u_int *ep;
u_int cause;
{
    union mips_instruction inst, branch_inst;
    u_int addr, new_epc, word;
    int error;


	if(cause & CAUSE_BD){
	    branch_inst.word = fuiword((caddr_t)ep[EF_EPC]);
	    inst.word = fuiword((caddr_t)(ep[EF_EPC] + 4));
	    if(branch_inst.i_format.opcode == cop1_op)
		checkfp(u.u_procp, 0);
	    new_epc = emulate_branch(ep, branch_inst.word, u.u_pcb.pcb_fpc_csr);
	}
	else{
	    inst.word = fuiword((caddr_t)ep[EF_EPC]);
	    new_epc = ep[EF_EPC] + 4;
	}
	
	if (new_epc == 1) /* illegal branch instruction found */
	    return(0);

	addr = REGVAL(inst.i_format.rs) + inst.i_format.simmediate;

	/*
	 * The addresses of both the left and right parts of the reference
	 * have to be checked.  If either is a kernel address it is an
	 * illegal reference.
	 */
	if(addr >= K0BASE || addr+3 >= K0BASE)
	    return(0);

	error = 0;

	switch(inst.i_format.opcode){
	case lw_op:
	case ll_op:
	    error = uload_word(addr, &word);
	    if(inst.i_format.rt == 0)
		break;
	    else
	    	ep[inst.i_format.rt+3] = word;
	    break;
	case lh_op:
	    error = uload_half(addr, &word);
	    if(inst.i_format.rt == 0)
		break;
	    else
	    	ep[inst.i_format.rt+3] = word;
	    break;
	case lhu_op:
	    error = uload_uhalf(addr, &word);
	    if(inst.i_format.rt == 0)
		break;
	    else
	    	ep[inst.i_format.rt+3] = word;
	    break;
	case lwc1_op:
	    checkfp(u.u_procp, 0);
	    error = uload_word(addr, &word);
	    u.u_pcb.pcb_fpregs[inst.i_format.rt] = word;
	    break;
	case ldc1_op:
	    checkfp(u.u_procp, 0);
	    error = uload_word(addr, &word);
	    u.u_pcb.pcb_fpregs[inst.i_format.rt+1] = word;
	    if (!error) {
		error = uload_word(addr+4, &word);
		u.u_pcb.pcb_fpregs[inst.i_format.rt] = word;
	    }
	    break;

	case sw_op:
	case sc_op:
	    error = ustore_word(addr, REGVAL(inst.i_format.rt));
	    break;
	case sh_op:
	    error = ustore_half(addr, REGVAL(inst.i_format.rt));
	    break;
	case swc1_op:
	    checkfp(u.u_procp, 0);
	    error = ustore_word(addr, u.u_pcb.pcb_fpregs[inst.i_format.rt]);
	    break;
	case sdc1_op:
	    checkfp(u.u_procp, 0);
	    error = ustore_word(addr, u.u_pcb.pcb_fpregs[inst.i_format.rt+1]);
	    if (!error) {
		error = ustore_word(addr+4,
				    u.u_pcb.pcb_fpregs[inst.i_format.rt]);
	    }
	    break;

	default:
	    return(0);
	}
	
	if(error)
	    return(0);

	ep[EF_EPC] = new_epc;
	return(1);
}

r3030_report()
{
	register volatile struct rambo *rp = 
		(struct rambo *)PHYS_TO_K1(RAMBO_BASE);
	int	chan;

	printf("Parity status register 0x%x\n", rp->rambo_ereg);
	printf("Parity control register 0x%x\n", rp->rambo_creg);
	for (chan=0; chan < 2; chan++) {
		printf(" channel %d mode reg 0x%x\n", chan, rp->rambo_ch[chan].dma_mode);
	}
	pkbd_report();
}
