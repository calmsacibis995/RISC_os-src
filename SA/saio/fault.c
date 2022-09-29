#ident "$Header: fault.c,v 1.14 90/05/22 15:02:02 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * fault.c -- saio fault handling routines
 */

#include "sys/param.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

/*
 * nofault -- jmp_buf pointer for fault handling
 */
int *nofault;

/*
 * exception state -- saved by exception code in csu.s
 */
extern unsigned _epc_save;		/* epc at time of exception */
extern unsigned _at_save;		/* at at time of exception */
extern unsigned _v0_save;		/* v0 at time of exception */
extern unsigned _exc_save;		/* exception "vector" */
extern unsigned _badvaddr_save;		/* badvaddr at time of exception */
extern unsigned _cause_save;		/* cause reg at time of exception */
extern unsigned _error_save;		/* error reg at time of exception */
extern unsigned _sp_save;		/* sp at time of exception */
extern unsigned _sr_save;		/* sr at time of exception */
extern unsigned	_stack_mode;
extern unsigned _fp_test_on;		/* flag for fpu exception handling
					   during FPU power-on test */
extern _regs[];
/*
 * register descriptions
 */
extern struct reg_desc sr_desc[], sr_desc_r6000[], cause_desc[];
extern struct reg_desc error_desc_r6000[];

static struct reg_values except_values[] = {
	{ EXCEPT_UTLB,	"UTLBMiss" },
	{ EXCEPT_NORM,	"NORMAL" },
	{ EXCEPT_BRKPT,	"BREAKPOINT" },
	{ 0,		NULL }
};

static struct reg_desc except_desc[] = {
	/* mask	     shift      name   format  values */
	{ -1,		0,	"vector",NULL,	except_values },
	{ 0,		0,	NULL,	NULL,	NULL }
};

static struct vector {
	int	inst1;	/* lui k0,0x???? */
	int	inst2;	/* addu k0,0x???? */
	int	inst3;	/* j	k0 */
	int	inst4;	/* nop */
};

void show_fault();

/*
 * _hook_exceptions -- point exception vector to prom exception handler
 */
_hook_exceptions()
{
	extern int _j_exceptutlb[];
	extern int _j_exceptnorm[];

	nofault = 0;
	*(struct vector *)UT_VEC = *(struct vector *)_j_exceptutlb;
	clear_cache(UT_VEC, sizeof(struct vector));
	*(struct vector *)E_VEC = *(struct vector *)_j_exceptnorm;
	clear_cache(E_VEC, sizeof(struct vector));
}

/*
 * _exception_handler -- saio library fault handler
 */
_exception_handler()
{
	int *jbp;
  	unsigned long branch_instr;

	if (nofault) {
		jbp = nofault;
		nofault = 0;
		_stack_mode = MODE_NORMAL;	/* reset the stack mode! */
		longjmp(jbp, 1);
	}
	clear_nofault();
	if (_fp_test_on) {
  		if (((_cause_save & CAUSE_EXCMASK) == EXC_INT)) {
       			if (!(_cause_save & CAUSE_IP6)) {
				printf("PHANTOM INTERRUPT\n");
				show_fault();
				_exit(-1);
       			/*	_resume();	/* return at the right place */
			}
	/*
	 * Get the floating point instruction.  If the floating point unit
	 * is the board implementation it's in the fpc_eir.  Else it is at
	 * the exception program counter as modified by the branch delay
	 * bit in the cause register.
	 * Now if the floating-point instruction was in a branch delay slot
	 * then emulate the branch to determine the target pc to return to.
	 * Else the target pc to return to is just address of the next
	 * instruction.
	 */
	    		if(_regs[R_CAUSE] & CAUSE_BD) {
				branch_instr = *(unsigned long *)(_epc_save);
				printf( "Error: Can't emulate branch instruction: 0x%x at: 0x%x\n", branch_instr, _epc_save);
				show_fault();
	    		} else {
	        		_regs[R_EPC] =_epc_save + sizeof(unsigned long);
	    		}
       			_resume();	/* return at the right place */
		} else {
			show_fault();
       			_exit(-1);
 		}
 	} else {
		show_fault();
		_exit(-1);
 	}
}

/*
 * show_fault -- display info pertinent to fault
 */
void
show_fault()
{
	extern int ignore_xoff;
	unsigned int far;
	unsigned short fid;

	ignore_xoff = 0;
	printf("\nEXCEPTION: %r\n", _exc_save, except_desc);
	printf("Exception pc: 0x%x\n", _epc_save);
	printf("Cause register: %R\n", _cause_save, cause_desc);
	printf("Status register: %R\n", _sr_save
	      ,(IS_R6300) ? sr_desc_r6000 : sr_desc);
#ifdef PROM
#ifdef R3030
	if (_cause_save & CAUSE_IP5) show_rambo();
#endif
#endif
	if (_exc_save == EXCEPT_UTLB)
		goto printvaddr;
	switch (_cause_save & CAUSE_EXCMASK) {
	case EXC_IBE:	case EXC_DBE:	case EXC_DBL_NC:
		if (IS_R6300)
			goto printvaddr;
		break;
	case EXC_MOD:	case EXC_RMISS:	case EXC_WMISS:
	case EXC_RADE:	case EXC_WADE:
printvaddr:
		printf("Bad Vaddress: 0x%x\n", _badvaddr_save);
		break;
	}
	if (IS_R2400) {
		fid = *(ushort *)PHYS_TO_K1(FID);
		far = *(unsigned int *)PHYS_TO_K1(FAR);
		printf("FAR: 0x%x FID: 0x%x\n", far, fid);
	} else
	if (IS_R6300) {
		printf("ErrorReg: %R\n", _error_save, error_desc_r6000 );
	} else
		if (_cause_save & CAUSE_IP8)
			printf("Write Bus Error Address: 0x%x\n",
			    *(int *)PHYS_TO_K1(SBE_ADDR));
}


#ifdef PROM
#ifdef R3030
#define	RAM_ERROR_REG	0x1c000e00
#define	RAM_CHAN1_MODE	0x1c000300
#define	RAM_CHAN2_MODE	0x1c000900

show_rambo()
{
    int *ramptr;
    int error_reg, chan1_mode, chan2_mode;

    ramptr = (int *)PHYS_TO_K1(RAM_ERROR_REG);
    error_reg = *ramptr;
    ramptr = (int *)PHYS_TO_K1(RAM_CHAN1_MODE);
    chan1_mode = *ramptr;
    ramptr = (int *)PHYS_TO_K1(RAM_CHAN2_MODE);
    chan2_mode = *ramptr;
    printf("RAMBO Regs:  Error=0x%x  Ch1Mode=0x%x  Ch2Mode=0x%x\n",
	error_reg, chan1_mode, chan2_mode);
}
#endif
#endif

