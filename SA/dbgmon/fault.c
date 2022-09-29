#ident "$Header: fault.c,v 1.5 90/06/22 11:09:22 rpharris Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * fault.c -- saio/dbgmon fault handling routines
 */

#include "sys/param.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
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
extern unsigned _sp_save;		/* sp at time of exception */
extern unsigned _sr_save;		/* sr at time of exception */
extern unsigned _error_save;		/* error reg at time of exception */

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

int show_fault();

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

	_check_bp();		/* give debug monitor a shot at it */
	if (nofault) {
		jbp = nofault;
		nofault = 0;
		longjmp(jbp, 1);
	}
	clear_nofault();
	show_fault();
	_restart();
}

/*
 * show_fault -- display info pertinent to fault
 */
show_fault()
{
	int regstyle;

	/* just return if its a breakpoint exception! */
	if ((_cause_save & CAUSE_EXCMASK) == EXC_BREAK)
		return;
	printf("\nEXCEPTION: %r\n", _exc_save, except_desc);
	_show_inst(_epc_save);	/* try to call disassembler */
	printf("Cause register: %R\n", _cause_save, cause_desc);
	printf("Status register: %R\n", _sr_save
	      ,(IS_R6300) ? sr_desc_r6000 : sr_desc);
	if (_exc_save == EXCEPT_UTLB)
		goto printvaddr;
	switch (_cause_save & CAUSE_EXCMASK) {
	case EXC_DBE:	case EXC_DBL_NC:
		if (IS_R6300)
			goto printvaddr;	/* is valid for R6000 */
		break;
	case EXC_MOD:	case EXC_RMISS:	case EXC_WMISS:
	case EXC_RADE:	case EXC_WADE:
printvaddr:
		printf("Bad Vaddress: 0x%x\n", _badvaddr_save);
		break;
	}
	if (IS_R6300) {
		printf("ErrorReg: %R\n", _error_save, error_desc_r6000);
	}
	/*
	 * Only print write bus errors if the client was enabled for them,
	 * otherwise we might swipe on that was intended for client
	 */
	if ((_sr_save & SR_IBIT8) && (_cause_save & CAUSE_IP8))
		printf("Write Bus Error Address: 0x%x\n",
		    *(int *)PHYS_TO_K1(SBE_ADDR));
}
