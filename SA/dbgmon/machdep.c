#ident "$Header: machdep.c,v 1.3 90/01/11 14:23:58 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * machdep.c -- machine dependent dbgmon routines
 */

#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "prom/entrypt.h"
#include "saio/saioctl.h"
#include "dbgmon/dbgmon.h"

/* IDPROM -- same 'part' on all machines so far -- saio/saio.c */

int IDPROM_ADDR[] = {
    PHYS_TO_K1(IDPROM_R2300),		/* M500 */
    PHYS_TO_K1(IDPROM_R2300),		/* M800 */
    PHYS_TO_K1(IDPROM_R2300),		/* M1000 */
    PHYS_TO_K1(IDPROM_R2400),		/* M120 */
    PHYS_TO_K1(IDPROM_R3200),		/* M2000 */
    PHYS_TO_K1(IDPROM_R6300)		/* Excalicur */
  };

/*
 * _regs -- software copy of machine registers
 * copied to hardware registers by _resume()
 */
extern int _regs[];

static struct vector {
	int	inst1;	/* lui k0,0x???? */
	int	inst2;	/* addu k0,0x???? */
	int	inst3;	/* j	k0 */
	int	inst4;	/* nop */
};

static struct vector evec_save;
static struct vector utvec_save;

/*
 * _save_vectors -- save client vectors
 */
_save_vectors()
{
	clear_cache(E_VEC, sizeof(struct vector));
	evec_save = *(struct vector *)E_VEC;
	clear_cache(UT_VEC, sizeof(struct vector));
	utvec_save = *(struct vector *)UT_VEC;
	_hook_exceptions();
}

/*
 * restore_vectors -- restore client vectors
 */
_restore_vectors()
{
	*(struct vector *)E_VEC = evec_save;
	clear_cache(E_VEC, sizeof(struct vector));
	*(struct vector *)UT_VEC = utvec_save;
	clear_cache(UT_VEC, sizeof(struct vector));
}

/*
 * _get_memory -- read memory location as indicated by flags
 * ??? SHOULD THIS FLUSH CACHE BEFORE DOING READ ???
 */
unsigned
_get_memory(caddr, width)
unsigned caddr;
{
	register struct restart_blk *rb = (struct restart_blk *)RESTART_ADDR;
	unsigned paddr;

	if ((IS_KUSEG(caddr) || IS_KSEG2(caddr))
	    && rb->rb_magic == RESTART_MAGIC && rb->rb_vtop) {
		paddr = (*rb->rb_vtop)(caddr);
		if (paddr != -1)
			caddr = paddr;
	}

	switch (width) {

	case SW_BYTE:
		return (*(unsigned char *)caddr);

	case SW_HALFWORD:
		return (*(unsigned short *)caddr);

	case SW_WORD:
		return(*(unsigned *)caddr);
	
	default:
		_dbg_error("get_memory: Illegal width");
	}
}

/*
 * set_memory -- set memory location as indicated by flags
 */
_set_memory(caddr, width, value)
unsigned caddr;
{
	register struct restart_blk *rb = (struct restart_blk *)RESTART_ADDR;
	unsigned paddr;

	if ((IS_KUSEG(caddr) || IS_KSEG2(caddr))
	    && rb->rb_magic == RESTART_MAGIC && rb->rb_vtop) {
		paddr = (*rb->rb_vtop)(caddr);
		if (paddr != -1)
			caddr = paddr;
	}

	switch (width) {

	case SW_BYTE:
		*(unsigned char *)caddr = value;
		break;

	case SW_HALFWORD:
		*(unsigned short *)caddr = value;
		break;

	case SW_WORD:
		*(unsigned *)caddr = value;
		break;
	
	default:
		_fatal_error("set_memory: Illegal switch");
	}
	clear_cache(caddr, width);
}

/*
 * _get_register -- read processor register as indicated by flags
 */
unsigned
_get_register(reg)
{
	if (reg >= (int)NREGS)
		_dbg_error("invalid register number");

	return(_regs[reg]);
}

/*
 * set_register -- write processor register as indicated by flags
 */
_set_register(reg, value)
{
	if (reg >= (int)NREGS)
		_dbg_error("invalid register number");

	_regs[reg] = value;
}
