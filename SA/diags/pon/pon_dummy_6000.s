#ident "$Header: pon_dummy_6000.s,v 1.3.7.1 90/07/18 14:30:43 huang Exp $"
/* $Copyright$ */

/*
 *	Entrypoints for routines which have no meaning for the R6000.
 */

#include "mips/asm.h"
#include "mips/regdef.h"
#include "pon.h"

		BSS(f_slot,8)

		.text

LEAF(Pon_LanceMaster)
	li	v0,PASS
	j	ra
	END(Pon_LanceMaster)

LEAF(Pon_LanceSlave)
	li	v0,PASS
	j	ra
	END(Pon_LanceSlave)

LEAF(Pon_Parity)
	li	v0,PASS
	j	ra
	END(Pon_Parity)

LEAF(Pon_Scr)
	li	v0,PASS
	j	ra
	END(Pon_Scr)

LEAF(Pon_ScsiMaster)
	li	v0,PASS
	j	ra
	END(Pon_ScsiMaster)

LEAF(Pon_ScsiSlave)
	li	v0,PASS
	j	ra
	END(Pon_ScsiSlave)

LEAF(Pon_UdcSlave)
	li	v0,PASS
	j	ra
	END(Pon_UdcSlave)

LEAF(Pon_Vme)
	li	v0,PASS
	j	ra
	END(Pon_Vme)

LEAF(Pon_WB)
	li	v0,PASS
	j	ra
	END(Pon_WB)
