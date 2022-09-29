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
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: fp_control.s,v 1.4.2.2 90/05/10 01:24:31 wje Exp $ */

/*
 * This file contains routines to get and set the floating point control
 * registers.
 */
#include "regdef.h"
#include "asm.h"
#include "sys/fpu.h"

/*
 * get_fpc_csr returns the fpc_csr.
 */
LEAF(get_fpc_csr)
	cfc1	v0,fpc_csr
	j	ra
	END(get_fpc_csr)

/*
 * set_fpc_csr sets the fpc_csr and returns the old fpc_csr.
 */
LEAF(set_fpc_csr)
	cfc1	v0,fpc_csr
	ctc1	a0,fpc_csr
	j	ra
	END(set_fpc_csr)

/*
 * get_fpc_irr returns the fpc_irr.
 */
LEAF(get_fpc_irr)
	cfc1	v0,fpc_irr
	j	ra
	END(get_fpc_irr)

/*
 * set_fpc_led sets the floating board leds.
 */
LEAF(set_fpc_led)
	ctc1	a0,fpc_led
	j	ra
	END(set_fpc_led)

/*
 * get_fpc_eir returns the fpc_eir.
 */
LEAF(get_fpc_eir)
	cfc1	v0,fpc_eir
	j	ra
	END(get_fpc_eir)
