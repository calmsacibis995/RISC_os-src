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
/* $Header: fp_instr.s,v 1.3.2.2 90/05/10 01:25:03 wje Exp $ */

#include "regdef.h"
#include "asm.h"

LEAF(sqrtf_instr)
	sqrt.s	$f0,$f12
	j	ra
END(sqrtf_instr)

LEAF(sqrtd_instr)
	sqrt.d	$f0,$f12
	j	ra
END(sqrtd_instr)

LEAF(absf_instr)
	abs.s	$f0,$f12
	j	ra
END(absf_instr)

LEAF(absd_instr)
	abs.d	$f0,$f12
	j	ra
END(absd_instr)

LEAF(cvtw_f_instr)
	cvt.w.s	$f0,$f12
	mfc1	a0,$f0
	j	ra
END(cvtw_f_instr)

LEAF(cvtw_d_instr)
	cvt.w.d	$f0,$f12
	mfc1	a0,$f0
	j	ra
END(cvtw_d_instr)

LEAF(cmpf_instr)
	lw	a1, cmpf_tab(a2)
	j	a1
	.data
cmpf_tab:
	.word 	f_f:1, f_un:1, f_eq:1, f_ueq:1, f_olt:1, f_ult:1, f_ole:1
	.word	f_ule:1, f_sf:1, f_ngle:1, f_seq:1, f_ngl:1, f_lt:1, f_nge:1
	.word	f_le:1, f_ngt:1
	.text
f_f:	c.f.s		$f12,$f14
	j	ra
f_un:	c.un.s		$f12,$f14
	j	ra
f_eq:	c.eq.s		$f12,$f14
	j	ra
f_ueq:	c.ueq.s		$f12,$f14
	j	ra
f_olt:	c.olt.s		$f12,$f14
	j	ra
f_ult:	c.ult.s		$f12,$f14
	j	ra
f_ole:	c.ole.s		$f12,$f14
	j	ra
f_ule:	c.ule.s		$f12,$f14
	j	ra
f_sf:	c.sf.s		$f12,$f14
	j	ra
f_ngle:	c.ngle.s	$f12,$f14
	j	ra
f_seq:	c.seq.s		$f12,$f14
	j	ra
f_ngl:	c.ngl.s		$f12,$f14
	j	ra
f_lt:	c.lt.s		$f12,$f14
	j	ra
f_nge:	c.nge.s		$f12,$f14
	j	ra
f_le:	c.le.s		$f12,$f14
	j	ra
f_ngt:	c.ngt.s		$f12,$f14
	j	ra
END(cmpf_instr)

LEAF(cmpd_instr)
	lw	a0,16(sp)
	lw	a1, cmpd_tab(a0)
	j	a1
	.data
cmpd_tab:
	.word 	d_f:1, d_un:1, d_eq:1, d_ueq:1, d_olt:1, d_ult:1, d_ole:1
	.word	d_ule:1, d_sf:1, d_ngle:1, d_seq:1, d_ngl:1, d_lt:1, d_nge:1
	.word	d_le:1, d_ngt:1
	.text
d_f:	c.f.d		$f12,$f14
	j	ra
d_un:	c.un.d		$f12,$f14
	j	ra
d_eq:	c.eq.d		$f12,$f14
	j	ra
d_ueq:	c.ueq.d		$f12,$f14
	j	ra
d_olt:	c.olt.d		$f12,$f14
	j	ra
d_ult:	c.ult.d		$f12,$f14
	j	ra
d_ole:	c.ole.d		$f12,$f14
	j	ra
d_ule:	c.ule.d		$f12,$f14
	j	ra
d_sf:	c.f.s		$f12,$f14
	j	ra
d_ngle:	c.ngle.d	$f12,$f14
	j	ra
d_seq:	c.eq.s		$f12,$f14
	j	ra
d_ngl:	c.ngl.d		$f12,$f14
	j	ra
d_lt:	c.lt.d		$f12,$f14
	j	ra
d_nge:	c.nge.d		$f12,$f14
	j	ra
d_le:	c.le.d		$f12,$f14
	j	ra
d_ngt:	c.ngt.d		$f12,$f14
	j	ra
END(cmpd_instr)
