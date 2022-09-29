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
/* $Header: softfp.h,v 1.6.3.2 90/05/10 04:43:27 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * softfp.h -- constants for software floating point emulation
 */

/*
 * The _MASK's are used to get a the specified field after it has been
 * shifted by _SHIFT and then bit patterns (like _COPN) can be used to test
 * the field.
 */
/* constants for the OPCODE field for some general instructions */
#define	BSD43_OPCODE_SHIFT	26
#define	BSD43_OPCODE_MASK	0x3f
#define	BSD43_OPCODE_SPECIAL	0x00
#define	BSD43_OPCODE_BCOND	0x01
#define	BSD43_OPCODE_J	0x02
#define	BSD43_OPCODE_JAL	0x03
#define	BSD43_OPCODE_BEQ	0x04
#define	BSD43_OPCODE_C1	0x11

/* constants for the emulating jump or jump and link instructions */
#define	BSD43_TARGET_MASK	0x03ffffff
#define	BSD43_PC_JMP_MASK	0xf0000000

/* constants for the FUNC field for some general instructions */
#define	BSD43_FUNC_MASK	0x3f
#define	BSD43_FUNC_JR		0x08
#define	BSD43_FUNC_JALR	0x09

/*
 * constants for the OPCODE field for detecting all general branch
 * (beq,bne,blez,bgtz) instructions and all coprocessor instructions.
 */
#define	BSD43_BRANCH_MASK	0x3c
#define	BSD43_OPCODE_BRANCHES	0x04
#define	BSD43_COPN_MASK	0x3c
#define	BSD43_OPCODE_COPN	0x10

/* constants for load/store COPN instructions */
#define	BSD43_OP_LSWCOPNMASK	0x37
#define	BSD43_OP_LSWCOPN	0x31
#define BSD43_OP_LSBITMASK	0x8
#define BSD43_OP_LBIT		0x0

/* constants for branch on COPN condition instructions */
#define	BSD43_COPN_BCSHIFT	24
#define	BSD43_COPN_BCMASK	0x3
#define	BSD43_COPN_BC		0x1
#define	BSD43_BC_TFBITSHIFT	16
#define	BSD43_BC_TFBITMASK	0x1
#define BSD43_BC_FBIT		0x0

/* constants for move to/from COPN instructions */
#define	BSD43_COPN_MTFSHIFT	25
#define	BSD43_COPN_MTFMASK	0x1
#define	BSD43_COPN_MTF	0x0
#define	BSD43_COPN_MTFBITSHIFT	23
#define	BSD43_COPN_MTFBITMASK	0x1
#define BSD43_COPN_MFBIT	0x0

/* constants for move control registers to/from CP1 instructions */
#define BSD43_M_CONBITSHIFT	22
#define	BSD43_M_CONBITMASK	0x1

#define BSD43_FPR_REV		0
#define BSD43_FPR_EIR		30
#define BSD43_FPR_CSR		31
#define	BSD43_SOFTFP_REVWORD	0x0

/*
 * These constants refer to the fields of coprocessor instructions not
 * cpu instructions (ie the RS and RD fields are different).
 */
#define BSD43_BASE_SHIFT	21
#define BSD43_BASE_MASK	0x1f
#define BSD43_RT_SHIFT	16
#define	BSD43_RT_MASK		0x1f
#define	BSD43_RT_FPRMASK	0x1e
#define BSD43_RS_SHIFT	11
#define	BSD43_RS_MASK		0x1f
#define	BSD43_RS_FPRMASK	0x1e
#define BSD43_RD_SHIFT	6
#define	BSD43_RD_MASK		0x1f
#define	BSD43_RD_FPRMASK	0x1e

#define BSD43_IMMED_SHIFT	16

#define BSD43_C1_FMT_SHIFT	21
#define	BSD43_C1_FMT_MASK	0xf
#define BSD43_C1_FMT_SINGLE	0
#define BSD43_C1_FMT_DOUBLE	1
#define BSD43_C1_FMT_EXTENDED	2
#define BSD43_C1_FMT_QUAD	3
#define BSD43_C1_FMT_WORD	4
#define BSD43_C1_FMT_MAX	4

#define BSD43_C1_FUNC_MASK	0x3f
#define BSD43_C1_FUNC_DIV	3
#define BSD43_C1_FUNC_NEG	7
#define BSD43_C1_FUNC_CVTS	32
#define BSD43_C1_FUNC_CVTW	36
#define BSD43_C1_FUNC_1stCMP	48

#define BSD43_COND_UN_MASK	0x1
#define BSD43_COND_EQ_MASK	0x2
#define BSD43_COND_LT_MASK	0x4
#define BSD43_COND_IN_MASK	0x8

/*
 * These constants refer to fields in the floating-point status and control
 * register.
 */
#define	BSD43_CSR_CBITSHIFT	23
#define	BSD43_CSR_CBITMASK	0x1
#define	BSD43_CSR_CBITSET	0x00800000
#define	BSD43_CSR_CBITCLEAR	0xff7fffff

#define	BSD43_CSR_EXCEPT	0x0003f000
#define	BSD43_UNIMP_EXC	0x00020000
#define	BSD43_INVALID_EXC	0x00010040
#define	BSD43_DIVIDE0_EXC	0x00008020
#define	BSD43_OVERFLOW_EXC	0x00004010
#define	BSD43_UNDERFLOW_EXC	0x00002008
#define	BSD43_INEXACT_EXC	0x00001004

#define BSD43_CSR_ENABLE		0x00000f80
#define	BSD43_INVALID_ENABLE		0x00000800
#define	BSD43_DIVIDE0_ENABLE		0x00000400
#define	BSD43_OVERFLOW_ENABLE		0x00000200
#define	BSD43_UNDERFLOW_ENABLE	0x00000100
#define	BSD43_INEXACT_ENABLE		0x00000080

#define	BSD43_CSR_RM_MASK	0x3
#define	BSD43_CSR_RM_RN	0
#define	BSD43_CSR_RM_RZ	1
#define	BSD43_CSR_RM_RPI	2
#define	BSD43_CSR_RM_RMI	3

/*
 * These constants refer to floating-point values for all formats
 */
#define	BSD43_SIGNBIT		0x80000000

#define	BSD43_GUARDBIT	0x80000000
#define	BSD43_STKBIT		0x20000000

/*
 * These constants refer to word values
 */
#define	BSD43_WORD_MIN	0x80000000
#define	BSD43_WORD_MAX	0x7fffffff
#define	BSD43_WEXP_MIN	-1
#define	BSD43_WEXP_MAX	30
#define	BSD43_WQUIETNAN_LEAST	0x7fffffff

/*
 * These constants refer to single format floating-point values
 */
#define	BSD43_SEXP_SHIFT	23
#define	BSD43_SEXP_MASK	0xff
#define	BSD43_SEXP_NAN	0xff
#define	BSD43_SEXP_INF	0xff
#define	BSD43_SEXP_BIAS	127
#define	BSD43_SEXP_MAX	127
#define	BSD43_SEXP_MIN	-126
#define	BSD43_SEXP_OU_ADJ	192
#define	BSD43_SIMP_1BIT	0x00800000
#define	BSD43_SFRAC_LEAD0S	8
#define	BSD43_SFRAC_BITS	23
#define	BSD43_SFRAC_MASK	0x007fffff
#define	BSD43_SFRAC_LEAST_MAX	0x007fffff

#define	BSD43_SSNANBIT_MASK	0x00400000
#define	BSD43_SQUIETNAN_LEAST	0x7fbfffff

/*
 * These constants refer to double format floating-point values
 */
#define	BSD43_DEXP_SHIFT	20
#define	BSD43_DEXP_MASK	0x7ff
#define	BSD43_DEXP_NAN	0x7ff
#define	BSD43_DEXP_INF	0x7ff
#define	BSD43_DEXP_BIAS	1023
#define	BSD43_DEXP_MAX	1023
#define	BSD43_DEXP_MIN	-1022
#define	BSD43_DEXP_OU_ADJ	1536
#define	BSD43_DIMP_1BIT	0x00100000
#define	BSD43_DFRAC_LEAD0S	11
#define	BSD43_DFRAC_BITS	52
#define	BSD43_DFRAC_MASK	0x000fffff
#define	BSD43_DFRAC_LESS_MAX	0x000fffff
#define	BSD43_DFRAC_LEAST_MAX	0xffffffff

#define	BSD43_DSNANBIT_MASK	0x00080000
#define	BSD43_DQUIETNAN_LESS	0x7ff7ffff
#define	BSD43_DQUIETNAN_LEAST	0xffffffff

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BASE_MASK BSD43_BASE_MASK
#   define BASE_SHIFT BSD43_BASE_SHIFT
#   define BC_FBIT BSD43_BC_FBIT
#   define BC_TFBITMASK BSD43_BC_TFBITMASK
#   define BC_TFBITSHIFT BSD43_BC_TFBITSHIFT
#   define BRANCH_MASK BSD43_BRANCH_MASK
#   define C1_FMT_DOUBLE BSD43_C1_FMT_DOUBLE
#   define C1_FMT_EXTENDED BSD43_C1_FMT_EXTENDED
#   define C1_FMT_MASK BSD43_C1_FMT_MASK
#   define C1_FMT_MAX BSD43_C1_FMT_MAX
#   define C1_FMT_QUAD BSD43_C1_FMT_QUAD
#   define C1_FMT_SHIFT BSD43_C1_FMT_SHIFT
#   define C1_FMT_SINGLE BSD43_C1_FMT_SINGLE
#   define C1_FMT_WORD BSD43_C1_FMT_WORD
#   define C1_FUNC_1stCMP BSD43_C1_FUNC_1stCMP
#   define C1_FUNC_CVTS BSD43_C1_FUNC_CVTS
#   define C1_FUNC_CVTW BSD43_C1_FUNC_CVTW
#   define C1_FUNC_DIV BSD43_C1_FUNC_DIV
#   define C1_FUNC_MASK BSD43_C1_FUNC_MASK
#   define C1_FUNC_NEG BSD43_C1_FUNC_NEG
#   define COND_EQ_MASK BSD43_COND_EQ_MASK
#   define COND_IN_MASK BSD43_COND_IN_MASK
#   define COND_LT_MASK BSD43_COND_LT_MASK
#   define COND_UN_MASK BSD43_COND_UN_MASK
#   define COPN_BC BSD43_COPN_BC
#   define COPN_BCMASK BSD43_COPN_BCMASK
#   define COPN_BCSHIFT BSD43_COPN_BCSHIFT
#   define COPN_MASK BSD43_COPN_MASK
#   define COPN_MFBIT BSD43_COPN_MFBIT
#   define COPN_MTF BSD43_COPN_MTF
#   define COPN_MTFBITMASK BSD43_COPN_MTFBITMASK
#   define COPN_MTFBITSHIFT BSD43_COPN_MTFBITSHIFT
#   define COPN_MTFMASK BSD43_COPN_MTFMASK
#   define COPN_MTFSHIFT BSD43_COPN_MTFSHIFT
#   define CSR_CBITCLEAR BSD43_CSR_CBITCLEAR
#   define CSR_CBITMASK BSD43_CSR_CBITMASK
#   define CSR_CBITSET BSD43_CSR_CBITSET
#   define CSR_CBITSHIFT BSD43_CSR_CBITSHIFT
#   define CSR_ENABLE BSD43_CSR_ENABLE
#   define CSR_EXCEPT BSD43_CSR_EXCEPT
#   define CSR_RM_MASK BSD43_CSR_RM_MASK
#   define CSR_RM_RMI BSD43_CSR_RM_RMI
#   define CSR_RM_RN BSD43_CSR_RM_RN
#   define CSR_RM_RPI BSD43_CSR_RM_RPI
#   define CSR_RM_RZ BSD43_CSR_RM_RZ
#   define DEXP_BIAS BSD43_DEXP_BIAS
#   define DEXP_INF BSD43_DEXP_INF
#   define DEXP_MASK BSD43_DEXP_MASK
#   define DEXP_MAX BSD43_DEXP_MAX
#   define DEXP_MIN BSD43_DEXP_MIN
#   define DEXP_NAN BSD43_DEXP_NAN
#   define DEXP_OU_ADJ BSD43_DEXP_OU_ADJ
#   define DEXP_SHIFT BSD43_DEXP_SHIFT
#   define DFRAC_BITS BSD43_DFRAC_BITS
#   define DFRAC_LEAD0S BSD43_DFRAC_LEAD0S
#   define DFRAC_LEAST_MAX BSD43_DFRAC_LEAST_MAX
#   define DFRAC_LESS_MAX BSD43_DFRAC_LESS_MAX
#   define DFRAC_MASK BSD43_DFRAC_MASK
#   define DIMP_1BIT BSD43_DIMP_1BIT
#   define DIVIDE0_ENABLE BSD43_DIVIDE0_ENABLE
#   define DIVIDE0_EXC BSD43_DIVIDE0_EXC
#   define DQUIETNAN_LEAST BSD43_DQUIETNAN_LEAST
#   define DQUIETNAN_LESS BSD43_DQUIETNAN_LESS
#   define DSNANBIT_MASK BSD43_DSNANBIT_MASK
#   define FPR_CSR BSD43_FPR_CSR
#   define FPR_EIR BSD43_FPR_EIR
#   define FPR_REV BSD43_FPR_REV
#   define FUNC_JALR BSD43_FUNC_JALR
#   define FUNC_JR BSD43_FUNC_JR
#   define FUNC_MASK BSD43_FUNC_MASK
#   define GUARDBIT BSD43_GUARDBIT
#   define IMMED_SHIFT BSD43_IMMED_SHIFT
#   define INEXACT_ENABLE BSD43_INEXACT_ENABLE
#   define INEXACT_EXC BSD43_INEXACT_EXC
#   define INVALID_ENABLE BSD43_INVALID_ENABLE
#   define INVALID_EXC BSD43_INVALID_EXC
#   define M_CONBITMASK BSD43_M_CONBITMASK
#   define M_CONBITSHIFT BSD43_M_CONBITSHIFT
#   define OPCODE_BCOND BSD43_OPCODE_BCOND
#   define OPCODE_BEQ BSD43_OPCODE_BEQ
#   define OPCODE_BRANCHES BSD43_OPCODE_BRANCHES
#   define OPCODE_C1 BSD43_OPCODE_C1
#   define OPCODE_COPN BSD43_OPCODE_COPN
#   define OPCODE_J BSD43_OPCODE_J
#   define OPCODE_JAL BSD43_OPCODE_JAL
#   define OPCODE_MASK BSD43_OPCODE_MASK
#   define OPCODE_SHIFT BSD43_OPCODE_SHIFT
#   define OPCODE_SPECIAL BSD43_OPCODE_SPECIAL
#   define OP_LBIT BSD43_OP_LBIT
#   define OP_LSBITMASK BSD43_OP_LSBITMASK
#   define OP_LSWCOPN BSD43_OP_LSWCOPN
#   define OP_LSWCOPNMASK BSD43_OP_LSWCOPNMASK
#   define OVERFLOW_ENABLE BSD43_OVERFLOW_ENABLE
#   define OVERFLOW_EXC BSD43_OVERFLOW_EXC
#   define PC_JMP_MASK BSD43_PC_JMP_MASK
#   define RD_FPRMASK BSD43_RD_FPRMASK
#   define RD_MASK BSD43_RD_MASK
#   define RD_SHIFT BSD43_RD_SHIFT
#   define RS_FPRMASK BSD43_RS_FPRMASK
#   define RS_MASK BSD43_RS_MASK
#   define RS_SHIFT BSD43_RS_SHIFT
#   define RT_FPRMASK BSD43_RT_FPRMASK
#   define RT_MASK BSD43_RT_MASK
#   define RT_SHIFT BSD43_RT_SHIFT
#   define SEXP_BIAS BSD43_SEXP_BIAS
#   define SEXP_INF BSD43_SEXP_INF
#   define SEXP_MASK BSD43_SEXP_MASK
#   define SEXP_MAX BSD43_SEXP_MAX
#   define SEXP_MIN BSD43_SEXP_MIN
#   define SEXP_NAN BSD43_SEXP_NAN
#   define SEXP_OU_ADJ BSD43_SEXP_OU_ADJ
#   define SEXP_SHIFT BSD43_SEXP_SHIFT
#   define SFRAC_BITS BSD43_SFRAC_BITS
#   define SFRAC_LEAD0S BSD43_SFRAC_LEAD0S
#   define SFRAC_LEAST_MAX BSD43_SFRAC_LEAST_MAX
#   define SFRAC_MASK BSD43_SFRAC_MASK
#   define SIGNBIT BSD43_SIGNBIT
#   define SIMP_1BIT BSD43_SIMP_1BIT
#   define SOFTFP_REVWORD BSD43_SOFTFP_REVWORD
#   define SQUIETNAN_LEAST BSD43_SQUIETNAN_LEAST
#   define SSNANBIT_MASK BSD43_SSNANBIT_MASK
#   define STKBIT BSD43_STKBIT
#   define TARGET_MASK BSD43_TARGET_MASK
#   define UNDERFLOW_ENABLE BSD43_UNDERFLOW_ENABLE
#   define UNDERFLOW_EXC BSD43_UNDERFLOW_EXC
#   define UNIMP_EXC BSD43_UNIMP_EXC
#   define WEXP_MAX BSD43_WEXP_MAX
#   define WEXP_MIN BSD43_WEXP_MIN
#   define WORD_MAX BSD43_WORD_MAX
#   define WORD_MIN BSD43_WORD_MIN
#   define WQUIETNAN_LEAST BSD43_WQUIETNAN_LEAST
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


