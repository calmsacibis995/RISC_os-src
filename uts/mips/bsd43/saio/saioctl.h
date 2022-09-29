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
/* $Header: saioctl.h,v 1.6.3.2 90/05/10 04:47:23 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * saioctl.h -- standalone ioctl definitions
 */

#ifndef BSD43_NULL
#define BSD43_NULL 0
#endif

#define	BSD43_EOF	(-1)			/* EOF from getc() */

/*
 * general ioctl's
 */
#define	BSD43_FIOCNBLOCK	(('f'<<8)|1)	/* set non-blocking io */
#define	BSD43_FIOCSCAN	(('f'<<8)|2)	/* scan for input */
/*
 * "tty" ioctl's
 */
#define	BSD43_TIOCRAW		(('t'<<8)|1)	/* no special chars on char devices */
#define	BSD43_TIOCFLUSH	(('t'<<8)|2)	/* flush input */
#define	BSD43_TIOCPROTO	(('t'<<8)|3)	/* control pseudo console access */
#define	BSD43_TIOCREOPEN	(('t'<<8)|4)	/* reopen to effect baud rate chg */

/*
 * network ioctl's
 */
#define	BSD43_NIOCBIND	(('n'<<8)|1)	/* bind network address */
		
/*
 * disk ioctl's
 */
#define	BSD43_DIOCGETVH	(('d'<<8)|1)	/* get volume header */
#define	BSD43_DIOCSETVH	(('d'<<8)|2)	/* set volume header */
#define	BSD43_DIOCGETCTLR	(('d'<<8)|3)	/* get controller info */
/*
 * Maybe should get rid of this and instead have an ioctl
 * that will allow a read of bad data.  If this goes, you
 * won't be able to reformat an active file system (like the root).
 */
#define	BSD43_DIOCREFMTTRK	(('d'<<8)|4)	/* atomically reformat track */
#define	BSD43_DIOCFMTMAP	(('d'<<8)|5)	/* format track or perform mapping */
#define	BSD43_DIOCVFYSEC	(('d'<<8)|6)	/* verify sectors */
#define BSD43_DIOCDIAG	(('d'<<8)|7)	/* cause ctlr to run diagnostics */
#define BSD43_DIOCNOECC	(('d'<<8)|8)	/* arg != 0 disables ECC correction */
#define BSD43_DIOCRDEFECTS	(('d'<<8)|9)	/* read media defect information */

/*
 * pseudo console ioctl's
 */
#define	BSD43_PIOCENABLE	(('p'<<8)|1)	/* enable device as console */
#define	BSD43_PIOCDISABLE	(('p'<<8)|2)	/* disable device as console */
#define	BSD43_PIOCSHOW	(('p'<<8)|3)	/* show enabled console devices */

/*
 * bit field descriptions for printf %r and %R formats
 */

/*
 * printf("%r %R", val, reg_descp);
 * struct reg_desc *reg_descp;
 *
 * the %r and %R formats allow formatted print of bit fields.  individual
 * bit fields are described by a struct reg_desc, multiple bit fields within
 * a single word can be described by multiple reg_desc structures.
 * %r outputs a string of the format "<bit field descriptions>"
 * %R outputs a string of the format "0x%x<bit field descriptions>"
 *
 * The fields in a reg_desc are:
 *	unsigned rd_mask;	An appropriate mask to isolate the bit field
 *				within a word, and'ed with val
 *
 *	int rd_shift;		A shift amount to be done to the isolated
 *				bit field.  done before printing the isolate
 *				bit field with rd_format and before searching
 *				for symbolic value names in rd_values
 *
 *	char *rd_name;		If non-null, a bit field name to label any
 *				out from rd_format or searching rd_values.
 *				if neither rd_format or rd_values is non-null
 *				rd_name is printed only if the isolated
 *				bit field is non-null.
 *
 *	char *rd_format;	If non-null, the shifted bit field value
 *				is printed using this format.
 *
 *	struct reg_values *rd_values;	If non-null, a pointer to a table
 *				matching numeric values with symbolic names.
 *				rd_values are searched and the symbolic
 *				value is printed if a match is found, if no
 *				match is found "???" is printed.
 *				
 */

/*
 * register values
 * map between numeric values and symbolic values
 */
#ifdef LANGUAGE_C
struct bsd43_(reg_values) {
	unsigned rv_value;
	char *rv_name;
};

/*
 * register descriptors are used for formatted prints of register values
 * rd_mask and rd_shift must be defined, other entries may be null
 */
struct bsd43_(reg_desc) {
	unsigned rd_mask;	/* mask to extract field */
	int rd_shift;		/* shift for extracted value, - >>, + << */
	char *rd_name;		/* field name */
	char *rd_format;	/* format to print field */
	struct bsd43_(reg_values) *rd_values;	/* symbolic names of values */
};
#endif LANGUAGE_C

#ifdef LANGUAGE_C
extern	int bsd43_(errno);	/* just like unix ??? */
#endif LANGUAGE_C

#define	BSD43_EXCEPT_NORM	1
#define	BSD43_EXCEPT_UTLB	2
#define	BSD43_EXCEPT_BRKPT	3

/*
 * register names
 */
#define	BSD43_R_R0		0
#define	BSD43_R_R1		1
#define	BSD43_R_R2		2
#define	BSD43_R_R3		3
#define	BSD43_R_R4		4
#define	BSD43_R_R5		5
#define	BSD43_R_R6		6
#define	BSD43_R_R7		7
#define	BSD43_R_R8		8
#define	BSD43_R_R9		9
#define	BSD43_R_R10		10
#define	BSD43_R_R11		11
#define	BSD43_R_R12		12
#define	BSD43_R_R13		13
#define	BSD43_R_R14		14
#define	BSD43_R_R15		15
#define	BSD43_R_R16		16
#define	BSD43_R_R17		17
#define	BSD43_R_R18		18
#define	BSD43_R_R19		19
#define	BSD43_R_R20		20
#define	BSD43_R_R21		21
#define	BSD43_R_R22		22
#define	BSD43_R_R23		23
#define	BSD43_R_R24		24
#define	BSD43_R_R25		25
#define	BSD43_R_R26		26
#define	BSD43_R_R27		27
#define	BSD43_R_R28		28
#define	BSD43_R_R29		29
#define	BSD43_R_R30		30
#define	BSD43_R_R31		31
#define	BSD43_R_F0		32
#define	BSD43_R_F1		33
#define	BSD43_R_F2		34
#define	BSD43_R_F3		35
#define	BSD43_R_F4		36
#define	BSD43_R_F5		37
#define	BSD43_R_F6		38
#define	BSD43_R_F7		39
#define	BSD43_R_F8		40
#define	BSD43_R_F9		41
#define	BSD43_R_F10		42
#define	BSD43_R_F11		43
#define	BSD43_R_F12		44
#define	BSD43_R_F13		45
#define	BSD43_R_F14		46
#define	BSD43_R_F15		47
#define	BSD43_R_F16		48
#define	BSD43_R_F17		49
#define	BSD43_R_F18		50
#define	BSD43_R_F19		51
#define	BSD43_R_F20		52
#define	BSD43_R_F21		53
#define	BSD43_R_F22		54
#define	BSD43_R_F23		55
#define	BSD43_R_F24		56
#define	BSD43_R_F25		57
#define	BSD43_R_F26		58
#define	BSD43_R_F27		59
#define	BSD43_R_F28		60
#define	BSD43_R_F29		61
#define	BSD43_R_F30		62
#define	BSD43_R_F31		63
#define	BSD43_R_EPC		64
#define	BSD43_R_MDHI		65
#define	BSD43_R_MDLO		66
#define	BSD43_R_SR		67
#define	BSD43_R_CAUSE		68
#define	BSD43_R_TLBHI		69
#define	BSD43_R_TLBLO		70
#define	BSD43_R_BADVADDR	71
#define	BSD43_R_INX		72
#define	BSD43_R_RAND		73
#define	BSD43_R_CTXT		74
#define	BSD43_R_EXCTYPE	75
#define	BSD43_R_C1_EIR	76
#define	BSD43_R_C1_SR		77
#define	BSD43_NREGS		78

/*
 * compiler defined bindings
 */
#define	BSD43_R_ZERO		BSD43_R_R0
#define	BSD43_R_AT		BSD43_R_R1
#define	BSD43_R_V0		BSD43_R_R2
#define	BSD43_R_V1		BSD43_R_R3
#define	BSD43_R_A0		BSD43_R_R4
#define	BSD43_R_A1		BSD43_R_R5
#define	BSD43_R_A2		BSD43_R_R6
#define	BSD43_R_A3		BSD43_R_R7
#define	BSD43_R_T0		BSD43_R_R8
#define	BSD43_R_T1		BSD43_R_R9
#define	BSD43_R_T2		BSD43_R_R10
#define	BSD43_R_T3		BSD43_R_R11
#define	BSD43_R_T4		BSD43_R_R12
#define	BSD43_R_T5		BSD43_R_R13
#define	BSD43_R_T6		BSD43_R_R14
#define	BSD43_R_T7		BSD43_R_R15
#define	BSD43_R_S0		BSD43_R_R16
#define	BSD43_R_S1		BSD43_R_R17
#define	BSD43_R_S2		BSD43_R_R18
#define	BSD43_R_S3		BSD43_R_R19
#define	BSD43_R_S4		BSD43_R_R20
#define	BSD43_R_S5		BSD43_R_R21
#define	BSD43_R_S6		BSD43_R_R22
#define	BSD43_R_S7		BSD43_R_R23
#define	BSD43_R_T8		BSD43_R_R24
#define	BSD43_R_T9		BSD43_R_R25
#define	BSD43_R_K0		BSD43_R_R26
#define	BSD43_R_K1		BSD43_R_R27
#define	BSD43_R_GP		BSD43_R_R28
#define	BSD43_R_SP		BSD43_R_R29
#define	BSD43_R_FP		BSD43_R_R30
#define	BSD43_R_RA		BSD43_R_R31

/*
 * Stack modes
 */
#define	BSD43_MODE_NORMAL	0	/* executing on normal stack */
#define	BSD43_MODE_FAULT	1	/* executing on fault stack */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DIOCDIAG BSD43_DIOCDIAG
#   define DIOCFMTMAP BSD43_DIOCFMTMAP
#   define DIOCGETCTLR BSD43_DIOCGETCTLR
#   define DIOCGETVH BSD43_DIOCGETVH
#   define DIOCNOECC BSD43_DIOCNOECC
#   define DIOCRDEFECTS BSD43_DIOCRDEFECTS
#   define DIOCREFMTTRK BSD43_DIOCREFMTTRK
#   define DIOCSETVH BSD43_DIOCSETVH
#   define DIOCVFYSEC BSD43_DIOCVFYSEC
#   define EOF BSD43_EOF
#   define EXCEPT_BRKPT BSD43_EXCEPT_BRKPT
#   define EXCEPT_NORM BSD43_EXCEPT_NORM
#   define EXCEPT_UTLB BSD43_EXCEPT_UTLB
#   define FIOCNBLOCK BSD43_FIOCNBLOCK
#   define FIOCSCAN BSD43_FIOCSCAN
#   define MODE_FAULT BSD43_MODE_FAULT
#   define MODE_NORMAL BSD43_MODE_NORMAL
#   define NIOCBIND BSD43_NIOCBIND
#   define NREGS BSD43_NREGS
#   define NULL BSD43_NULL
#   define PIOCDISABLE BSD43_PIOCDISABLE
#   define PIOCENABLE BSD43_PIOCENABLE
#   define PIOCSHOW BSD43_PIOCSHOW
#   define R_A0 BSD43_R_A0
#   define R_A1 BSD43_R_A1
#   define R_A2 BSD43_R_A2
#   define R_A3 BSD43_R_A3
#   define R_AT BSD43_R_AT
#   define R_BADVADDR BSD43_R_BADVADDR
#   define R_C1_EIR BSD43_R_C1_EIR
#   define R_C1_SR BSD43_R_C1_SR
#   define R_CAUSE BSD43_R_CAUSE
#   define R_CTXT BSD43_R_CTXT
#   define R_EPC BSD43_R_EPC
#   define R_EXCTYPE BSD43_R_EXCTYPE
#   define R_F0 BSD43_R_F0
#   define R_F1 BSD43_R_F1
#   define R_F10 BSD43_R_F10
#   define R_F11 BSD43_R_F11
#   define R_F12 BSD43_R_F12
#   define R_F13 BSD43_R_F13
#   define R_F14 BSD43_R_F14
#   define R_F15 BSD43_R_F15
#   define R_F16 BSD43_R_F16
#   define R_F17 BSD43_R_F17
#   define R_F18 BSD43_R_F18
#   define R_F19 BSD43_R_F19
#   define R_F2 BSD43_R_F2
#   define R_F20 BSD43_R_F20
#   define R_F21 BSD43_R_F21
#   define R_F22 BSD43_R_F22
#   define R_F23 BSD43_R_F23
#   define R_F24 BSD43_R_F24
#   define R_F25 BSD43_R_F25
#   define R_F26 BSD43_R_F26
#   define R_F27 BSD43_R_F27
#   define R_F28 BSD43_R_F28
#   define R_F29 BSD43_R_F29
#   define R_F3 BSD43_R_F3
#   define R_F30 BSD43_R_F30
#   define R_F31 BSD43_R_F31
#   define R_F4 BSD43_R_F4
#   define R_F5 BSD43_R_F5
#   define R_F6 BSD43_R_F6
#   define R_F7 BSD43_R_F7
#   define R_F8 BSD43_R_F8
#   define R_F9 BSD43_R_F9
#   define R_FP BSD43_R_FP
#   define R_GP BSD43_R_GP
#   define R_INX BSD43_R_INX
#   define R_K0 BSD43_R_K0
#   define R_K1 BSD43_R_K1
#   define R_MDHI BSD43_R_MDHI
#   define R_MDLO BSD43_R_MDLO
#   define R_R0 BSD43_R_R0
#   define R_R1 BSD43_R_R1
#   define R_R10 BSD43_R_R10
#   define R_R11 BSD43_R_R11
#   define R_R12 BSD43_R_R12
#   define R_R13 BSD43_R_R13
#   define R_R14 BSD43_R_R14
#   define R_R15 BSD43_R_R15
#   define R_R16 BSD43_R_R16
#   define R_R17 BSD43_R_R17
#   define R_R18 BSD43_R_R18
#   define R_R19 BSD43_R_R19
#   define R_R2 BSD43_R_R2
#   define R_R20 BSD43_R_R20
#   define R_R21 BSD43_R_R21
#   define R_R22 BSD43_R_R22
#   define R_R23 BSD43_R_R23
#   define R_R24 BSD43_R_R24
#   define R_R25 BSD43_R_R25
#   define R_R26 BSD43_R_R26
#   define R_R27 BSD43_R_R27
#   define R_R28 BSD43_R_R28
#   define R_R29 BSD43_R_R29
#   define R_R3 BSD43_R_R3
#   define R_R30 BSD43_R_R30
#   define R_R31 BSD43_R_R31
#   define R_R4 BSD43_R_R4
#   define R_R5 BSD43_R_R5
#   define R_R6 BSD43_R_R6
#   define R_R7 BSD43_R_R7
#   define R_R8 BSD43_R_R8
#   define R_R9 BSD43_R_R9
#   define R_RA BSD43_R_RA
#   define R_RAND BSD43_R_RAND
#   define R_S0 BSD43_R_S0
#   define R_S1 BSD43_R_S1
#   define R_S2 BSD43_R_S2
#   define R_S3 BSD43_R_S3
#   define R_S4 BSD43_R_S4
#   define R_S5 BSD43_R_S5
#   define R_S6 BSD43_R_S6
#   define R_S7 BSD43_R_S7
#   define R_SP BSD43_R_SP
#   define R_SR BSD43_R_SR
#   define R_T0 BSD43_R_T0
#   define R_T1 BSD43_R_T1
#   define R_T2 BSD43_R_T2
#   define R_T3 BSD43_R_T3
#   define R_T4 BSD43_R_T4
#   define R_T5 BSD43_R_T5
#   define R_T6 BSD43_R_T6
#   define R_T7 BSD43_R_T7
#   define R_T8 BSD43_R_T8
#   define R_T9 BSD43_R_T9
#   define R_TLBHI BSD43_R_TLBHI
#   define R_TLBLO BSD43_R_TLBLO
#   define R_V0 BSD43_R_V0
#   define R_V1 BSD43_R_V1
#   define R_ZERO BSD43_R_ZERO
#   define TIOCFLUSH BSD43_TIOCFLUSH
#   define TIOCPROTO BSD43_TIOCPROTO
#   define TIOCRAW BSD43_TIOCRAW
#   define TIOCREOPEN BSD43_TIOCREOPEN
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


