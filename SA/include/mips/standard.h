#ident "$Header: standard.h,v 1.2 90/01/23 14:20:41 huang Exp $"
/* $Copyright$ */

/******************************************************************************
 *
 *		Standard definitions for avt's 
 *
 * %Q% %M% %I% 
 *
 * Author	Bromo
 * Date started Wed July 10 15:37:59 PDT 1985
 * Module	standard.h
 * Purpose	provide a set of standard constants and macros for all avts
 *
 ******************************************************************************/

/*
 *    C O N S T A N T S
 */

#define TRUE 1
#define FALSE 0

/* PROM Definitions */

#define PROM_SIZE 0x80000
#define PROM_WORDSIZE +(PROM_SIZE/4)

/* Load Addresses */

#define MAPPER_BASE 0xbfc00200
#define BOOT_BASE 0xbfc02000


/*
 *   M  A C R O S
 */

/* This macro sets all registers to a value in the range +-65K */
#define set_all_registers( value )		addiu	r1,r0,value;	\
		addiu	r2,r0,value;					\
		addiu	r3,r0,value;					\
		addiu	r4,r0,value;					\
		addiu	r5,r0,value;					\
		addiu	r6,r0,value;					\
		addiu	r7,r0,value;					\
		addiu	r8,r0,value;					\
		addiu	r9,r0,value;					\
		addiu	r10,r0,value;					\
		addiu	r11,r0,value;					\
		addiu	r12,r0,value;					\
		addiu	r13,r0,value;					\
		addiu	r14,r0,value;					\
		addiu	r15,r0,value;					\
		addiu	r16,r0,value;					\
		addiu	r17,r0,value;					\
		addiu	r18,r0,value;					\
		addiu	r19,r0,value;					\
		addiu	r20,r0,value;					\
		addiu	r21,r0,value;					\
		addiu	r22,r0,value;					\
		addiu	r23,r0,value;					\
		addiu	r24,r0,value;					\
		addiu	r25,r0,value;					\
		addiu	r26,r0,value;					\
		addiu	r27,r0,value;					\
		addiu	r28,r0,value;					\
		addiu	r29,r0,value;					\
		addiu	r30,r0,value;					\
		addiu	r31,r0,value;

