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
/* $Header: dkipdebug.h,v 1.6.4.2 90/05/10 06:09:49 wje Exp $ */

#ifndef	_SYS_DKIPDEBUG_
#define	_SYS_DKIPDEBUG_	1



/* Copyright(C) 1986, MIPS Computer Systems */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#define	YES 1
#define	NO  0

#if DEBUG == YES
#define ASSERT(EX) if(!(EX))assfail("EX", __FILE__, __LINE__)
#else
#define ASSERT(x)
#endif

#ifdef MONITOR
#undef MONTIOR
#define MONITOR(id, w1, w2, w3, w4) monitor(id, w1, w2, w3, w4)
#else
#define MONITOR(id, w1, w2, w3, w4)
#endif

#ifndef LOCORE
/*
 * bit field descriptions for printf %r and %R formats
 */

/*
 * printf("%r %R", val, reg_descp);
 * struct reg_desc *reg_descp;
 *
 * the %r and %R formats allow formatted output of bit fields.
 * reg_descp points to an array of reg_desc structures, each element of the
 * array describes a range of bits within val.  the array should have a
 * final element with all structure elements 0.
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
struct reg_values {
	unsigned rv_value;
	char *rv_name;
};

/*
 * register descriptors are used for formatted prints of register values
 * rd_mask and rd_shift must be defined, other entries may be null
 */
struct reg_desc {
	unsigned rd_mask;	/* mask to extract field */
	int rd_shift;		/* shift for extracted value, - >>, + << */
	char *rd_name;		/* field name */
	char *rd_format;	/* format to print field */
	struct reg_values *rd_values;	/* symbolic names of values */
};
#endif !LOCORE

#endif	_SYS_DKIPDEBUG_
