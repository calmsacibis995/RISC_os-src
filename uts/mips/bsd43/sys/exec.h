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
/* $Header: exec.h,v 1.6.3.2 90/05/10 04:50:51 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)exec.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifdef mips

struct bsd43_(filehdr) {
	unsigned short	f_magic;	/* magic number */
	unsigned short	f_nscns;	/* number of sections */
	long		f_timdat;	/* time & date stamp */
	long		f_symptr;	/* file pointer to symbolic header */
	long		bsd43_(f_nsyms);	/* sizeof(symbolic hdr) */
	unsigned short	bsd43_(f_opthdr);	/* sizeof(optional hdr) */
	unsigned short	f_flags;	/* flags */
};

struct bsd43_(aouthdr) {
	short	magic;		/* see magic.h				*/
	short	vstamp;		/* version stamp			*/
	long	tsize;		/* text size in bytes, padded to FW
				   bdry					*/
	long	dsize;		/* initialized data "  "		*/
	long	bsize;		/* uninitialized data "   "		*/
	long	entry;		/* entry pt.				*/
	long	text_start;	/* base of text used for this file	*/
	long	data_start;	/* base of data used for this file	*/
	long	bss_start;	/* base of bss used for this file	*/
	long	gprmask;	/* general purpose register mask	*/
	long	cprmask[4];	/* co-processor register masks		*/
	long	gp_value;	/* the gp value used for this object    */
};

struct bsd43_(scnhdr) {
	char		s_name[8];	/* section name */
	long		s_paddr;	/* physical address */
	long		s_vaddr;	/* virtual address */
	long		s_size;		/* section size */
	long		s_scnptr;	/* file ptr to raw data for section */
	long		s_relptr;	/* file ptr to relocation */
	long		s_lnnoptr;	/* file ptr to line numbers (not used)*/
	unsigned short	s_nreloc;	/* number of relocation entries */
	unsigned short	s_nlnno;	/* number of line numbers (not used) */
	long		s_flags;	/* flags */
};

struct bsd43_(exec) {
	struct bsd43_(filehdr)	ex_f;
	struct bsd43_(aouthdr)	ex_o;
};

/*
 * Coff files produced by the mips loader are guaranteed to have the raw data
 * for the sections follow the headers in this order: .text, .rdata, .data and
 * .sdata the sum of the sizes of last three is the value in dsize in the
 * optional header.  This is all done for the benefit of the programs that
 * have to load these objects so only the file header and optional header
 * have to be inspected.  The macro N_TXTOFF() takes pointers to file header
 * and optional header and returns the file offset to the start of the raw
 * data for the .text section.  The raw data for the three data sections
 * follows the start of the .text section by the value of tsize in the optional
 * header.
 */
#define	BSD43_FILHDR	struct bsd43_(filehdr)
#define	BSD43_FILHSZ	sizeof(BSD43_FILHDR)
#define	BSD43_AOUTHSZ	sizeof(struct bsd43_(aouthdr))
#define	BSD43_SCNHSZ	sizeof(struct bsd43_(scnhdr))
/* SCNROUND is the size that sections are rounded off to */
#define BSD43_SCNROUND ((long)16)

#define BSD43_N_TXTOFF(f, a) \
 ((a).magic == BSD43_ZMAGIC ? 0 : \
  ((a).vstamp < 23 ? \
   ((BSD43_FILHSZ + BSD43_AOUTHSZ + (f).f_nscns * BSD43_SCNHSZ + 7) & 0xfffffff8) : \
   ((BSD43_FILHSZ + BSD43_AOUTHSZ + (f).f_nscns * BSD43_SCNHSZ + BSD43_SCNROUND-1) & ~(BSD43_SCNROUND-1)) ) )

/*
 * for vax compatibility
 */
#define bsd43_a_data	ex_o.dsize
#define bsd43_a_text	ex_o.tsize
#define bsd43_a_bss	ex_o.bsize
#define bsd43_a_entry	ex_o.entry
#define bsd43_a_magic	ex_o.magic

#ifdef LANGUAGE_C
#define  BSD43_MIPSEBMAGIC	0x0160
#define  BSD43_MIPSELMAGIC	0x0162
#endif LANGUAGE_C
#ifdef LANGUAGE_PASCAL
#define  BSD43_MIPSEBMAGIC	16#0160
#define  BSD43_MIPSELMAGIC	16#0162
#endif LANGUAGE_PASCAL

#ifdef MIPSEL
#define BSD43_OBJMAGIC	BSD43_MIPSELMAGIC
#endif
#ifdef MIPSEB
#define BSD43_OBJMAGIC	BSD43_MIPSEBMAGIC
#endif

#endif mips

#ifdef vax
/*
 * Header prepended to each a.out file.
 */
struct bsd43_(exec) {
	long	bsd43_a_magic;	/* magic number */
unsigned long	bsd43_a_text;		/* size of text segment */
unsigned long	bsd43_a_data;		/* size of initialized data */
unsigned long	bsd43_a_bss;		/* size of uninitialized data */
unsigned long	a_syms;		/* size of symbol table */
unsigned long	bsd43_a_entry;	/* entry point */
unsigned long	a_trsize;	/* size of text relocation */
unsigned long	a_drsize;	/* size of data relocation */
};
#endif vax

#define	BSD43_OMAGIC	0407		/* old impure format */
#define	BSD43_NMAGIC	0410		/* read-only text */
#define	BSD43_ZMAGIC	0413		/* demand load format */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define AOUTHSZ BSD43_AOUTHSZ
#   define FILHDR BSD43_FILHDR
#   define FILHSZ BSD43_FILHSZ
#   define MIPSEBMAGIC BSD43_MIPSEBMAGIC
#   define MIPSELMAGIC BSD43_MIPSELMAGIC
#   define NMAGIC BSD43_NMAGIC
#   define N_TXTOFF BSD43_N_TXTOFF
#   define OBJMAGIC BSD43_OBJMAGIC
#   define OMAGIC BSD43_OMAGIC
#   define SCNHSZ BSD43_SCNHSZ
#   define SCNROUND BSD43_SCNROUND
#   define ZMAGIC BSD43_ZMAGIC
#   define a_bss bsd43_a_bss
#   define a_data bsd43_a_data
#   define a_entry bsd43_a_entry
#   define a_magic bsd43_a_magic
#   define a_text bsd43_a_text
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


