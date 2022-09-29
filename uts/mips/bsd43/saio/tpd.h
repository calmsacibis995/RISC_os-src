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
/* $Header: tpd.h,v 1.6.3.2 90/05/10 04:48:20 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * tpd.h -- definitions for format of boot tape directories
 *
 * NOTES:
 *	- all binary values are 2's complement, big-endian regardless
 *	of target machine
 *	- checksum is calculated by first zeroing the td_cksum field, then
 *	2's complement summing all 32 bit words in the struct tp_dir and
 *	and then assigning the 2's complement of the cksum to td_cksum.
 *	Thus the checksum is verified by resumming the header and verifing
 *	the sum to be zero.
 *	- each tape record is TP_BLKSIZE bytes long, trailing bytes in
 *	in a tape block (after the end of the directory or an end of file)
 *	are unspecified (although they should be zero).
 *	- all files start on a tape block boundry, the start of a particular 
 *	file may be found by skipping backward to the beginning of the
 *	file containing the tape directory and then skipping forward
 *	tp_lbn records.
 */
#define	BSD43_TP_NAMESIZE	16
#define	BSD43_TP_NENTRIES	20
#define	BSD43_TP_BLKSIZE	512
#define	BSD43_TP_MAGIC	0xaced1234

#ifdef LANGUAGE_C
/*
 * tape directory entry
 */
struct bsd43_(tp_entry) {
	char		te_name[BSD43_TP_NAMESIZE];	/* file name */
	unsigned	te_lbn;		/* tp record number, 0 is tp_dir */
	unsigned	te_nbytes;	/* file byte count */
};

/*
 * boot tape directory block
 * WARNING: must not be larger than 512 bytes!
 */
struct bsd43_(tp_dir) {
	unsigned	td_magic;	/* identifying magic number */
	unsigned	td_cksum;	/* 32 bits 2's comp csum of tp_dir */
	unsigned	td_spare1;
	unsigned	td_spare2;
	unsigned	td_spare3;
	unsigned	td_spare4;
	unsigned	td_spare5;
	unsigned	td_spare6;
	struct bsd43_(tp_entry) td_entry[BSD43_TP_NENTRIES];	/* directory */
};

union tp_header {
	char	bsd43_th_block[BSD43_TP_BLKSIZE];
	struct	bsd43_(tp_dir) th_td;
};
#endif LANGUAGE_C

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define TP_BLKSIZE BSD43_TP_BLKSIZE
#   define TP_MAGIC BSD43_TP_MAGIC
#   define TP_NAMESIZE BSD43_TP_NAMESIZE
#   define TP_NENTRIES BSD43_TP_NENTRIES
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


