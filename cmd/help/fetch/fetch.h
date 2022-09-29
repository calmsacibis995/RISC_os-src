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
/* $Header: fetch.h,v 1.5.2.2 90/05/09 16:10:38 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*		COMMON ARCHIVE FORMAT


	ARCHIVE File Organization:

	_______________________________________________
	|__________ARCHIVE_HEADER_DATA________________|
	|					      |
	|	Archive Header	"ar_hdr"	      |
	|.............................................|
	|					      |
	|	Symbol Directory "ar_sym"	      |
	|					      |
	|_____________________________________________|
	|________ARCHIVE_FILE_MEMBER_1________________|
	|					      |
	|	Archive File Header "arf_hdr"	      |
	|.............................................|
	|					      |
	|	Member Contents (either a.out.h       |
	|			 format or text file) |
	|_____________________________________________|
	|					      |
	|	.		.		.     |
	|	.		.		.     |
	|	.		.		.     |
	|_____________________________________________|
	|________ARCHIVE_FILE_MEMBER_n________________|
	|					      |
	|	Archive File Header "arf_hdr"	      |
	|.............................................|
	|					      |
	|	Member Contents (either a.out.h       |
	|			 format or text file) |
	|_____________________________________________|              */


#define	ARMAG	"<ar>"
#define	SARMAG	4


struct	ar_hdr {			/* archive header */
	char	ar_magic[SARMAG];	/* magic number */
	char	ar_name[16];		/* archive name */
	char	ar_date[4];		/* date of last archive modification */
	char	ar_syms[4];		/* number of ar_sym entries */
};

struct	ar_sym {			/* archive symbol table entry */
	char	sym_name[8];		/* symbol name, recognized by ld */
	char	sym_ptr[4];		/* archive position of symbol */
};

struct	arf_hdr {			/* archive file member header */
	char	arf_name[16];		/* file member name */
	char	arf_date[4];		/* file member date */
	char	arf_uid[4];		/* file member user identification */
	char	arf_gid[4];		/* file member group identification */
	char	arf_mode[4];		/* file member mode */
	char	arf_size[4];		/* file member size */
};
