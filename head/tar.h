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
/* $Header: tar.h,v 1.1.1.2 90/05/10 01:05:41 wje Exp $ */

/*
 * This file specifies the symbolic constants used by tar.
 */

#define TMAGIC		"ustar"
#define TMAGLEN		6
#define TVERSION	"00" 	/* 00 and no null */
#define TVERSLEN	2

/* Values used in typeflag field */
#define REGTYPE		'0'		/* Regular File */
#define AREGTYPE	'\0'		/* Regular File */
#define LNKTYPE		'1'		/* Link */
#define SYMTYPE		'2'		/* Symbolic Link */
#define CHRTYPE		'3'		/* Character Special File */
#define BLKTYPE		'4'		/* Block Special File */
#define DIRTYPE		'5'		/* Directory */
#define FIFOTYPE	'6'		/* FIFO Special File */
#define CONTTYPE	'7'		/* Reserved */

/* Bits used in the mode field - values in octal */
#define	TSUID	04000	/* Set UID on execution */
#define	TSGID	02000	/* Set GID on execution */
#define	TSVTX	01000	/* Set "sticky" bit on execution */
			/* File permissions */
#define	TUREAD	00400	/* read by owner */
#define	TUWRITE	00200	/* write by owner */
#define	TUEXEC	00100	/* execute/search by owner */
#define	TGREAD	00040	/* read by group */
#define	TGWRITE	00020	/* write by group */
#define	TGEXEC	00010	/* execute/search by group */
#define	TOREAD	00004	/* read by others */
#define	TOWRITE	00002	/* write by others */
#define	TOEXEC	00001	/* execute/search by others */
