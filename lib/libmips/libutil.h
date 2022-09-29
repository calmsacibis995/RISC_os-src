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
/* $Header: libutil.h,v 1.1.2.2 90/05/10 02:51:07 wje Exp $ */

#define MAX_KERNEL_ID_LEN 256

/* Error return values from check_kernel_id procedure */

#define LIB_SUCCESS 0
#define LIB_ERR_BADOFFSET 1   /* Bad offset value for symbol */
#define LIB_ERR_SHORTREAD 2   /* Short memory read error */
#define LIB_ERR_KFILEOPEN 3	  /* Can't open kernel text file */
#define LIB_ERR_KLSEEK    4   /* Failed on kernel lseek operation */
#define LIB_ERR_KTEXTREAD 5   /* Failed reading kernel text segment */
#define LIB_ERR_KNODATA   6	  /* Cannot locate data in kernel text file */
#define LIB_ERR_NOMATCH   7	  /* Kernel id's do NOT match */

