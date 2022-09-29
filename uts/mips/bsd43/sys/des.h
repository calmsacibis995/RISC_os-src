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
/* $Header: des.h,v 1.7.1.2 90/05/10 04:49:59 wje Exp $ */
/*
 * Generic DES driver interface
 * Keep this file hardware independent!
 * Copyright (c) 1986 by Sun Microsystems, Inc.
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#define BSD43_DES_MAXLEN      65536   /* maximum # of bytes to encrypt  */
#define BSD43_DES_QUICKLEN    16      /* maximum # of bytes to encrypt quickly */

enum desdir { ENCRYPT, DECRYPT };
enum desmode { CBC, ECB };

/*
 * parameters to ioctl call
 */
struct bsd43_(desparams) {
	u_char des_key[8];      /* key (with low bit parity) */
	enum desdir des_dir;    /* direction */
	enum desmode des_mode;  /* mode */
	u_char des_ivec[8];     /* input vector */
	unsigned des_len;       /* number of bytes to crypt */
	union {
		u_char UDES_data[BSD43_DES_QUICKLEN];
		u_char *UDES_buf;
	} UDES;
#       define bsd43_des_data UDES.UDES_data  /* direct data here if quick */
#       define bsd43_des_buf  UDES.UDES_buf   /* otherwise, pointer to data */
};

/*
 * Encrypt an arbitrary sized buffer
 */
#define BSD43_DESIOCBLOCK     BSD43__IOWR(d, 6, struct bsd43_(desparams))

/* 
 * Encrypt of small amount of data, quickly
 */
#define BSD43_DESIOCQUICK     BSD43__IOWR(d, 7, struct bsd43_(desparams)) 


/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DESIOCBLOCK BSD43_DESIOCBLOCK
#   define DESIOCQUICK BSD43_DESIOCQUICK
#   define DES_MAXLEN BSD43_DES_MAXLEN
#   define DES_QUICKLEN BSD43_DES_QUICKLEN
#   define des_buf bsd43_des_buf
#   define des_data bsd43_des_data
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


