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
#ident	"$Header: des_crypt.c,v 1.3.1.2 90/05/07 20:27:46 wje Exp $"
/*
 * @(#)des_crypt.c 1.4 88/07/26 4.0NFSSRC SMI
 * @(#)des_crypt.c  1.13 88/02/08 1986 Sun Micro Systems
 *
 * des_crypt.c, DES encryption library routines
 */

#ifdef KERNEL
#include "sys/types.h"
#include "../des/des_crypt.h"
#include "des.h"
#else
#include <sys/types.h>
#include <des_crypt.h>
#include <des/des.h>
#endif

/*
 * To see if chip is installed 
 */
#define UNOPENED (-2)
static int g_desfd = UNOPENED;


/*
 * Copy 8 bytes
 */
#define COPY8(src, dst) { \
	register char *a = (char *) dst; \
	register char *b = (char *) src; \
	*a++ = *b++; *a++ = *b++; *a++ = *b++; *a++ = *b++; \
	*a++ = *b++; *a++ = *b++; *a++ = *b++; *a++ = *b++; \
}
 
/*
 * Copy multiple of 8 bytes
 */
#define DESCOPY(src, dst, len) { \
	register char *a = (char *) dst; \
	register char *b = (char *) src; \
	register int i; \
	for (i = (int) len; i > 0; i -= 8) { \
		*a++ = *b++; *a++ = *b++; *a++ = *b++; *a++ = *b++; \
		*a++ = *b++; *a++ = *b++; *a++ = *b++; *a++ = *b++; \
	} \
}

/*
 * CBC mode encryption
 */
cbc_crypt(key, buf, len, mode, ivec)
	char *key;
	char *buf;
	unsigned len;
	unsigned mode;
	char *ivec;	
{
	int err;
	struct desparams dp;

	dp.des_mode = CBC;
	COPY8(ivec, dp.des_ivec);
	err = common_crypt(key, buf, len, mode, &dp);
	COPY8(dp.des_ivec, ivec);
	return(err);
}


/*
 * ECB mode encryption
 */
ecb_crypt(key, buf, len, mode)
	char *key;
	char *buf;
	unsigned len;
	unsigned mode;
{
	struct desparams dp;

	dp.des_mode = ECB;	
	return(common_crypt(key, buf, len, mode, &dp));
}



/*
 * Common code to cbc_crypt() & ecb_crypt()
 */
common_crypt(key, buf, len, mode, desp)	
	char *key;	
	char *buf;
	register unsigned len;
	unsigned mode;
	register struct desparams *desp;
{
	register int desdev;
	register int res;

	if ((len % 8) != 0 || len > DES_MAXDATA) {
		return(DESERR_BADPARAM);
	}
	desp->des_dir =
		((mode & DES_DIRMASK) == DES_ENCRYPT) ? ENCRYPT : DECRYPT;

	desdev = mode & DES_DEVMASK;
	COPY8(key, desp->des_key);
	/* 
	 * software
	 */
	if (!_des_crypt(buf, len, desp)) {
		return (DESERR_HWERROR);
	}
	return(desdev == DES_SW ? DESERR_NONE : DESERR_NOHWDEVICE);
}
