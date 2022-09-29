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
#ident	"$Header: ovbcopy.c,v 1.1.1.3 90/05/07 20:39:46 wje Exp $"

/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include <sys/types.h>

/*
 * ovbcopy
 */
ovbcopy(src, dst, length)
	register char *src, *dst;
	register int length;
{
        register long *lsrc, *ldst;

	if (length && src != dst)
		if ((u_int)dst < (u_int)src)
			if (((int)src | (int)dst | length) & 3)
				do	/* copy by bytes */
					*dst++ = *src++;
				while (--length);
			else {
			        lsrc = (long *)src;  ldst = (long *)dst;
				length >>= 2;
				do	/* copy by longs */
					*ldst++ = *lsrc++;
	/* get around cc bug		*((long *)dst)++ = *((long *)src)++; */
				while (--length);
			}
		else {			/* copy backwards */
			src += length;
			dst += length;
			if (((int)src | (int)dst | length) & 3)
				do	/* copy by bytes */
					*--dst = *--src;
				while (--length);
			else {
			        lsrc = (long *)src;  ldst = (long *)dst;
				length >>= 2;
				do	/* copy by longs */
					*--ldst = *--lsrc;
	/* get around cc bug		*--((long *)dst) = *--((long *)src); */
				while (--length);
			}
		}
	return(0);
}
