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
#ident	"$Header: gcvt.c,v 1.7.2.2 90/05/10 01:31:53 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * gcvt  - Floating output conversion to
 *
 * pleasant-looking string.
 */
#if defined(mips) && !defined(MOXIE)
#include "fp_class.h"
#endif

#include "shlib.h"
extern char *ecvt();

char *
gcvt(number, ndigit, buf)
double	number;
int	ndigit;
char	*buf;
{
	int sign, decpt;
	register char *p1, *p2 = buf;
	register int i;

#if defined(mips) && !defined(MOXIE)
	int fp_class;

	fp_class = fp_class_d(number);
	if (fp_class == FP_QNAN || fp_class == FP_SNAN) {
		strcpy(buf, "NaN");
		return(buf);
	}
	if (fp_class == FP_POS_INF) {
		strcpy(buf, "Infinity");
		return(buf);
	}
	if (fp_class == FP_NEG_INF) {
		strcpy(buf, "-Infinity");
		return(buf);
	}
#endif

	p1 = ecvt(number, ndigit, &decpt, &sign);
	if (sign)
		*p2++ = '-';
	if (decpt > ndigit || decpt <= -4) {	/* E-style */
		decpt--;
		*p2++ = *p1++;
		*p2++ = '.';
		for (i = 1; i < ndigit; i++)
			*p2++ = *p1++;
		while (p2[-1] == '0')
			p2--;
		if (p2[-1] == '.')
			p2--;
		*p2++ = 'e';
		if (decpt < 0) {
			decpt = -decpt;
			*p2++ = '-';
		} else
			*p2++ = '+';
		for (i = 1000; i != 0; i /= 10) /* 3B or CRAY, for example */
			if (i <= decpt || i <= 10) /* force 2 digits */
				*p2++ = (decpt / i) % 10 + '0';
	} else {
		if (decpt <= 0) {
			*p2++ = '0';
			*p2++ = '.';
			while (decpt < 0) {
				decpt++;
				*p2++ = '0';
			}
		}
		for (i = 1; i <= ndigit; i++) {
			*p2++ = *p1++;
			if (i == decpt)
				*p2++ = '.';
		}
		if (ndigit < decpt) {
			while (ndigit++ < decpt)
				*p2++ = '0';
			*p2++ = '.';
		}
		while (*--p2 == '0' && p2 > buf)
			;
		if (*p2 != '.')
			p2++;
	}
	*p2 = '\0';
	return (buf);
}
