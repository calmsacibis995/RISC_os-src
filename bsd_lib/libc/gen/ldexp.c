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
#ident	"$Header: ldexp.c,v 1.1.2.2 90/05/07 20:39:12 wje Exp $"
/*LINTLIBRARY*/
/*
 *	double ldexp (value, exp)
 *		double value;
 *		int exp;
 *
 *	Ldexp returns value * 2**exp, if that result is in range.
 *	If underflow occurs, it returns zero.  If overflow occurs,
 *	it returns a value of appropriate sign and largest single-
 *	precision magnitude.
 */

#include <values.h>
/* Largest signed long int power of 2 */
#define MAXSHIFT	(BITSPERBYTE * sizeof(long) - 2)

extern double frexp();

double
ldexp(value, exp)
register double value;
register int exp;
{
	int old_exp;

	if (exp == 0 || value == 0.0) /* nothing to do for zero */
		return (value);
#if	!(pdp11 || u3b5)	/* pdp11 "cc" can't handle cast of
				   double to void on pdp11 or 3b5 */
	(void)
#endif
	frexp(value, &old_exp);
	if (exp > 0) {
		if (exp + old_exp > MAXBEXP) { /* overflow */
			return (value < 0 ? -MAXFLOAT : MAXFLOAT);
		}
		for ( ; exp > MAXSHIFT; exp -= MAXSHIFT)
			value *= (1L << MAXSHIFT);
		return (value * (1L << exp));
	}
	if (exp + old_exp < MINBEXP) { /* underflow */
		return (0.0);
	}
	for ( ; exp < -MAXSHIFT; exp += MAXSHIFT)
		value *= 1.0/(1L << MAXSHIFT); /* mult faster than div */
	return (value / (1L << -exp));
}
