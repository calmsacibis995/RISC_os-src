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
#ident	"$Header: atof.c,v 1.8.2.2 90/05/10 01:28:25 wje Exp $"

#include <stdio.h>
#include <ctype.h>

#define MAXDIGITS 17

#if MIPSEL
#  define LSW 0
#  define MSW 1
static unsigned infinity[2] = { 0x00000000, 0x7ff00000 };
#endif
#if MIPSEB
#  define MSW 0
#  define LSW 1
static unsigned infinity[2] = { 0x7ff00000, 0x00000000 };
#endif

extern double _atod ();

double
atof (s)
    register char *s;
{
    register unsigned c;
    register unsigned negate, decimal_point;
    register char *d;
    register int exp;
    register double x;
    char *ssave = s;
    union {
	double d;
	unsigned w[2];
    } o;
    char digits[MAXDIGITS];

    while (c = *s++, isspace(c));

    negate = 0;
    if (c == '+') {
	c = *s++;
    }
    else if (c == '-') {
	negate = 1;
	c = *s++;
    }
    d = digits;
    decimal_point = 0;
    exp = 0;
    while (1) {
	c -= '0';
	if (c < 10) {
	    if (d == digits+MAXDIGITS) {
		/* ignore more than 17 digits, but adjust exponent */
		exp += (decimal_point ^ 1);
	    }
	    else {
		if (c == 0 && d == digits) {
		    /* ignore leading zeros */
		}
		else {
		    *d++ = c;
		}
		exp -= decimal_point;
	    }
	}
	else if (c == '.' - '0' && !decimal_point) {
	    decimal_point = 1;
	}
	else {
	    break;
	}
	c = *s++;
    }

    if (c == 'e'-'0' || c == 'E'-'0') {
	register unsigned negate_exp = 0;
	register int e = 0;
	c = *s++;
	if (c == '+' || c == ' ') {
	    c = *s++;
	}
	else if (c == '-') {
	    negate_exp = 1;
	    c = *s++;
	}
	while (c -= '0', c < 10) {
	    e = e * 10 + c;
	    c = *s++;
	}
	if (negate_exp) {
	    e = -e;
	}
	exp += e;
    }
    if (d == digits) {
	return 0.0;
    }

    if (exp < -340) {
	x = 0;
    }
    else if (exp > 308) {
	x = *(double *)infinity;
    }
    else {
	x = _atod (digits, d - digits, exp);
    }
    if (negate) {
	x = -x;
    }
    return x;
}
