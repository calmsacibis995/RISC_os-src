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
#ident	"$Header: ecvt.c,v 1.6.2.2 90/05/10 01:31:04 wje Exp $"

/* Support obsolete interfaces */

#define	NDIG 82
static char buffer[NDIG+2];

char *
ecvt (arg, ndigits, decpt, sign)
    register double arg;
    register int ndigits, *decpt, *sign;
{
    if (ndigits > 17) {
	register char *p, *e;
	*decpt = _dtoa (buffer, 17, arg, 0) + 1;
	for (p = buffer+18, e = buffer + 1 + (ndigits > NDIG ? NDIG : ndigits);
	     p != e; ) *p++ = '0';
	*p++ = '\0';
    }
    else if (ndigits <= 0) {
	*decpt = _dtoa (buffer, 1, arg, 0) + 1;
	buffer[1] = '\0';
    }
    else {
	*decpt = _dtoa (buffer, ndigits, arg, 0) + 1;
    }
    *sign = buffer[0] == '-';
    return buffer+1;
}

char *
fcvt (arg, ndigits, decpt, sign)
    register double arg;
    register int ndigits, *decpt, *sign;
{
    *decpt = _dtoa (buffer, ndigits, arg, 1) + 1;
    *sign = buffer[0] == '-';
    return buffer+1;
}
