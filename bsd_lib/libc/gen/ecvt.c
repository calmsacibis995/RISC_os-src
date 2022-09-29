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
#ident	"$Header: ecvt.c,v 1.1.2.2 90/05/07 20:35:57 wje Exp $"

/*
 *	ecvt converts to decimal
 *	the number of digits is specified by ndigit
 *	decpt is set to the position of the decimal point
 *	sign is set to 0 for positive, 1 for negative
 */

char	*cvt();

#define	NDIG	80
char*
ecvt(arg, ndigits, decpt, sign)
double arg;
int ndigits, *decpt, *sign;
{
	return(cvt(arg, ndigits, decpt, sign, 1));
}

char*
fcvt(arg, ndigits, decpt, sign)
double arg;
int ndigits, *decpt, *sign;
{
	return(cvt(arg, ndigits, decpt, sign, 0));
}

#if defined(mips) && !defined(MOXIE)
#include <fp_class.h>
#endif

static char*
cvt(arg, ndigits, decpt, sign, eflag)
double arg;
int ndigits, *decpt, *sign;
{
	register int r2;
	double fi, fj;
	register char *p, *p1;
	static char buf[NDIG];
	double modf();

#if defined(mips) && !defined(MOXIE)
	int fp_class;

	fp_class = fp_class_d(arg);
	if (fp_class == FP_QNAN || fp_class == FP_SNAN) {
		*sign = 0;
		*decpt = strlen("NaN");
		strcpy(buf, "NaN");
		return(buf);
	}
#endif

	if (ndigits<0)
		ndigits = 0;
	if (ndigits>=NDIG-1)
		ndigits = NDIG-2;
	r2 = 0;
	*sign = 0;
	p = &buf[0];
	if (arg<0) {
		*sign = 1;
		arg = -arg;
	}

#if defined(mips) && !defined(MOXIE)
	if (fp_class == FP_POS_INF || fp_class == FP_NEG_INF) {
		*decpt = strlen("Infinity");
		strcpy(buf, "Infinity");
		return(buf);
	}
#endif

	arg = modf(arg, &fi);
	p1 = &buf[NDIG];
	/*
	 * Do integer part
	 */
	if (fi != 0) {
		p1 = &buf[NDIG];
		while (fi != 0) {
			fj = modf(fi/10, &fi);
			*--p1 = (int)((fj+.03)*10) + '0';
			r2++;
		}
		while (p1 < &buf[NDIG])
			*p++ = *p1++;
	} else if (arg > 0) {
		while ((fj = arg*10) < 1) {
			arg = fj;
			r2--;
		}
	}
	p1 = &buf[ndigits];
	if (eflag==0)
		p1 += r2;
	*decpt = r2;
	if (p1 < &buf[0]) {
		buf[0] = '\0';
		return(buf);
	}
	while (p<=p1 && p<&buf[NDIG]) {
		arg *= 10;
		arg = modf(arg, &fj);
		*p++ = (int)fj + '0';
	}
	if (p1 >= &buf[NDIG]) {
		buf[NDIG-1] = '\0';
		return(buf);
	}
	p = p1;
	*p1 += 5;
	while (*p1 > '9') {
		*p1 = '0';
		if (p1>buf)
			++*--p1;
		else {
			*p1 = '1';
			(*decpt)++;
			if (eflag==0) {
				if (p>buf)
					*p = '0';
				p++;
			}
		}
	}
	*p = '\0';
	return(buf);
}
