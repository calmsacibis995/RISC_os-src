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
#ident	"$Header: gcvt.c,v 1.1.2.2 90/05/07 20:36:31 wje Exp $"

/*
 * gcvt  - Floating output conversion to
 * minimal length string
 */
#if defined(mips) && !defined(MOXIE)
#include <fp_class.h>
#endif

char	*ecvt();

char *
gcvt(number, ndigit, buf)
double number;
char *buf;
{
	int sign, decpt;
	register char *p1, *p2;
	register i;

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
	p2 = buf;
	if (sign)
		*p2++ = '-';
	for (i=ndigit-1; i>0 && p1[i]=='0'; i--)
		ndigit--;
	if (decpt >= 0 && decpt-ndigit > 4
	 || decpt < 0 && decpt < -3) { /* use E-style */
		decpt--;
		*p2++ = *p1++;
		*p2++ = '.';
		for (i=1; i<ndigit; i++)
			*p2++ = *p1++;
		*p2++ = 'e';
		if (decpt<0) {
			decpt = -decpt;
			*p2++ = '-';
		} else
			*p2++ = '+';
		if(decpt > 99){
			*p2++ = decpt/100 + '0';
			*p2++ = (decpt - (decpt/100 *100))/10 + '0';
			*p2++ = (decpt - (decpt/10 *10)) + '0';
		} else {
			*p2++ = decpt/10 + '0';
			*p2++ = decpt%10 + '0';
		}
	} else {
		if (decpt<=0) {
			if (*p1!='0')
				*p2++ = '.';
			while (decpt<0) {
				decpt++;
				*p2++ = '0';
			}
		}
		for (i=1; i<=ndigit; i++) {
			*p2++ = *p1++;
			if (i==decpt)
				*p2++ = '.';
		}
		if (ndigit<decpt) {
			while (ndigit++<decpt)
				*p2++ = '0';
			*p2++ = '.';
		}
	}
	if (p2[-1]=='.')
		p2--;
	*p2 = '\0';
	return(buf);
}
