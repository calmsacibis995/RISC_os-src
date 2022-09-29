/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: memcpy.c,v 1.1 88/02/08 23:25:33 mdove Exp $ */

char *
memcpy(t, f, n)
	register char *t, *f;
	register n;
{
	register char *p = t;

	while (--n >= 0)
		*t++ = *f++;

	return (p);
}
