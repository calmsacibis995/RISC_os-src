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
#ident	"$Header: bgets.c,v 1.5.2.2 90/05/10 02:34:09 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	read no more that <count> characters into <buf> from stream <fp>,
	stoping at any character slisted in <stopstr>.
	stopstr == 0 uses previous value of stopstr.
*/

#include <stdio.h>

#define CHARS	256

static unsigned char	stop[CHARS];

unsigned char *
bgets( buf, count, fp, stopstr )
unsigned char	*buf;
register
  int	count;
FILE	*fp;
unsigned char	*stopstr;
{
	register unsigned char	*cp;
	register int	c;

	if( stopstr ) {
		/* clear and set stopstr array */
		for( cp = stop;  cp < &stop[CHARS]; )
			*cp++ = 0;
		for( cp = stopstr;  *cp; )
			stop[ *cp++ ] = 1;
	}
	for( cp = buf;  ; ) {
		if( --count < 0 ) {
			*cp = '\0';
			break;
		}
		if( (c = getc(fp)) == EOF ) {
			*cp = '\0';
			if( cp == buf ) {
				cp = (unsigned char *) EOF;
			}
			break;
		}
		*cp++ = c;
		if( stop[ c ] ) {
			*cp = '\0';
			break;
		}
	}

	return  cp;
}
