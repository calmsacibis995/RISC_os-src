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
#ident	"$Header: types.c,v 1.1.1.2 90/05/09 15:39:23 wje Exp $"
/* $Log:	types.c,v $
 * Revision 1.1.1.2  90/05/09  15:39:23  wje
 * add restricted rights legend
 * 
 * Revision 1.1.1.1  89/12/10  22:21:12  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/12/10  18:43:39  jay
 * Initial revision
 * 
 * Revision 2000.7  89/03/26  14:48:48  bettina
 * 2.0G
 * 
 * Revision 1040.6  88/09/29  15:53:49  bettina
 * bootstraped 1.40
 * 
 * Revision 1.2  87/12/09  11:42:28  gb
 * added $Log keyword
 *  */
#include "manifest"

/* This module exports operations that work on the type descriptor */
/* for an object. It was necessarily added to introduce the volatile*/
/* type qualifier with some consideration for adding const later.   */


TWORD
rmtyq( t ) 
register TWORD t; {
/* Remove volatile qualifiers */
	register TWORD t1, t2;
	register int s;

	t1 = 0;
	for( t2= t & ~BTMASK, s = 0; t2; t2 = DECREF1(t2))
	   /* shift volatile bits out */
           if( !ISVOL(t2) ) t1 |= ((t2&TMASK) << (TSHIFT * s++)); 
	return( t1 | BTYPE(t) );
} /* rmtyq */

int
tyequal( t1, t2 )
register TWORD t1,t2; {
/* ignoring volatile qualifiers, compare two types for equality */
/* and return true if equal.			               */

	return( NORM(t1) == NORM(t2) );
} /* tyequal */

TWORD
btqualifier( t ) 
register TWORD t; {
/* returns volatile if basic type is volatile */
	register TWORD t1, t2;

	for( t1= t2 = t & ~BTMASK; t1; t2=t1, t1 = DECREF1(t1));
        return( t2 & VOL);
} /* isbtvol */

TWORD
setbtv( t )
register TWORD t; {
/* sets the base type of t to volatile */

	register TWORD t1;
	register int s = 0;

	for( t1= t & ~BTMASK, s = 0; t1; s++, t1 = DECREF1(t1));
	return( (VOL<< (TSHIFT*s)) | t );
} /* setbtv */
