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
/* $Header: strselect.h,v 1.7.3.2 90/05/10 01:04:32 wje Exp $ */

#ifndef	_STRSELECT_
#define	_STRSELECT_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	defines for the STRSELECT construct
	for selecting among character strings
	Typical use:

		char	string[100];

		gets( string );
		STRSELECT( string )
		WHEN2( "y", "yes" )	/* only accept "y" or "yes" *
			yescode();
		WHENN( "n" )		/* anything beginning "n" is no *
			nocode();
		DEFAULT
			tryagain();
		ENDSEL
	Note:
		No {}s are used.
		No : after WHEN().
		WHENs do not fall through.
		WHENs are evaluated in order.
		DEFAULT should be last, if present.
*/

/*	Hide characters from the preprocessor so it won't rescan them.
	This allows us to contruct comment delimiters, e.g. "/*", that are
	not stripped by the preprocessor and hence get through to the compiler.
	STRSELECT depends on this working.
*/
#define	WoRD(x)x


#define	STRSELECT(a)	{  char *STRSeLeCT; STRSeLeCT = a;  WoRD(/)WoRD(*)

#define	WHEN(a)	} else WoRD(/)WoRD(*) WoRD(*)WoRD(/) \
		if( !strcmp(STRSeLeCT, a) ) {

#define	WHEN2(a1,a2)	} else WoRD(/)WoRD(*) WoRD(*)WoRD(/)\
		if( !strcmp(STRSeLeCT, a1)  ||  !strcmp(STRSeLeCT, a2) ) {

#define	WHEN3(a1,a2,a3)	} else WoRD(/)WoRD(*) WoRD(*)WoRD(/)\
		if( !strcmp(STRSeLeCT, a1)  ||  !strcmp(STRSeLeCT, a2)  ||\
		!strcmp(STRSeLeCT, a3) ) {

#define	WHENN(a)	} else WoRD(/)WoRD(*) WoRD(*)WoRD(/)\
		if( !strncmp(STRSeLeCT, a, sizeof(a)-1) ) {

#define	WHENN2(a1,a2)	} else WoRD(/)WoRD(*) WoRD(*)WoRD(/)\
		if( !strncmp(STRSeLeCT, a1, sizeof(a1)-1)  ||\
		!strncmp(STRSeLeCT, a2, sizeof(a2)-1) ) {

#define	WHENN3(a1,a2,a3)	} else WoRD(/)WoRD(*) WoRD(*)WoRD(/)\
		if( !strncmp(STRSeLeCT, a1, sizeof(a1)-1)  ||\
		!strncmp(STRSeLeCT, a2, sizeof(a2)-1)  ||\
		!strncmp(STRSeLeCT, a3, sizeof(a3)-1) ) {

#define	DEFAULT	} else WoRD(/)WoRD(*) WoRD(*)WoRD(/) {

#define	ENDSEL	}  WoRD(/)WoRD(*) WoRD(*)WoRD(/) }

#endif	_STRSELECT_
