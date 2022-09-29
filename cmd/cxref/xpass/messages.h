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
/* $Header: messages.h,v 1.5.2.3 90/05/09 15:37:37 wje Exp $ */
/* $Log:	messages.h,v $
 * Revision 1.5.2.3  90/05/09  15:37:37  wje
 * add restricted rights legend
 * 
 * Revision 1.5.2.2  89/12/10  22:25:50  wje
 * add RID 1.6 to branch RISCOS_4_50_FIXES
 * 
 * Revision 1.6  89/12/10  18:42:13  jay
 * replaced with files from 2.0 C compiler's lint, modified to support cxref.
 * closes bugs 1373
 * 
 * Revision 2000.7  89/03/26  14:48:14  bettina
 * 2.0G
 * 
 * Revision 1040.6  88/09/29  15:53:02  bettina
 * bootstraped 1.40
 * 
 * Revision 1.2  87/12/09  11:41:48  gb
 * added $Log keyword
 *  */
/* ref.		date			description			*/
/* !01		16may85			introduced volatile		*/
/* !02		30aug85			only supported unsigned bit fields */

#define	NUMMSGS	154

#ifndef WERROR
#    define	WERROR	werror
#endif

#ifndef UERROR
#    define	UERROR	uerror
#endif

#ifndef MESSAGE
#    define	MESSAGE(x)	msgtext[ x ]
#endif

extern char	*msgtext[ ];
