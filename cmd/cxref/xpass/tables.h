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
/* $Header: tables.h,v 1.1.1.2 90/05/09 15:39:07 wje Exp $ */
/* $Log:	tables.h,v $
 * Revision 1.1.1.2  90/05/09  15:39:07  wje
 * add restricted rights legend
 * 
 * Revision 1.1.1.1  89/12/10  22:20:49  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/12/10  18:43:25  jay
 * Initial revision
 * 
 * Revision 2000.7  89/03/26  14:48:52  bettina
 * 2.0G
 * 
 * Revision 1040.6  88/09/29  15:53:54  bettina
 * bootstraped 1.40
 * 
 * Revision 1.2  87/12/09  11:42:23  gb
 * added $Log keyword
 *  */



# define NTSTRBUF	40
# define TSTRSZ		4096
# define DELAYS		20
# define MAXHASH	20
# define STRBUFLEN 	2048
# define INSTACKLEN     10
# define LINE_LENGTH    515

extern int strtabmax;
extern int itstrbufmax;
extern int tstrbufmax;
extern int stabmax;
extern int bnestmax;
extern int dimtabmax;
extern int paramstkmax;
extern int swtabmax;
extern int nodemax;
extern int asavbcmax;
extern int udeltreesmax;
extern int htabmax;
extern int file_name_length_max;
extern int strbufmax;
extern int instackmax;
extern int bufrmax;
extern int filestackmax;

extern char *itstrbuf;
extern char **tstrbuf;






