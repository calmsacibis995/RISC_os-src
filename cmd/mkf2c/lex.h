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
/* $Header: lex.h,v 1.3.2.2 90/05/09 16:49:08 wje Exp $ */



#define ENDFILE 0xFFFF8000
#define SP 0x200
#define IGNORE	0x100
#define ALPH 1
#define NL	(SP | IGNORE)
#define BC (SP | 9)
#define BC2 (SP | INDIRECT)
#define CPP (0x1 | IGNORE)
