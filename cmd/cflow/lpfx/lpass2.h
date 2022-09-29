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
/* $Header: lpass2.h,v 1.5.2.3 90/05/09 15:21:00 wje Exp $ */
typedef struct sty STYPE;
struct sty { ATYPE t; STYPE *next; };

typedef struct sym {
#ifdef FLEXNAMES
	char *name;
#else
	char name[LCHNM];
#endif
	char nargs;
	int decflag;
	int fline;
	STYPE symty;
	int fno;
	int mno;
	int use;
	short more;
	} STAB;
