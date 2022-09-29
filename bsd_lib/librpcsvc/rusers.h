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
/* $Header: rusers.h,v 1.2.1.2 90/05/09 14:47:17 wje Exp $ */

/*	@(#)rusers.h 1.1 86/09/25 SMI */

/* 
 * Copyright (c) 1984 by Sun Microsystems, Inc.
 */

#define RUSERSPROC_NUM 1
#define RUSERSPROC_NAMES 2
#define RUSERSPROC_ALLNAMES 3
#define RUSERSPROG 100002
#define RUSERSVERS_ORIG 1
#define RUSERSVERS_IDLE 2
#define RUSERSVERS 2

#define MAXUSERS 100

struct utmparr {
	struct utmp **uta_arr;
	int uta_cnt
};

struct utmpidle {
	struct utmp ui_utmp;
	unsigned ui_idle;
};

struct utmpidlearr {
	struct utmpidle **uia_arr;
	int uia_cnt
};

int xdr_utmparr();
int xdr_utmpidlearr();
