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
/* $Header: trace.h,v 1.2.2.2 90/05/09 18:08:53 wje Exp $ */
/* use tracef() as tracef((tr, <format>, <format-args> ... )); */
#ifdef DEBUG
	extern int	tron;
	extern char	tr[];
#	define trace(step) strace(__FILE__, __LINE__, step)
#	define tracef(step) {sprintf step; trace(tr);}
#else
#	define trace(step)
#	define tracef(step)
#endif
