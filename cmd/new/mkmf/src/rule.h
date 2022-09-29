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
/* $Header: rule.h,v 1.1.1.2 90/05/09 18:03:15 wje Exp $ */

/*
 * Rule definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Rule table block struct
 */
typedef struct _ruleblk
	{
	char *r_rule;			/* pointer to rule string */
	struct _ruleblk *r_next;	/* ptr to next rule list block */
	} RULEBLK;
