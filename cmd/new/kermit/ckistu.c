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
#ident	"$Header: ckistu.c,v 1.1.1.2 90/05/09 17:48:56 wje Exp $"
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* |_o_o|\\ Copyright (c) 1986 The Software Distillery.  All Rights Reserved */
/* |. o.| || This program may not be distributed without the permission of   */
/* | .  | || the authors.                                                    */
/* | o  | ||    Dave Baker     Ed Burnette  Stan Chow    Jay Denebeim        */
/* |  . |//     Gordon Keener  Jack Rouse   John Toebes  Doug Walker         */
/* ======          BBS:(919)-471-6436      VOICE:(919)-469-4210              */ 
/*                                                                           */
/* Contributed to Columbia University for inclusion in C-Kermit.             */
/* Permission is granted to any individual or institution to use, copy, or   */
/* redistribute this software so long as it is not sold for profit, provided */
/* this copyright notice is retained.                                        */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*  C K I S T U  --  Stubs for functions not yet implemented on Amiga */
 
/*
 Author: Jack Rouse, The Software Distillery
*/

#include "ckcdeb.h"
#include <stdio.h>
#include <ctype.h>
#include "ckcker.h"
#include "ckucmd.h"

char *dialv = "Dial Command unimplemented";
struct keytab mdmtab[] = {
	"direct", 	0, 	0,	/* no modem control */
	"generic", 	1, 	0	/* use 7 wire modem control */
};

int nmdm = sizeof(mdmtab) / sizeof(struct keytab);
 
dial()
{
	printf("Sorry, DIAL command not implemented yet.\n");
	return(-2);
}

char *loginv = "Script Command unimplemented";
login()
{
	printf("Sorry, SCRIPT command not implemented yet\n");
	return(-2);
}
