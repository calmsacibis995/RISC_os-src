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
/* $Header: dates.h,v 1.1.2.2 90/05/10 03:46:02 wje Exp $ */
#ifdef MAIN
static char rcsm[]=MAKEID;
static char rcsh[]="$Header: dates.h,v 1.1.2.2 90/05/10 03:46:02 wje Exp $";
#endif

#include <sys/param.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef BSD
#include <sys/time.h>
#else
#include "time.h"
#endif
#ifdef BSD
#include <tzfile.h>
#else
#include "tzfile.h"
#endif
#include <ctype.h>

/* neato GLOBAL declaration macros */

#ifdef MAIN
#define DCL
#else
#define DCL extern
#endif

#define GLOBAL(THING) DCL THING

